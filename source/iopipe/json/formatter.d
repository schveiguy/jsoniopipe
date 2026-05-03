module iopipe.json.formatter;

public import iopipe.json.common;

import iopipe.traits : isIopipe, WindowType;
import iopipe.bufpipe; 
import iopipe.json.serialize : jsonEscapeSubstitutions;

import std.bitmanip : BitArray;
import std.exception : enforce;
import std.traits : isNumeric;
import std.conv : to;
import std.regex;
import std.algorithm.iteration : substitute;
import std.algorithm.searching : startsWith, endsWith;
import std.format;

/**
 * Controls the whitespace placed around the colon separator between a JSON
 * object key and its value.
 */
enum ColonSpacing {
    None,  /// No spaces around the colon: `"key":"value"`
    After, /// A single space after the colon: `"key": "value"`
    Both,  /// A single space before and after the colon: `"key" : "value"`
}

/**
 * Controls how an object member name is quoted when writing JSON5 output.
 * In standard JSON, member names are always double-quoted; this enum is only
 * meaningful when the `JSON5` template parameter is `true`.
 */
enum MemberNameStyle
{
    DoubleQuote, /// Enclose the member name in double quotes: `"key"`
    SingleQuote, /// Enclose the member name in single quotes: `'key'`
    Symbol,      /// Write the member name as a bare identifier (no quotes): `key`
    Auto         /// Pick the best style: bare symbol if valid, otherwise double-quoted (or single-quoted if the name contains a double-quote)
}

/**
 * Controls how raw data passed to `addStringData` is interpreted and written
 * into the output stream.
 */
enum StringMode : ubyte
{
    PassThru,   /// Write data to the output as-is, with no escaping or validation.
    AddEscapes, /// Escape any characters that are invalid inside a JSON string (default).
    Validate    /// Verify that the data is already valid JSON string content and throw `JSONIopipeException` if not.
}


/**
 * Represents a JSON or JSON5 keyword literal that can be written directly to
 * the output stream via `addKeywordValue`. Each member's value is the exact
 * string token that will appear in the output.
 */
enum KeywordValue : string {
    Null = "null",             /// The JSON `null` literal.
    True = "true",             /// The JSON `true` literal.
    False = "false",           /// The JSON `false` literal.
    // JSON5 extensions
    Infinity = "Infinity",         /// The JSON5 `Infinity` literal.
    NegativeInfinity = "-Infinity", /// The JSON5 `-Infinity` literal.
    NaN = "NaN"                /// The JSON5 `NaN` literal.
}

/**
 * Represents the current position in the JSON output state machine.
 * Exposed via the `state` property so that formatters layered on top of
 * `JSONOutputter` can make spacing decisions without duplicating state tracking.
 */
enum State : ubyte
{
    Begin,        /// No output has been written yet; a value, object, or array may be started.
    First,        /// Inside an object or array, before the first member; a value or end-of-aggregate is expected.
    Member,       /// Inside an object or array after a comma; the next member (or end-of-aggregate in JSON5) is expected.
    Colon,        /// An object key has been written; a colon is expected next.
    Comma,        /// A value has been written; a comma or end-of-aggregate is expected.
    Value,        /// A colon has been written; the object field's value is expected next.
    String,       /// Inside a string value; `addStringData` and `endString` are valid.
    MemberString, /// Inside an object member-name string; `addStringData` and `endString` are valid.
    End           /// The top-level value is complete; no further output is allowed.
}

/**
 * Low-level JSON writer that emits tokens directly to a character iopipe with
 * no added whitespace. It enforces the structural rules of JSON (or JSON5 when
 * `JSON5` is `true`) via an internal state machine, throwing
 * `JSONIopipeException` if methods are called in an invalid order.
 *
 * Use `JSONStandardFormatter` for output with indentation and newlines, or
 * construct this type directly when compact output is required.
 *
 * Params:
 *   Chain = An iopipe whose element type is `char`.
 *   JSON5 = When `true`, enables JSON5 extensions such as single-quoted
 *           strings, bare symbol keys, trailing commas, and comment output.
 */
struct JSONOutputter(Chain, bool JSON5 = false)
if (isIopipe!Chain)
{
private:
    Chain* outputChain;
    size_t pos = 0;
    static if(JSON5)
        char[1] currentQuoteChar = [0];
    else
        enum currentQuoteChar = "\"";

    // 0 = array, 1 = object (same convention as parser)
    BitArray stack;
    size_t   stackLen;
    State    _state = State.Begin;

    void pushContainer(bool isObj)
    {
        if (stackLen == stack.length)
            stack ~= isObj;
        else
            stack[stackLen] = isObj;
        ++stackLen;
    }

    void popContainer()
    {
        assert(stackLen != 0);
        _state = (--stackLen == 0) ? State.End : State.Comma;
    }

    // Can we add a value (string, number, keyword, begin object/array)?
    bool canAddValue() const nothrow
    {
        final switch (_state) with (State)
        {
        case Begin:
        case Value:
            return true;
        case Member:
        case First:
            // only allowed for arrays; for objects we must first put out the member name
            return !inObj;
        case Colon:
        case Comma:
        case String:
        case MemberString:
        case End:
            return false;
        }
    }

    // can end object or array. For normal JSON, this is either at the
    // beginning of the aggregate, or when a comma would be expected. For
    // JSON5, we can have trailing commas, so it can also come when the next
    // member is expected.
    bool canEndAggregate() const nothrow
    {
        return stackLen > 0 &&
            (_state == State.First ||
             _state == State.Comma ||
             (JSON5 && _state == State.Member));
    }

    // set up the state for outputting a string. Returns true if in a valid
    // state for strings, false otherwise.
    bool beginStringState() nothrow
    {
        final switch (_state) with (State)
        {
        case Begin:
        case Value:
            _state = State.String;
            return true;
        case Member:
        case First:
            // for objects, starting a string here means you are writing the
            // member name. For arrays, this is a string value.
            _state = inObj ? State.MemberString : State.String;
            return true;
        case Colon:
        case Comma:
        case String:
        case MemberString:
        case End:
            return false;
        }
    }

    static if(JSON5)
    bool isValidSymbol(const(char)[] s)
    {
        // use the parser to validate, this is kinda complicated
        import iopipe.json.parser;
        size_t pos = 0;
        auto item = s.jsonItem!(ParseConfig(JSON5: true, includeComments: true, includeSpaces: true))(pos, 0);
        // if there was a valid symbol, and it was the full string, this can be used.
        return item.token == JSONToken.Symbol && pos == s.length;
    }

    void putStr(const(char)[] s)
    {
        import std.stdio;
        outputChain.ensureElems(pos + s.length);
        outputChain.window[pos .. pos + s.length] = s;
        pos += s.length;
    }

    void putEscapedStr(const(char)[] s)
    {
        // Add any escapes necessary.
        // TODO: this does not handle single quoted strings with JSON5
        import std.range : put;
        auto r = &putStr;
        const(char)[2] quoterep = ['\\', currentQuoteChar[0]];
        // TODO: see if I can write this better, I don't like the string casts, but the compiler can't handle it otherwise.
        put(r, s.substitute(jsonEscapeSubstitutions!(), cast(string)currentQuoteChar[], cast(string)quoterep[]));
    }

public:

    /**
     * Construct a `JSONOutputter` that writes into `chain` starting at `pos`.
     *
     * Params:
     *   chain = The output iopipe to write JSON into. The outputter holds a
     *           pointer to this chain, so it must outlive the outputter.
     *   pos   = Initial write offset within `chain`'s window.
     */
    this(ref Chain chain, size_t pos = 0)
    {
        this.outputChain = &chain;
        this.pos = pos;
    }

    /// The current state of the output state machine.
    State state() const @property nothrow => _state;
    /// The number of characters written into the output chain's window so far.
    size_t position() const @property nothrow => pos;
    /// A slice of the output chain's window containing all characters written so far.
    auto window() => outputChain.window[0 .. pos];
    /// `true` when the innermost open aggregate is an object.
    bool inObj() const @property nothrow => stackLen == 0 ? false : stack[stackLen - 1];
    /// The number of currently open (not yet closed) objects and arrays.
    size_t depth() const @property nothrow => stackLen;

    /**
     * Write `{` to the output and push a new object onto the aggregate stack.
     * Throws `JSONIopipeException` if the current state does not allow a value.
     */
    void beginObject()
    {
        if (!canAddValue)
            throw new JSONIopipeException(format("beginObject not allowed (state = %s)", _state));

        pushContainer(true);  // object
        _state = State.First;
        putStr("{");
    }

    /**
     * Write `[` to the output and push a new array onto the aggregate stack.
     * Throws `JSONIopipeException` if the current state does not allow a value.
     */
    void beginArray()
    {
        if (!canAddValue)
            throw new JSONIopipeException(format("beginObject not allowed (state = %s)", _state));

        pushContainer(false); // array
        _state = State.First;
        putStr("[");
    }

    /**
     * Close the innermost open object or array, writing `}` or `]` to the
     * output. May be called on an empty aggregate, after a complete value, or —
     * in JSON5 mode — after a trailing comma.
     * Throws `JSONIopipeException` if no aggregate is open or the state does
     * not allow closing.
     */
    void endAggregate()
    {
        if (!canEndAggregate)
            throw new JSONIopipeException(format("endAggregate not allowed (state = %s)", _state));

        putStr(inObj ? "}" : "]");
        popContainer();
    }

    /**
     * Write an opening `"` and transition into string-writing mode.
     * Follow with zero or more calls to `addStringData`, then `endString`.
     * Throws `JSONIopipeException` if the current state does not allow a string.
     */
    void beginString()
    {
        if (!beginStringState())
            throw new JSONIopipeException(format("beginString not allowed (state = %s)", _state));

        putStr("\"");
        static if (JSON5)
            currentQuoteChar[0] = '"';
    }

    /**
     * JSON5 only. Write an opening quote character and transition into
     * string-writing mode, using the specified quote character.
     * Follow with zero or more calls to `addStringData`, then `endString`.
     * Throws `JSONIopipeException` if the current state does not allow a string.
     *
     * Params:
     *   quoteChar = The quote character to use; must be `'` or `"`.
     */
    static if(JSON5) void beginString(char quoteChar)
    {
        assert(quoteChar == '\'' || quoteChar == '"');
        if (!beginStringState())
            throw new JSONIopipeException(format("beginString not allowed (state = %s)", _state));
        putStr((&quoteChar)[0 .. 1]);
        currentQuoteChar[0] = quoteChar;
    }

    /**
     * Write a complete, double-quoted object member name and transition to
     * expecting a colon.
     * Throws `JSONIopipeException` if not currently inside an object at a
     * position where a member name is expected.
     *
     * Params:
     *   memberName = The member name to write.
     */
    void addMemberName(const(char)[] memberName)
    {
        if (!inObj || (_state != State.First && _state != State.Member))
            throw new JSONIopipeException("cannot add member name when not inside object or at member state");
        beginString();
        addStringData(memberName);
        endString();
    }

    /**
     * JSON5 only. Write a complete object member name using the specified
     * quoting style and transition to expecting a colon.
     * Throws `JSONIopipeException` if not currently inside an object at a
     * position where a member name is expected, or if `style` is `Symbol` and
     * `memberName` is not a valid JSON5 identifier.
     *
     * Params:
     *   memberName = The member name to write.
     *   style      = How to quote the member name.
     */
    static if(JSON5)
        void addMemberName(const(char)[] memberName, MemberNameStyle style)
    {
        final switch(style) with (MemberNameStyle) {
        case SingleQuote:
            beginString('\'');
            addStringData(memberName);
            endString();
            break;
        case DoubleQuote:
            beginString('"');
            addStringData(memberName);
            endString();
            break;
        case Symbol:
            // do not do any escaping, just validate the data
            if (!isValidSymbol(memberName))
                throw new JSONIopipeException(format("Member name '%s' is not a valid symbol for JSON5", memberName));
PUT_SYMBOL:
            putStr(memberName);
            _state = State.Colon;
            break;
        case Auto:
            // if the member name can be output, just do it. Otherwise, pick a
            // good quote char.
            import std.algorithm : canFind;
            if (isValidSymbol(memberName))
                goto PUT_SYMBOL;
            else if(memberName.canFind('\"'))
                goto case SingleQuote;
            goto case DoubleQuote;
        }
    }

    /**
     * Write data into the currently open string according to `mode`.
     * May be called multiple times between `beginString` and `endString`.
     * Throws `JSONIopipeException` if not currently inside a string, or if
     * `mode` is `StringMode.Validate` and `data` contains invalid JSON string
     * content. Note that escape sequences must not be split across calls when
     * using `StringMode.Validate`.
     *
     * Params:
     *   mode = Controls how `data` is interpreted; see `StringMode`.
     *   data = The content to append, converted to string data.
     */
    void addStringData(StringMode mode = StringMode.AddEscapes, T)(T data)
    {
        if (_state != State.String && _state != State.MemberString)
            throw new JSONIopipeException("cannot add string data outside of beginString/endString");
        static if (mode == StringMode.AddEscapes)
        {
            // write the string data, escaping any characters that should be escaped.
            formattedWrite(&putEscapedStr, "%s", data);
        }
        else
        { 
            static if (mode == StringMode.Validate)
            {
                // Write the string as-is, including a termination quote character.
                // Then we validate the resulting data, and trim off the quote character.
                size_t origLen = pos;
                formattedWrite(&putStr, "%s%c", data, currentQuoteChar[0]);
                import iopipe.json.parser;
                size_t curPos = 0;
                JSONParseHint hint;
                auto win = outputChain.window[origLen .. pos];
                auto len = validateString!(false, JSON5)(currentQuoteChar[0], win, curPos, hint);
                // len will not include the final quote character
                if (len + 1 != win.length)
                {
                    pos = origLen;
                    throw new JSONIopipeException("Invalid data in string");
                }
                // we wrote the final quote character to validate the string data, ignore it.
                --pos;
            }
            else
            {
                // pass thru, assume data is correct
                formattedWrite(&putStr, "%s", data);
            }
        }
    }

    /**
     * Write the closing quote character for the current string and transition
     * to the next expected state.
     * Throws `JSONIopipeException` if not currently inside a string.
     */
    void endString()
    {
        if (_state == State.String)
        {
            putStr(currentQuoteChar[]);
            _state = (stackLen == 0) ? State.End : State.Comma;
        }
        else if(_state == State.MemberString)
        {
            putStr(currentQuoteChar[]);
            _state = State.Colon;
        }
        else
            throw new JSONIopipeException("Attempting to end a string when not in a string");
    }

    /**
     * Write a numeric value to the output.
     * Throws `JSONIopipeException` if the current state does not allow a value,
     * or if the resulting text is not a valid JSON/JSON5 number.
     *
     * Params:
     *   value  = The numeric value to write.
     *   format = How to format `value` using `std.format`.
     */
    void addNumber(T)(T value, string format = "%s")
    {
        if (!canAddValue())
            throw new JSONIopipeException(.format("addNumber not allowed (state = %s)", _state));
        // save the original position
        auto origPos = pos;
        formattedWrite(&putStr, format, value);
        
        // validate the number by parsing it.
        // TODO: fill this out

        _state = (stackLen == 0) ? State.End : State.Comma;
    }

    /**
     * Write a JSON or JSON5 keyword literal to the output.
     * Throws `JSONIopipeException` if the current state does not allow a value.
     *
     * Params:
     *   value = The keyword literal to write; see `KeywordValue`.
     */
    void addKeywordValue(KeywordValue value)
    {
        if (!canAddValue())
            throw new JSONIopipeException(format("addKeywordValue not allowed (state = %s)", _state));

        putStr(cast(string)value);

        _state = (stackLen == 0) ? State.End : State.Comma;
    }

    /**
     * Write the `:` separator between an object member name and its value.
     * Throws `JSONIopipeException` if a member name has not just been written.
     */
    void addColon()
    {
        if(_state != State.Colon)
            throw new JSONIopipeException(format("Tried to add a colon when unexpected (state = %s)", _state));

        putStr(":");
        _state = State.Value;
    }

    /**
     * Write the `,` separator between members of an object or array.
     * Throws `JSONIopipeException` if a complete value inside an aggregate has not just been written.
     */
    void addComma()
    {
        if(_state != State.Comma)
            throw new JSONIopipeException(format("Tried to add a comma when unexpected (state = %s)", _state));

        putStr(",");
        _state = State.Member;
    }

    /**
     * Write raw whitespace into the output between tokens.
     * Throws `JSONIopipeException` if currently inside a string.
     *
     * Params:
     *   validate = When `true`, validates that `data` contains only whitespace characters.
     *   data     = The whitespace characters to write.
     */
    void addWhitespace(bool validate = false, T)(T data) // if (isSomeCharRange!T)
    {
        if(_state == State.String || _state == State.MemberString)
            throw new JSONIopipeException("Cannot add whitespace inside string value");

        static if(validate)
            static assert(false, "whitespace validation not yet impelmented");
        import std.range : put;
        auto r = &putStr;
        put(r, data);
    }

    /**
     * JSON5 only. Write a raw comment into the output between tokens.
     * Throws `JSONIopipeException` if currently inside a string.
     *
     * Params:
     *   validate    = When `true`, validates that `commentData` is a well-formed JSON5 comment.
     *   commentData = The comment text to write, including the comment delimiters.
     */
    static if(JSON5)
        void addComment(bool validate = false, T)(T commentData) // if (isSomeCharRange!T)
    {
        if(_state == State.String || _state == State.MemberString)
            throw new JSONIopipeException("Cannot add comments inside string value");

        static if(validate)
            static assert(false, "comment validation not yet impelmented");
        import std.range : put;
        auto r = &putStr;
        put(r, commentData);
    }

    /**
     * Release all characters written so far from the output chain's window,
     * allowing the underlying buffer to reclaim that space. For push iopipes,
     * this queues the data for writing to the output stream.
     */
    void flushWritten()
    {
        outputChain.release(pos);
        pos = 0;
    }
}

/**
 * JSON writer that wraps `JSONOutputter` and adds indentation, newlines, and
 * configurable colon spacing to produce human-readable output. Additional
 * whitespace can be added as needed. The public interface mirrors
 * `JSONOutputter`.
 *
 * Use `jsonFormatter` for IFTI construction.
 *
 * Params:
 *   Chain = A text iopipe to write JSON into.
 *   JSON5 = When `true`, enables JSON5 extensions.
 */
struct JSONStandardFormatter(Chain, bool JSON5 = false)
if (isIopipe!Chain)
{
private:

    JSONOutputter!(Chain, JSON5) outputter;
    int indent;
    ColonSpacing colonSpacing;

    void putIndent(size_t depth)
    {
        outputter.addWhitespace("\n");
        // TODO: make this less cumbersome
        import std.range : repeat;
        outputter.addWhitespace(repeat(' ', indent * depth));
    }

    void spacingBeforeValue()
    {
        // if starting a member, then output index. otherwise (after colon), do not.
        if(outputter.state == State.Member || state == State.First)
            putIndent(outputter.depth);
    }

public:
    /**
     * Construct a `JSONStandardFormatter` that writes into `chain`.
     *
     * Params:
     *   chain         = The output iopipe to write JSON into.
     *   pos           = Initial write offset within `chain`'s window.
     *   indent        = Number of spaces per indentation level.
     *   colonSpacing  = Whitespace style around the `:` separator.
     */
    this(ref Chain chain, size_t pos = 0, int indent = 4, ColonSpacing colonSpacing = ColonSpacing.After) {
        assert(indent >= 0);
        this.outputter = typeof(outputter)(chain, pos);
        this.indent = indent;
        this.colonSpacing = colonSpacing;
    }

    /// The current state of the output state machine.
    State state() const @property nothrow => outputter.state();
    /// The number of characters written into the output chain's window so far.
    size_t position() const @property nothrow => outputter.position();
    /// A slice of the output chain's window containing all characters written so far.
    auto window() => outputter.window();
    /// `true` when the innermost open aggregate is an object.
    bool inObj() const @property nothrow => outputter.inObj();
    /// The number of currently open (not yet closed) objects and arrays.
    size_t depth() const @property nothrow => outputter.depth();

    /**
     * Write `{` to the output and push a new object onto the aggregate stack.
     * Throws `JSONIopipeException` if the current state does not allow a value.
     */
    void beginObject() {
        spacingBeforeValue();
        outputter.beginObject();
    }

    /**
     * Write `[` to the output and push a new array onto the aggregate stack.
     * Throws `JSONIopipeException` if the current state does not allow a value.
     */
    void beginArray() {
        spacingBeforeValue();
        outputter.beginArray();
    }

    /**
     * Close the innermost open object or array, writing `}` or `]` to the
     * output. May be called on an empty aggregate, after a complete value, or —
     * in JSON5 mode — after a trailing comma.
     * Throws `JSONIopipeException` if no aggregate is open or the state does
     * not allow closing.
     */
    void endAggregate() {
        import std.algorithm : max;
        putIndent(max(outputter.depth, 1) - 1);
        outputter.endAggregate();
    }

    /**
     * Write an opening `"` and transition into string-writing mode.
     * Follow with zero or more calls to `addStringData`, then `endString`.
     * Throws `JSONIopipeException` if the current state does not allow a string.
     */
    void beginString()
    {
        spacingBeforeValue();
        outputter.beginString();
    }

    /**
     * JSON5 only. Write an opening quote character and transition into
     * string-writing mode, using the specified quote character.
     * Follow with zero or more calls to `addStringData`, then `endString`.
     * Throws `JSONIopipeException` if the current state does not allow a string.
     *
     * Params:
     *   quoteChar = The quote character to use; must be `'` or `"`.
     */
    static if(JSON5)
    void beginString(char quoteChar)
    {
        spacingBeforeValue();
        outputter.beginString(quoteChar);
    }


    /**
     * Write a complete, double-quoted object member name and transition to
     * expecting a colon.
     * Throws `JSONIopipeException` if not currently inside an object at a
     * position where a member name is expected.
     *
     * Params:
     *   memberName = The member name to write.
     */
    void addMemberName(const(char)[] memberName)
    {
        spacingBeforeValue();
        outputter.addMemberName(memberName);
    }

    /**
     * JSON5 only. Write a complete object member name using the specified
     * quoting style and transition to expecting a colon.
     * Throws `JSONIopipeException` if not currently inside an object at a
     * position where a member name is expected, or if `style` is `Symbol` and
     * `memberName` is not a valid JSON5 identifier.
     *
     * Params:
     *   memberName = The member name to write.
     *   style      = How to quote the member name.
     */
    static if(JSON5)
    void addMemberName(const(char)[] memberName, MemberNameStyle style)
    {
        spacingBeforeValue();
        outputter.addMemberName(memberName, style);
    }

    /**
     * Write data into the currently open string according to `mode`.
     * May be called multiple times between `beginString` and `endString`.
     * Throws `JSONIopipeException` if not currently inside a string, or if
     * `mode` is `StringMode.Validate` and `data` contains invalid JSON string
     * content. Note that escape sequences must not be split across calls when
     * using `StringMode.Validate`.
     *
     * Params:
     *   mode = Controls how `data` is interpreted; see `StringMode`.
     *   data = The content to append, converted to string data.
     */
    void addStringData(StringMode mode = StringMode.AddEscapes, T)(T data) => outputter.addStringData!mode(data);

    /**
     * Write the closing quote character for the current string and transition
     * to the next expected state.
     * Throws `JSONIopipeException` if not currently inside a string.
     */
    void endString() => outputter.endString();

    /**
     * Write a numeric value to the output.
     * Throws `JSONIopipeException` if the current state does not allow a value,
     * or if the resulting text is not a valid JSON/JSON5 number.
     *
     * Params:
     *   value  = The numeric value to write.
     *   format = How to format `value` using `std.format`.
     */
    void addNumber(T)(T value, string format = "%s")
    {
        spacingBeforeValue();
        outputter.addNumber(value, format);
    }

    /**
     * Write a JSON or JSON5 keyword literal to the output.
     * Throws `JSONIopipeException` if the current state does not allow a value.
     *
     * Params:
     *   value = The keyword literal to write; see `KeywordValue`.
     */
    void addKeywordValue(KeywordValue value)
    {
        spacingBeforeValue();
        outputter.addKeywordValue(value);
    }

    /**
     * Write the `:` separator between an object member name and its value,
     * with whitespace applied according to the `colonSpacing` setting.
     * Throws `JSONIopipeException` if a member name has not just been written.
     */
    void addColon()
    {
        final switch(colonSpacing) with(ColonSpacing)
        {
        case None:
            outputter.addColon();
            break;
        case Both:
            outputter.addWhitespace(" ");
            goto case After;
        case After:
            outputter.addColon();
            outputter.addWhitespace(" ");
            break;
        }
    }

    /**
     * Write the `,` separator between members of an object or array.
     * Throws `JSONIopipeException` if a complete value inside an aggregate has not just been written.
     */
    void addComma() => outputter.addComma();

    /**
     * Write raw whitespace into the output between tokens, in addition to
     * the automatic indentation rules.
     * Throws `JSONIopipeException` if currently inside a string.
     *
     * Params:
     *   validate = When `true`, validates that `data` contains only whitespace characters.
     *   data     = The whitespace characters to write.
     */
    void addWhitespace(bool validate = false, T)(T data) => outputter.addWhitespace!validate(data);

    /**
     * JSON5 only. Write a raw comment into the output between tokens.
     * Throws `JSONIopipeException` if currently inside a string.
     *
     * Params:
     *   validate    = When `true`, validates that `commentData` is a well-formed JSON5 comment.
     *   commentData = The comment text to write, including the comment delimiters.
     */
    static if(JSON5)
        void addComment(bool validate = false, T)(T commentData) => outputter.addComment!validate(commentData);

    /**
     * Release all characters written so far from the output chain's window,
     * allowing the underlying buffer to reclaim that space. For push iopipes,
     * this queues the data for writing to the output stream.
     */
    void flushWritten() => outputter.flushWritten();
}

/**
 * Construct a `JSONStandardFormatter` for `chain`.
 *
 * Params:
 *   JSON5         = When `true`, enables JSON5 extensions.
 *   chain         = The output iopipe to write JSON into.
 *   pos           = Initial write offset within `chain`'s window.
 *   indent        = Number of spaces per indentation level.
 *   colonSpacing  = Whitespace style around the `:` separator.
 */
auto jsonFormatter(bool JSON5 = false, Chain)(ref Chain chain, size_t pos = 0, int indent=4, ColonSpacing colonSpacing = ColonSpacing.After)
{
    return JSONStandardFormatter!(Chain, JSON5)(chain, pos, indent, colonSpacing);
}

unittest
{
    import iopipe.traits : isIopipe;
    import iopipe.bufpipe : ensureElems;
    import std.string : strip;
    import std.stdio : writeln;

    auto chain = bufd!char();

    auto fmt = chain.jsonFormatter;

    fmt.beginObject();
    fmt.addMemberName("a");
    fmt.addColon();
    fmt.addNumber(1);
    fmt.endAggregate();

    auto s = fmt.window;
    assert(s == "{\n    \"a\": 1\n}");

    // do the same without formatting.
    auto fmt2 = JSONOutputter!(typeof(chain))(chain);

    fmt2.beginObject();
    fmt2.addMemberName("a");
    fmt2.addColon();
    fmt2.addNumber(1);
    fmt2.endAggregate();

    s = fmt2.window;
    assert(s == "{\"a\":1}");
}

unittest
{
    // Simple array: [1,2,3]
    auto chain = bufd!char();
    auto fmt = chain.jsonFormatter;

    fmt.beginArray();
    fmt.addNumber(1);
    fmt.addComma();
    fmt.addNumber(2);
    fmt.addComma();
    fmt.addNumber(3);
    fmt.endAggregate();

    auto s = fmt.window;
    assert(s == "[\n    1,\n    2,\n    3\n]");
}

unittest
{
    // String with escapes and single‑quote behavior
    auto chain = bufd!char;
    auto fmt = chain.jsonFormatter;

    // double‑quoted: inner " and \ should be escaped
    fmt.beginString();
    fmt.addStringData(`He said: "hi" \ test`);
    fmt.endString();

    auto s = fmt.window;
    assert(s == `"He said: \"hi\" \\ test"`);

    // reset chain, now test single‑quoted: inner ' must be escaped as \'
    auto fmt2 = chain.jsonFormatter!true;

    fmt2.beginString('\'');
    fmt2.addStringData(`It's ok`);
    fmt2.endString();

    s = fmt2.window;
    assert(s == `'It\'s ok'`);
}

unittest
{
    // addEscapes: real newline becomes literal "\n" in JSON
    auto chain = bufd!char();
    auto fmt = chain.jsonFormatter;

    fmt.beginString();
    fmt.addStringData("line1\nline2"); // contains an actual newline character
    fmt.endString();

    auto s = fmt.window;
    // Expect JSON text with a backslash-n escape, not a raw newline
    assert(s == `"line1\nline2"`);
}

unittest
{
    // StringMode.validate: accept only correctly escaped content
    import std.exception : assertThrown;
    import std.string : strip;

    auto chain = bufd!char();

    // 1) Double‑quoted string with valid escapes should pass unchanged
    auto fmt = chain.jsonFormatter;
    fmt.beginString();
    fmt.addStringData!(StringMode.Validate)(`He said: \"hi\" \\ test`);
    fmt.endString();

    auto s = fmt.window;
    assert(s == `"He said: \"hi\" \\ test"`);

    // 2) Invalid escape ("\q") should throw in validate mode
    auto fmt2 = chain.jsonFormatter;
    fmt2.beginString();
    assertThrown!JSONIopipeException(fmt2.addStringData!(StringMode.Validate)(`He said: \q`));

    // 3) Lone backslash at end should throw in validate mode
    auto fmt3 = chain.jsonFormatter;
    fmt3.beginString();
    assertThrown!JSONIopipeException(fmt3.addStringData!(StringMode.Validate)(`oops \`));

    // 4) Unescaped quote should throw in validate mode
    auto fmt4 = chain.jsonFormatter;
    fmt4.beginString();
    assertThrown!JSONIopipeException(fmt4.addStringData!(StringMode.Validate)(`He said: "hi"`));

    // 5) Single‑quoted validate: \' is allowed, bare ' is not
    auto fmt5 = chain.jsonFormatter!true;
    fmt5.beginString('\'');
    fmt5.addStringData!(StringMode.Validate)(`It\'s ok`);
    fmt5.endString();

    s = fmt5.window;
    assert(s == `'It\'s ok'`);

    auto fmt6 = chain.jsonFormatter!true;
    fmt6.beginString('\'');
    assertThrown!JSONIopipeException(fmt6.addStringData!(StringMode.Validate)(`It's bad`));
}

unittest
{
    // StringMode.Validate: cover all standard JSON escapes and \u forms
    import std.exception : assertThrown;

    auto chain = bufd!char();

    // 1) All standard single-character escapes should be accepted
    auto fmt = chain.jsonFormatter;
    fmt.beginString();
    fmt.addStringData!(StringMode.Validate)(`\" \\ \/ \b \f \n \r \t`);
    fmt.endString();

    // 2) Valid \u escape should be accepted
    auto fmt2 = chain.jsonFormatter;
    fmt2.beginString();
    fmt2.addStringData!(StringMode.Validate)(`\u00AF`);
    fmt2.endString();

    // 3) Too-short \u escape should throw
    auto fmt3 = chain.jsonFormatter;
    fmt3.beginString();
    assertThrown!JSONIopipeException(fmt3.addStringData!(StringMode.Validate)(`\u12`));

    // 4) \u escape with non-hex digits should throw
    auto fmt4 = chain.jsonFormatter;
    fmt4.beginString();
    assertThrown!JSONIopipeException(fmt4.addStringData!(StringMode.Validate)(`\u12xz`));

    // 5) Bare control character (< 0x20) should throw
    auto fmt5 = chain.jsonFormatter;
    fmt5.beginString();
    string bad = "ok" ~ "\x01"; // embed a real control char 0x01
    assertThrown!JSONIopipeException(fmt5.addStringData!(StringMode.Validate)(bad));
}

unittest
{
    // Keywords: null/true/false
    auto chain = bufd!char();
    auto fmt = chain.jsonFormatter;

    fmt.beginArray();
    fmt.addKeywordValue(KeywordValue.Null);
    fmt.addComma();
    fmt.addKeywordValue(KeywordValue.True);
    fmt.addComma();
    fmt.addKeywordValue(KeywordValue.False);
    fmt.endAggregate();

    auto s = fmt.window;
    assert(s == "[\n    null,\n    true,\n    false\n]");
}

unittest
{
    // ERROR: endAggregate without beginObject or beginArray
    import std.exception : assertThrown;

    auto chain = bufd!char();
    auto fmt = chain.jsonFormatter;

    assertThrown!JSONIopipeException(fmt.endAggregate());
}

unittest
{
    // ERROR: addMember when not inside object
    import std.exception : assertThrown;

    auto chain = bufd!char();
    auto fmt = chain.jsonFormatter;

    assertThrown!JSONIopipeException(fmt.addMemberName("a"));

    auto fmt2 = chain.jsonFormatter;
    fmt2.beginArray();
    assertThrown!JSONIopipeException(fmt2.addMemberName("a"));

}

unittest
{
    // Test adding member name via beginString calls
    auto chain = bufd!char;
    auto fmt = chain.jsonFormatter;

    fmt.beginObject();
    fmt.beginString();
    fmt.addStringData("a");
    fmt.endString();
    fmt.addColon();
    fmt.addNumber(1);
    fmt.endAggregate();

    auto s = fmt.window;
    assert(s == "{\n    \"a\": 1\n}");
}

unittest
{
    // ERROR: nested beginString
    import std.exception : assertThrown;

    auto chain = bufd!char();
    auto fmt = chain.jsonFormatter;

    fmt.beginString();
    assertThrown!JSONIopipeException(fmt.beginString());
}

unittest
{
    // ERROR: addStringData without beginString
    import std.exception : assertThrown;

    auto chain = bufd!char();
    auto fmt = chain.jsonFormatter;

    assertThrown!JSONIopipeException(fmt.addStringData("oops"));
}

unittest
{
    // ERROR: endString without beginString
    import std.exception : assertThrown;

    auto chain = bufd!char();
    auto fmt = chain.jsonFormatter;

    assertThrown!JSONIopipeException(fmt.endString());
}

unittest
{
    // JSON5 options
    auto chain = bufd!char();
    auto fmt = chain.jsonFormatter!true;

    
    fmt.beginObject();
    fmt.addMemberName("a", MemberNameStyle.SingleQuote);
    fmt.addColon();
    fmt.addNumber(1);
    fmt.addComma();
    fmt.addMemberName("b", MemberNameStyle.Symbol);
    fmt.addColon();
    fmt.addNumber(2);
    fmt.addComma(); // trailing comma
    fmt.addComment(" // trailing comma\n");
    fmt.endAggregate();
    
    auto s = fmt.window;
    assert(s == "{\n    'a': 1,\n    b: 2, // trailing comma\n\n}");
}

/+
TODO: enable when number validation is working
unittest
{
    // ERROR: invalid JSON number string
    import std.exception : assertThrown;

    auto chain = bufd!char();
    auto fmt = chain.jsonFormatter;

    assertThrown!JSONIopipeException(fmt.addNumber("01"));
}
+/
