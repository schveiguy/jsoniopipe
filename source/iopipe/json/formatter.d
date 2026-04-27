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

enum ColonSpacing {
    None,  // "key":"value"
    After, // "key": "value"
    Both,  // "key" : "value"
}

enum MemberNameStyle
{
    DoubleQuote,
    SingleQuote,
    Symbol,
    Auto /// Pick the best one for JSON5 based on the member name
}

// How to treat string data passed to addStringData
enum StringMode : ubyte
{
    passThru,   // do not modify or validate the contents
    addEscapes, // escape any characters that would be invalid in JSON (default)
    validate    // validate that existing escapes / characters are JSON-valid
}


enum KeywordValue : string {
    Null = "null",
    True = "true",
    False = "false",
    // JSON5 extensions
    Infinity = "Infinity",
    NegativeInfinity = "-Infinity",
    NaN = "NaN"
}

/++
    Mechanism to output json data to a character iopipe.

    This does not by default put any spacing, but allows spacing to be added as
    needed. It can function as a formatter which does not add any formatting (spacing).
 +/
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

    this(ref Chain chain, size_t pos = 0)
    {
        this.outputChain = &chain;
        this.pos = pos;
    }
    // public access to state for formatting purposes
    enum State : ubyte
    {
        Begin,  // next item should be either an Object or Array or String or Number or Keyword
        First,  // Expect the first member of a new object or array or end of aggregate (EOA)
        Member, // Expect next member (key for object, value for array) or EOA(JSON5 only)
        Colon, // Expect colon token
        Comma, // Expect comma/EOA
        Value,  // Expect value of object field
        String, // Inside a value string
        MemberString, // Inside a membername string
        End     // there shouldn't be any more items
    }

    State state() const @property nothrow => _state;
    size_t position() const @property nothrow => pos;
    auto window() => outputChain.window[0 .. pos];
    bool inObj() const @property nothrow => stackLen == 0 ? false : stack[stackLen - 1];
    size_t depth() const @property nothrow => stackLen;

    void beginObject()
    {
        if (!canAddValue)
            throw new JSONIopipeException(format("beginObject not allowed (state = %s)", _state));

        pushContainer(true);  // object
        _state = State.First;
        putStr("{");
    }

    void beginArray()
    {
        if (!canAddValue)
            throw new JSONIopipeException(format("beginObject not allowed (state = %s)", _state));

        pushContainer(false); // array
        _state = State.First;
        putStr("[");
    }

    void endAggregate()
    {
        if (!canEndAggregate)
            throw new JSONIopipeException(format("endAggregate not allowed (state = %s)", _state));

        putStr(inObj ? "}" : "]");
        popContainer();
    }

    void beginString()
    {
        if (!beginStringState())
            throw new JSONIopipeException(format("beginString not allowed (state = %s)", _state));

        putStr("\"");
        static if (JSON5)
            currentQuoteChar[0] = '"';
    }

    static if(JSON5) void beginString(char quoteChar)
    {
        assert(quoteChar == '\'' || quoteChar == '"');
        if (!beginStringState())
            throw new JSONIopipeException(format("beginString not allowed (state = %s)", _state));
        putStr((&quoteChar)[0 .. 1]);
        currentQuoteChar[0] = quoteChar;
    }

    // put a full member name. Uses default JSON quote style
    void addMemberName(const(char)[] memberName)
    {
        if (!inObj || (_state != State.First && _state != State.Member))
            throw new JSONIopipeException("cannot add member name when not inside object or at member state");
        beginString();
        addStringData(memberName);
        endString();
    }

    // put a full member name. Uses default JSON quote style
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

    // add string data. The mode of adding data can be one of three:
    // 1. addEscapes - any invalid string characters per the JSON spec will be replaced with escapes
    // 2. passThru - data will be added as is, no validation.
    // 3. validate - this string data must be valid JSON string data, invalid
    // data will cause the function to throw. Note that splitting escape
    // sequences across calls to this function will fail. e.g. `addStringData("abc\\"); addStringData("n");`
    void addStringData(StringMode mode = StringMode.addEscapes, T)(T data)
    {
        if (_state != State.String && _state != State.MemberString)
            throw new JSONIopipeException("cannot add string data outside of beginString/endString");
        static if (mode == StringMode.addEscapes)
        {
            // write the string data, escaping any characters that should be escaped.
            formattedWrite(&putEscapedStr, "%s", data);
        }
        else
        { 
            static if (mode == StringMode.validate)
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

    void addKeywordValue(KeywordValue value)
    {
        if (!canAddValue())
            throw new JSONIopipeException(format("addKeywordValue not allowed (state = %s)", _state));

        putStr(cast(string)value);

        _state = (stackLen == 0) ? State.End : State.Comma;
    }

    void addColon()
    {
        if(_state != State.Colon)
            throw new JSONIopipeException(format("Tried to add a colon when unexpected (state = %s)", _state));

        putStr(":");
        _state = State.Value;
    }

    void addComma()
    {
        if(_state != State.Comma)
            throw new JSONIopipeException(format("Tried to add a comma when unexpected (state = %s)", _state));

        putStr(",");
        _state = State.Member;
    }

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

    static if(JSON5)
        void addComment(bool validate = false, T)(T commentData) // if (isSomeCharRange!T)
    {
        if(_state == State.String || _state == State.MemberString)
            throw new JSONIopipeException("Cannot add comments inside string value");

        static if(validate)
            static assert(false, "comment validation not yet impelmented");
        import std.range : put;
        put(&putStr, data);
    }

    void flushWritten()
    {
        outputChain.release(pos);
        pos = 0;
    }
}

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
        if(outputter.state == outputter.State.Member || outputter.state == outputter.State.First)
            putIndent(outputter.depth);
    }

public:
    this(ref Chain chain, size_t pos = 0, int indent = 4, ColonSpacing colonSpacing = ColonSpacing.After) {
        assert(indent >= 0);
        this.outputter = typeof(outputter)(chain, pos);
        this.indent = indent;
        this.colonSpacing = colonSpacing;
    }

    void beginObject() {
        spacingBeforeValue();
        outputter.beginObject();
    }

    void beginArray() {
        spacingBeforeValue();
        outputter.beginArray();
    }

    void endAggregate() {
        import std.algorithm : max;
        putIndent(max(outputter.depth, 1) - 1);
        outputter.endAggregate();
    }

    void beginString()
    {
        spacingBeforeValue();
        outputter.beginString();
    }

    static if(JSON5)
    void beginString(char quoteChar)
    {
        spacingBeforeValue();
        outputter.beginString(quoteChar);
    }


    void addMemberName(const(char)[] memberName)
    {
        spacingBeforeValue();
        outputter.addMemberName(memberName);
    }

    void addStringData(StringMode mode = StringMode.addEscapes, T)(T data)
    {
        outputter.addStringData!mode(data);
    }

    void endString()
    {
        outputter.endString();
    }

    void addNumber(T)(T value, string format = "%s")
    {
        spacingBeforeValue();
        outputter.addNumber(value, format);
    }

    void addKeywordValue(KeywordValue value)
    {
        spacingBeforeValue();
        outputter.addKeywordValue(value);
    }

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

    void addComma() => outputter.addComma();

    static if(JSON5)
        void addComment(bool validate = false, T)(T commentData) => outputter.addComment!validate(commentData);

    void flushWritten() => outputter.flushWritten();
    size_t position() const @property nothrow => outputter.position();
    auto window() => outputter.window();
}

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
    fmt.addStringData!(StringMode.validate)(`He said: \"hi\" \\ test`);
    fmt.endString();

    auto s = fmt.window;
    assert(s == `"He said: \"hi\" \\ test"`);

    // 2) Invalid escape ("\q") should throw in validate mode
    auto fmt2 = chain.jsonFormatter;
    fmt2.beginString();
    assertThrown!JSONIopipeException(fmt2.addStringData!(StringMode.validate)(`He said: \q`));

    // 3) Lone backslash at end should throw in validate mode
    auto fmt3 = chain.jsonFormatter;
    fmt3.beginString();
    assertThrown!JSONIopipeException(fmt3.addStringData!(StringMode.validate)(`oops \`));

    // 4) Unescaped quote should throw in validate mode
    auto fmt4 = chain.jsonFormatter;
    fmt4.beginString();
    assertThrown!JSONIopipeException(fmt4.addStringData!(StringMode.validate)(`He said: "hi"`));

    // 5) Single‑quoted validate: \' is allowed, bare ' is not
    auto fmt5 = chain.jsonFormatter!true;
    fmt5.beginString('\'');
    fmt5.addStringData!(StringMode.validate)(`It\'s ok`);
    fmt5.endString();

    s = fmt5.window;
    assert(s == `'It\'s ok'`);

    auto fmt6 = chain.jsonFormatter!true;
    fmt6.beginString('\'');
    assertThrown!JSONIopipeException(fmt6.addStringData!(StringMode.validate)(`It's bad`));
}

unittest
{
    // StringMode.validate: cover all standard JSON escapes and \u forms
    import std.exception : assertThrown;

    auto chain = bufd!char();

    // 1) All standard single-character escapes should be accepted
    auto fmt = chain.jsonFormatter;
    fmt.beginString();
    fmt.addStringData!(StringMode.validate)(`\" \\ \/ \b \f \n \r \t`);
    fmt.endString();

    // 2) Valid \u escape should be accepted
    auto fmt2 = chain.jsonFormatter;
    fmt2.beginString();
    fmt2.addStringData!(StringMode.validate)(`\u00AF`);
    fmt2.endString();

    // 3) Too-short \u escape should throw
    auto fmt3 = chain.jsonFormatter;
    fmt3.beginString();
    assertThrown!JSONIopipeException(fmt3.addStringData!(StringMode.validate)(`\u12`));

    // 4) \u escape with non-hex digits should throw
    auto fmt4 = chain.jsonFormatter;
    fmt4.beginString();
    assertThrown!JSONIopipeException(fmt4.addStringData!(StringMode.validate)(`\u12xz`));

    // 5) Bare control character (< 0x20) should throw
    auto fmt5 = chain.jsonFormatter;
    fmt5.beginString();
    string bad = "ok" ~ "\x01"; // embed a real control char 0x01
    assertThrown!JSONIopipeException(fmt5.addStringData!(StringMode.validate)(bad));
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
