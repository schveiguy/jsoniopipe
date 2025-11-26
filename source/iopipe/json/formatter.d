module iopipe.json.formatter;

public import iopipe.json.common;

import iopipe.traits : isIopipe, WindowType;
import iopipe.bufpipe; 

import std.bitmanip : BitArray;
import std.exception : enforce;
import std.traits : isNumeric;
import std.conv : to;
import std.regex;
import std.algorithm.iteration : substitute;
import std.algorithm.searching : startsWith, endsWith;
import std.format;


struct MemberOptions {
    char quoteChar = '"';        // Use " or ' for quotes
    bool escapeKey = true;       // Whether to escape special chars in key
    
    enum ColonSpacing {
        none,  // "key":"value"
        after, // "key": "value"
        both,  // "key" : "value"
    }
    
    ColonSpacing colonSpacing = ColonSpacing.after;  // Default spacing
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

struct JSONFormatter(Chain) 
if (isIopipe!Chain)
{
private:

    Chain* outputChain;
    int spacing = 0;
    int indent = 4;
    MemberOptions memberOptions;

    size_t nWritten = 0;
    char currentQuoteChar = '"';

    private enum State : ubyte
    {
        Begin,  // next item should be either an Object or Array or String
        First,  // Just started a new object or array.
        Member, // Expect next member (name for object, value for array_
        Value,  // Expect value
        End     // there shouldn't be any more items
    }

    // 0 = array, 1 = object (same convention as parser)
    BitArray stack;
    size_t   stackLen;
    State    state = State.Begin;

    bool inObj() @property
    {
        return stackLen == 0 ? false : stack[stackLen - 1];
    }

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
        state = (--stackLen == 0) ? State.End : State.Member;
    }

    void putIndent() {
        outputChain.ensureElems(nWritten + indent * spacing + 1);
        outputChain.window[nWritten] = '\n';
        outputChain.window[nWritten + 1 .. nWritten + 1 + indent * spacing] = ' ';
        nWritten += indent * spacing + 1;
    }

    void putStr(const(char)[] s) {
        outputChain.ensureElems(nWritten + s.length);
        outputChain.window[nWritten .. nWritten + s.length] = s;
        nWritten += s.length;
    }

    bool isValidJSONNumber(string str) {
        static auto numberRegex = regex(r"^-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?$");
        return !str.matchFirst(numberRegex).empty;
    }

    void startObjectMember()
    {
        enforce(inObj(), "startObjectMember only valid inside object");

        final switch (state) with (State)
        {
        case First:
            // first member: no comma
            state = Member;
            break;
        case Member:
            // subsequent member: comma + indent
            putStr(",");
            putIndent();
            // stay in Member
            break;
        case Begin, Value, End:
            throw new JSONIopipeException(
                "startObjectMember not allowed in current state");
        }
    }

    void startArrayValue()
    {
        enforce(!inObj(), "startArrayValue only valid inside array");

        final switch (state) with (State)
        {
        case First:
            // first element: no comma
            state = Member;
            break;
        case Member:
            // subsequent element: comma + indent
            putStr(",");
            putIndent();
            // stay in Member
            break;
        case Begin, Value, End:
            throw new JSONIopipeException(
                "startArrayValue not allowed in current state");
        }
    }

    private void endStringRaw() {
      putStr((&currentQuoteChar)[0 .. 1]);
      // no state change
    }

public:
    this(ref Chain chain) {
        outputChain = &chain;
    }

    this(ref Chain chain, int spacing, int indent) {
        outputChain = &chain;
        this.spacing = spacing;
        this.indent = indent;
    }

    void beginObject() {
        final switch (state) with (State)
        {
        case Begin, Member, Value:
            break;
        case First, End:
            throw new JSONIopipeException(
                "beginObject not allowed in current state");
        }

        pushContainer(true);  // object
        state = State.First;  // just started

        ++spacing;
        putStr("{");
        putIndent();
    }

    void endObject() {
        enforce(inObj(), "endObject outside object");

        --spacing;
        putIndent();
        putStr("}");

        popContainer(); // State.End or State.Comma
    }

    void beginArray() {
        final switch (state) with (State)
        {
        case Begin, Member, Value:
            break;
        case First, End:
            throw new JSONIopipeException(
                "beginArray not allowed in current state");
        }

        pushContainer(false); // array
        state = State.First;

        ++spacing;
        putStr("[");
        putIndent();
    }

    void endArray() {
        enforce(!inObj(), "endArray outside array");

        --spacing;
        putIndent();
        putStr("]");

        popContainer();
    }

    // Public hook for arrays: call before each element
    void beginArrayValue()
    {
        startArrayValue();
    }

    void addMember(T)(T key) {
        addMember(key, memberOptions);
    }

    void addMember(T)(T key, MemberOptions options) {
        
        enforce(inObj(), "addMember only valid inside object");

        final switch (state) with (State)
        {
        case First, Member:
            break;
        case Begin, Value, End:
            throw new JSONIopipeException(
                "addMember not allowed in current state");
        }

        startObjectMember();

        static if (is(typeof(key) == string))
        {
            if (options.escapeKey)
            {
                beginString(options.quoteChar);
                addStringData(key);
                endStringRaw();
            }
            else
            {
                beginString(options.quoteChar);
                addStringData!(false, false)(key);
                endStringRaw();
            }
        }
        else
        {
            putStr(to!string(key));
        }

        final switch (options.colonSpacing)
        {
        case MemberOptions.ColonSpacing.none:
            putStr(":");
            break;
        case MemberOptions.ColonSpacing.after:
            putStr(": ");
            break;
        case MemberOptions.ColonSpacing.both:
            putStr(" : ");
            break;
        }

        state = State.Value;
    }

    void beginString(char quoteChar = '"') {
        // value allowed at: Begin (root), Member (array element), Value (object value)
        final switch (state) with (State)
        {
        case Begin, Member, Value:
            break;
        case First, End:
            throw new JSONIopipeException(
                "beginString not allowed in current state");
        }

        if (quoteChar != '"' && quoteChar != '\'')
            throw new JSONIopipeException("Invalid quote character");

        currentQuoteChar = quoteChar;
        putStr((&quoteChar)[0 .. 1]);
    }

    // will automatically escape string data as needed.
    void addStringData(bool validate = true, bool addEscapes = true, T)(T value) {
        static if (validate) {
            // Validate that the string doesn't contain invalid characters
            foreach(char c; value) {
                if (c < 0x20 && c != '\t' && c != '\n' && c != '\r') {
                    throw new JSONIopipeException(format("Invalid control character \\u%04X in string", cast(int)c));
                }
            }
        }
    
        static if (addEscapes) {
            auto escaped = value.substitute!(jsonEscapeSubstitutions!()).to!string;
            putStr(escaped);
        } else {
            putStr(value);
        }
    }


    void endString() {
        putStr((&currentQuoteChar)[0 .. 1]);

        if (stackLen == 0)
            state = State.End;
        else
        // If it's at the end of an array or an object,
        // endArray() and endObject() will set the state appropriately
            state = State.Member;
    }

    // allow numeric types, or strings that are validated to be JSON numbers
    void addNumericData(T)(T value, string formatStr = "%s") {
        
        final switch (state) with (State)
        {
        case Begin, Member, Value:
            break;
        case First, End:
            throw new JSONIopipeException(
                "addNumericData not allowed in current state");
        }
        
        static if (is(T == string)) {
            if (!isValidJSONNumber(value)) {
                throw new JSONIopipeException(format("Invalid JSON number: %s", value));
            }
            putStr(value);
        } else {
            static assert(isNumeric!T, "addNumericData requires a numeric type");
            formattedWrite(&putStr, formatStr, value);
        }

        if (stackLen == 0)
            state = State.End;
        else
            state = State.Member;
    }

    // null, true, false, inf, etc.
    void addKeywordValue(KeywordValue value) {
        final switch (state) with (State)
        {
        case Begin, Member, Value:
            break;
        case First, End:
            throw new JSONIopipeException(
                "addKeywordValue not allowed in current state");
        }

        putStr(value);

        if (stackLen == 0)
            state = State.End;
        else
            state = State.Member;
    }

    void addWhitespace(T)(T data) {
        foreach(char c; data) {
            if (c != ' ' && c != '\t' && c != '\n' && c != '\r') {
                throw new JSONIopipeException(format("Invalid whitespace character: \\u%04X", cast(int)c));
            }
        }
        putStr(data);
    }

    // // add a comment (JSON5 only), must be a complete comment (validated)
    void addComment(T)(T commentData) {
        static if (is(T == string)) {
            if (!commentData.startsWith("//") && !commentData.startsWith("/*") && !commentData.endsWith("*/")) {
                throw new JSONIopipeException(format("Invalid comment format: %s", commentData));
            }
        }
        putStr(commentData);
    }

    void flushWritten() {
        outputChain.release(nWritten);
        nWritten = 0;
    }

    auto chain() {
        return outputChain;
    }

}

/** Eponymous template that generates an argument list for std.algorithm.substitute to correctly escape json strings
 * Result:
 *      AliasSeq!("<char>", "<escapesequence>", "<char>", "<escapesequence>", ...);
 * 
 * copied from serialize.d
 */
private template jsonEscapeSubstitutions()
{
    import std.algorithm.iteration;
    import std.range;
    import std.ascii: ControlChar;

    // Control characters [0,31 aka 0x1f] + 2 special characters '"' and '\'
    private enum charactersToEscape = chain(only('\"', '\\'), iota(0x1f + 1));

    private struct JsonEscapeMapping {
        char chr;
        string escapeSequence;
    }

    /* Special characters we have to (in case of '"' and '\') per the spec or want to escape seperately for readability
    * Everything else gets converted to the "\uxxxx" unicode escape
    */
    private enum JsonEscapeMapping[7] JSON_ESCAPES = [
        {'\"', `\"`},
        {'\\', `\\`},
        {ControlChar.bs, `\b`},
        {ControlChar.lf, `\n`},
        {ControlChar.cr, `\r`},
        {ControlChar.tab, `\t`},
        {ControlChar.ff, `\f`},
    ];

    private JsonEscapeMapping escapeSingleChar(int c)
    {
        switch(c)
        {
            static foreach(e; JSON_ESCAPES)
            {
                case e.chr:
                    return e;
            }
            default:
                return JsonEscapeMapping(cast(char)c, format!`\u%04x`(c));
        }
    }

    // Convert struct to AliasSeq with correct types so it can be used as parameters of a function
    private template UnpackStruct(JsonEscapeMapping jem)
    {
        // Must convert char to string for use with substitute
        enum string staticCast(char c) = c.to!string;
        enum string staticCast(string s) = s;
        alias UnpackStruct = staticMap!(staticCast, jem.tupleof);
    }

    import std.meta;
    private alias jsonEscapeSubstitutions = staticMap!(UnpackStruct, aliasSeqOf!(charactersToEscape.map!escapeSingleChar));
}