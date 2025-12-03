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
    char currentQuoteChar = char.init;

    private enum State : ubyte
    {
        Begin,  // next item should be either an Object or Array or String
        First,  // Expect the first member of a new object or array.
        /*
        * In an object:
        *   - First
        *     - Value(key)
        *   - Value(value)
        *   
        *   - Member
        *     - Value(key)
        *   - Value(value)
        *   ...
        *
        * In an array:
        *   - First(value)
        *   - Member(value)
        *   ...
        */
        Member, // Expect next member (key for object, value for array)
        Value,  // Expect value
        End     // there shouldn't be any more items
    }

    // 0 = array, 1 = object (same convention as parser)
    BitArray stack;
    size_t   stackLen;
    State    state = State.Begin;

    bool inObj()  const @property nothrow
    {
        return stackLen == 0 ? false : stack[stackLen - 1];
    }

    // Can we add a value (string, number, keyword, begin object/array)?
    bool canAddValue() const nothrow
    {
        final switch (state) with (State)
        {
        case Begin:
            return true;  // root value
        case First:
            // only allowed for arrays; for objects we must go through addMember
            return !inObj();
        case Member:
            // only allowed for arrays; for objects we must go through addMember
            return !inObj();
        case Value:
            return true;
        case End:
            return false;
        }
    }

    // Can we add an object member (key)?
    // If so, add comma/indent as needed.
    bool canAddObjMember()
    {
        if (!inObj())
            return false;
        final switch (state) with (State)
        {
        case First:
            // no need to change state to Member, any key string added later will do that
            return true;
        case Member:
            putStr(",");
            putIndent();
            return true;
        case Begin:
        case Value:
        case End:
            return false;
        }
    }

    // Can we add an array member?
    // If so, add comma/indent as needed.
    bool canAddArrayMember()
    {
        if (inObj())
            return false;
        final switch (state) with (State)
        {
        case First:
            // no need to change state to Member, any value added later will do that
            return true;
        case Member:
            putStr(",");
            putIndent();
            return true;
        case Begin:
        case Value:
        case End:
            return false;
        }
    }

    bool canCloseObject() const nothrow
    {
        return inObj() && (state == State.Member || state == State.First);
    }

    bool canCloseArray() const nothrow
    {
        return stackLen > 0 && !inObj() && (state == State.Member || state == State.First);
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
        if (state == state.End)
            putIndent();
    }

    void putIndent()
    {
        outputChain.ensureElems(nWritten + indent * spacing + 1);
        outputChain.window[nWritten] = '\n';
        outputChain.window[nWritten + 1 .. nWritten + 1 + indent * spacing] = ' ';
        nWritten += indent * spacing + 1;
    }

    void putStr(const(char)[] s)
    {
        outputChain.ensureElems(nWritten + s.length);
        outputChain.window[nWritten .. nWritten + s.length] = s;
        nWritten += s.length;
    }

    bool isValidJSONNumber(string str) {
        static auto numberRegex = regex(r"^-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?$");
        return !str.matchFirst(numberRegex).empty;
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
        if (!canAddValue())
            throw new JSONIopipeException("beginObject not allowed in current state");
        
        pushContainer(true);  // object
        state = State.First;  // just started

        ++spacing;
        putStr("{");
        putIndent();
    }

    void endObject() {
        if (!canCloseObject())
            throw new JSONIopipeException("endObject not allowed in current state");
        
        --spacing;
        putIndent();
        putStr("}");

        popContainer(); // State.End or State.Member
    }

    void beginArray() {
        if (!canAddValue())
            throw new JSONIopipeException("beginArray not allowed in current state");

        pushContainer(false); // array
        state = State.First;

        ++spacing;
        putStr("[");
        putIndent();
    }

    void endArray() {
        if (!canCloseArray())
            throw new JSONIopipeException("endArray not allowed in current state");

        --spacing;
        putIndent();
        putStr("]");

        popContainer();
    }

    // call before each element
    void beginArrayValue()
    {
        if (!canAddArrayMember())
            throw new JSONIopipeException("beginArrayValue not allowed in current state");
    }

    void addMember(T)(T key) {
        addMember(key, memberOptions);
    }

    // First/Member -> Value(before string key) -> Member(after string key) -> Value(before value) -> Member(after value) ...
    void addMember(T)(T key, MemberOptions options) {
        
        if (!canAddObjMember())
            throw new JSONIopipeException("addMember not allowed in current state");

        // prepare for a key string
        state = State.Value;

        static if (is(typeof(key) == string))
        {
            if (options.escapeKey)
            {
                beginString(options.quoteChar);
                addStringData(key);
                endString();
            }
            else
            {
                beginString(options.quoteChar);
                addStringData!(false, false)(key);
                endString();
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

        // prepare for a key's value
        state = State.Value;
    }

    void beginString(char quoteChar = '"') {
        if (!canAddValue())
            throw new JSONIopipeException("beginString not allowed in current state");
        
        if (currentQuoteChar != char.init)
          throw new JSONIopipeException("nested beginString not allowed");

        if (quoteChar != '"' && quoteChar != '\'')
            throw new JSONIopipeException("Invalid quote character");

        currentQuoteChar = quoteChar;
        putStr((&quoteChar)[0 .. 1]);
    }

    // will automatically escape string data as needed.
    void addStringData(bool validate = true, bool addEscapes = true, T)(T value) {
        if (currentQuoteChar == char.init)
            throw new JSONIopipeException("cannot add string data outside of beginString/endString");

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
            
            // if we started with a single quote, also escape any remaining '
            if (currentQuoteChar == '\'') {
                import std.array : appender;
                auto app = appender!string();
                foreach (char c; escaped) {
                    if (c == '\'') {
                        app.put('\\');
                        app.put('\'');
                    } else {
                        app.put(c);
                    }
                }
                putStr(app.data);
            } else {
                // normal JSON style (double quotes only)
                putStr(escaped);
            }
        } else {
            putStr(value);
        }
    }

    void endString() {
        if (currentQuoteChar == char.init)
          throw new JSONIopipeException("cannot endString without a matching beginString");
        
        putStr((&currentQuoteChar)[0 .. 1]);
        currentQuoteChar = char.init;

        if (stackLen == 0) {
            state = State.End;
            putIndent();
        } else
        // If it's at the end of an array or an object,
        // endArray() and endObject() will set the state appropriately
            state = State.Member;
    }

    // allow numeric types, or strings that are validated to be JSON numbers
    void addNumericData(T)(T value, string formatStr = "%s") {
        if (!canAddValue())
            throw new JSONIopipeException("addNumericData not allowed in current state");
        
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
        if (!canAddValue())
            throw new JSONIopipeException("addKeywordValue not allowed in current state");

        putStr(value);

        if (stackLen == 0)
            state = State.End;
        else
            state = State.Member;
    }

    void flushWritten() {
        outputChain.release(nWritten);
        nWritten = 0;
    }

    auto chain() {
        return outputChain;
    }

}

private struct TestChain
{
    char[] buf;

    @property char[] window()
    {
        return buf;
    }

    // Grow buffer when formatter calls ensureElems via free function
    size_t extend(size_t elements)
    {
        import core.memory : GC;

        // extend is called by ensureElems until window.length >= requested
        auto oldLen = buf.length;
        auto newLen = oldLen + elements;
        auto p = cast(char*)GC.malloc(newLen * char.sizeof);
        auto newBuf = p[0 .. newLen];

        if (oldLen)
            newBuf[0 .. oldLen] = buf[];

        buf = newBuf;
        return elements;
    }

    void release(size_t /*elements*/)
    {
        // formatter calls release(nWritten); for tests we can ignore it
    }
}

unittest
{
    import iopipe.traits : isIopipe;
    import iopipe.bufpipe : ensureElems;
    import std.string : strip;
    import std.stdio : writeln;

    TestChain chain;
    static assert(isIopipe!TestChain);

    auto fmt = JSONFormatter!(TestChain)(chain);

    fmt.beginObject();
    fmt.addMember("a");
    fmt.addNumericData(1);
    fmt.endObject();
    fmt.flushWritten();

    auto s = cast(string)chain.buf.strip;
    assert(s == "{\n    \"a\": 1\n}");
}

unittest
{
    // Simple array: [1,2,3]
    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);

    fmt.beginArray();
    fmt.beginArrayValue();
    fmt.addNumericData(1);
    fmt.beginArrayValue();
    fmt.addNumericData(2);
    fmt.beginArrayValue();
    fmt.addNumericData(3);
    fmt.endArray();
    fmt.flushWritten();

    import std.string : strip;
    auto s = cast(string)chain.buf.strip;
    assert(s == "[\n    1,\n    2,\n    3\n]");
}

unittest
{
    // String with escapes and single‑quote behavior
    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);

    // double‑quoted: inner " and \ should be escaped
    fmt.beginString('"');
    fmt.addStringData(`He said: "hi" \ test`);
    fmt.endString();
    fmt.flushWritten();

    import std.string : strip;
    auto s = cast(string)chain.buf.strip;
    assert(s == `"He said: \"hi\" \\ test"`);

    // reset chain, now test single‑quoted: inner ' must be escaped as \'
    chain.buf.length = 0;
    auto fmt2 = JSONFormatter!(TestChain)(chain);

    fmt2.beginString('\'');
    fmt2.addStringData(`It's ok`);
    fmt2.endString();
    fmt2.flushWritten();

    s = cast(string)chain.buf.strip;
    assert(s == `'It\'s ok'`);
}

unittest
{
    // Keywords: null/true/false
    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);

    fmt.beginArray();
    fmt.beginArrayValue();
    fmt.addKeywordValue(KeywordValue.Null);
    fmt.beginArrayValue();
    fmt.addKeywordValue(KeywordValue.True);
    fmt.beginArrayValue();
    fmt.addKeywordValue(KeywordValue.False);
    fmt.endArray();
    fmt.flushWritten();

    import std.string : strip;
    auto s = cast(string)chain.buf.strip;
    assert(s == "[\n    null,\n    true,\n    false\n]");
}

unittest
{
    // ERROR: endArray without beginArray
    import std.exception : assertThrown;

    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);

    assertThrown!JSONIopipeException(fmt.endArray());
}

unittest
{
    // ERROR: addMember when not inside object
    import std.exception : assertThrown;

    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);

    assertThrown!JSONIopipeException(fmt.addMember("a"));
}

unittest
{
    // ERROR: add key string directily without in addMember
    import std.exception : assertThrown;

    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);
    fmt.beginObject();

    assertThrown!JSONIopipeException(fmt.beginString());
}

unittest
{
    // ERROR: addArrayValue when not inside array
    import std.exception : assertThrown;

    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);

    assertThrown!JSONIopipeException(fmt.beginArrayValue());
}

unittest
{
    // ERROR: nested beginString
    import std.exception : assertThrown;

    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);

    fmt.beginString();
    assertThrown!JSONIopipeException(fmt.beginString());
}

unittest
{
    // ERROR: addStringData without beginString
    import std.exception : assertThrown;

    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);

    assertThrown!JSONIopipeException(fmt.addStringData("oops"));
}

unittest
{
    // ERROR: endString without beginString
    import std.exception : assertThrown;

    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);

    assertThrown!JSONIopipeException(fmt.endString());
}

unittest
{
    // ERROR: invalid JSON number string
    import std.exception : assertThrown;

    TestChain chain;
    auto fmt = JSONFormatter!(TestChain)(chain);

    assertThrown!JSONIopipeException(fmt.addNumericData("01"));
}