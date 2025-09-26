/**
 * Serialize and deserialize arbitrary objects to/ from json
 *
 * Both $(LREF serialize) and $(LREF deserialize) support 3 levels of detail.
 * If the template argument is just a type, they try to serialize it as
 * faithfully as possible.
 * For structs and classes, this means treating the fieldnames as keys in
 * a JSON object.
 * The type JSONValue can be used if dynamic objects are needed.
 *
 * For more controls, the various UDAs defined here can be used on compound
 * types or their fields to customize the behaviour of this module in a
 * limited way.
 *
 * If that is not enough, structs/ classes can define toJSON or a static
 * fromJSON method and take full control over how they are serialized.
 * For toJSON, $(LREF serializeAllMembers) might be useful. fromJSON will
 * have to use $(LREF iopipe,json,parser).
 */
module iopipe.json.serialize;
import iopipe.json.parser;
import iopipe.json.dom;
public import iopipe.json.common;
import iopipe.traits;
import iopipe.bufpipe;
import std.range.primitives;

import std.traits;
import std.meta;
import std.typecons : Nullable;
import std.conv;
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

struct JSONFormatter(Chain) {
private:
    Chain* outputChain;
    int spacing = 0;
    int indent = 4;
    MemberOptions memberOptions;

    bool isFirstInObj;
    bool isFirstInArray;
    size_t nWritten = 0;

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

    private bool isValidJSONNumber(string str) {
        import std.regex;
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
        isFirstInObj = true;
        ++spacing;
        putStr("{");
        putIndent();
    }

    void endObject() {
        --spacing;
        putIndent();
        putStr("}");
    }

    void beginArray() {
        isFirstInArray = true;
        ++spacing;
        putStr("[");
        putIndent();
    }

    void endArray() {
        --spacing;
        putIndent();
        putStr("]");
    }

    void addMember(T)(T key) {
        addMember(key, memberOptions);
    }

    void addMember(T)(T key, MemberOptions options) {
        
        if (key.length == 0 ) {
            // use for array elements
            if (!isFirstInArray) {
                putStr(", ");
                putIndent();
            }
            isFirstInArray = false;
            return;
        }

        if (!isFirstInObj) {
            putStr(",");
            putIndent();
        }

        static if (is(typeof(key) == string)) {
            if (options.escapeKey) {
                beginString(options.quoteChar);
                addStringData(key);
                endString(options.quoteChar);
            } else {
                beginString(options.quoteChar);
                addStringData!(false, false)(key);
                endString(options.quoteChar);
            }
        } else {
            putStr(to!string(key));
        }
        // Handle colon spacing based on enum value
        final switch (options.colonSpacing) {
            case MemberOptions.ColonSpacing.none:
                putStr(":");           // "key":"value"
                break;
            case MemberOptions.ColonSpacing.after:
                putStr(": ");          // "key": "value"  
                break;
            case MemberOptions.ColonSpacing.both:
                putStr(" : ");         // "key" : "value"
                break;
        }

        isFirstInObj = false;
    }

    void beginString(char quoteChar = '"') {
        putStr([quoteChar]);
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
            import std.algorithm.iteration: substitute;
            import std.conv: to;
            auto escaped = value.substitute!(jsonEscapeSubstitutions!()).to!string;
            putStr(escaped);
        } else {
            putStr(value);
        }
    }


    void endString(char quoteChar = '"') {
        putStr([quoteChar]);
    }

    // allow numeric types, or strings that are validated to be JSON numbers
    void addNumericData(T)(T value, string formatStr = "%s") {
        static if (is(T == string)) {
            if (!isValidJSONNumber(value)) {
                throw new JSONIopipeException(format("Invalid JSON number: %s", value));
            }
            putStr(value);
        } else {
            import std.format : formattedWrite;
            formattedWrite(&putStr, formatStr, value);
        }
    }

    // null, true, false, inf, etc.
    void addKeywordValue(KeywordValue value) {
        putStr(value);
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
            import std.algorithm.searching : startsWith, endsWith;
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


struct DefaultDeserializationPolicy(bool caseInsensitive = false) {
    ReleasePolicy relPol = ReleasePolicy.afterMembers; // default policy
    int maxDepthAvailable = 64;

    this(ReleasePolicy relPol) {
        this.relPol = relPol;
    }

    this(int maxDepthAvailable) {
        this.maxDepthAvailable = maxDepthAvailable;
    }

    this(ReleasePolicy relPol, int maxDepthAvailable) {
        this.relPol = relPol;
        this.maxDepthAvailable = maxDepthAvailable;
    }

    auto onObjectBegin(JT, T)(ref JT tokenizer, ref T item) {
        if (--maxDepthAvailable < 0) {
            throw new JSONIopipeException("Maximum parse depth exceeded");
        }
        return .onObjectBegin(this, tokenizer, item);
    }

    void onField(JT, T, Element, C)(
        ref JT tokenizer, 
        ref T item, 
        Element key,
        ref C context
    ) {
        .onField!caseInsensitive(this, tokenizer, item, key, context);
        if(relPol == ReleasePolicy.afterMembers) {
            tokenizer.flushCache();
        }
    }

    void onObjectEnd(JT, T, C)(ref JT tokenizer, ref T item, ref C context) {
        .onObjectEnd(this, tokenizer, item, context);
        ++maxDepthAvailable;
    }
}


unittest
{

    static struct S {
        string userName;
        @alternateName("pet") string dogName;
        int age;
    }
    auto jsonStr = `{"username": "Alice", "PET": "Buddy", "AGE": 30}`;
    auto policy = DefaultDeserializationPolicy!true();
    auto s = deserialize!S(jsonStr, policy);
    assert(s.userName == "Alice");
    assert(s.dogName == "Buddy");
    assert(s.age == 30);
}

unittest
{
    // Test maxDepth on deserializing a struct with nested objects in extra members.

    static struct T {
        string name;
	      @extras JSONValue!string stuff;
    }


    auto jsonStr = `{
        "name": "valid",
        "a": "another string",
        "b": 2,
        "c": 8.5,
        "pet1": {
            "name": "Fido",
            "age": 5
        },
        "pet2": {
            "name": "Rex",
            "age": 3
        }
      }`;

    auto policy1 = DefaultDeserializationPolicy!false(1); // Set max depth to 1
    import std.exception;
    // This should throw an exception because the depth exceeds 1
    assertThrown!JSONIopipeException(
        deserialize!T(jsonStr, policy1)
    );

    // Now deserialize with a higher max depth
    auto policy2 = DefaultDeserializationPolicy!false(2); // Set max depth to 2
    auto t = deserialize!T(jsonStr, policy2);
    assert(t.name == "valid");
    assert(t.stuff.object["a"].type == JSONType.String);
    assert(t.stuff.object["a"].str == "another string");
    assert(t.stuff.object["b"].type == JSONType.Integer);
    assert(t.stuff.object["b"].integer == 2);
    assert(t.stuff.object["c"].type == JSONType.Floating);
    assert(t.stuff.object["c"].floating == 8.5);
    assert(t.stuff.object["pet1"].type == JSONType.Obj);
    assert(t.stuff.object["pet1"].object["name"].type == JSONType.String);
    assert(t.stuff.object["pet1"].object["name"].str == "Fido");
    assert(t.stuff.object["pet1"].object["age"].type == JSONType.Integer);
    assert(t.stuff.object["pet1"].object["age"].integer == 5);
    assert(t.stuff.object["pet2"].type == JSONType.Obj);
    assert(t.stuff.object["pet2"].object["name"].type == JSONType.String);
    assert(t.stuff.object["pet2"].object["name"].str == "Rex");
    assert(t.stuff.object["pet2"].object["age"].type == JSONType.Integer);
    assert(t.stuff.object["pet2"].object["age"].integer == 3);
}

// shim for policies that do not specify a release policy
private ReleasePolicy relPol(P)(ref P policy) => ReleasePolicy.afterMembers;

auto onObjectBegin(P, JT, T)(ref P policy, ref JT tokenizer, ref T item) {
    static if(is(T == V[K], V, K)) {
        return ubyte.init; // no context for AAs.
    }
    else {
        alias members = SerializableMembers!T;
        bool[members.length] visited;
        // Pre-mark optional fields as visited
        static foreach(idx, m; members) {
            static if(hasUDA!(__traits(getMember, T, m), optional)) {
                visited[idx] = true;
            }
            static if(hasUDA!(__traits(getMember, T, m), extras))
            {
                // this is the extras member, it holds any extra data that was not
                // specified as a member.
                static assert(is(typeof(__traits(getMember, T, m)) == JSONValue!S, S));
                // initialize it for use
                __traits(getMember, item, m).type = JSONType.Obj;
                __traits(getMember, item, m).object = null;
                // extras is always optional.
                visited[idx] = true;
            }
        }

        return visited;
    }
}

auto onArrayBegin(P, JT, T)(ref P policy, ref JT tokenizer, ref T item)
{
    return ubyte.init;
}

void onField(bool caseInsensitive = false, P, JT, T, Element, C)(ref P policy, ref JT tokenizer, ref T item, Element key, ref C context) {
    static if(is(T == V[K], V, K)) {
        // convert key into K, forcing a copy. We must copy because this key needs to exist forever.
        auto k = extractString!(K, true)(key);
        // extract value
        V v;
        policy.deserializeImpl(tokenizer, v);
        // store in the AA.
        item[k] = v;
    }
    else {
        static assert(is(C == bool[N], size_t N) && N == SerializableMembers!T.length);
        alias members = SerializableMembers!T;
        alias ignoredMembers = AllIgnoredMembers!T;

        static foreach(idx, m; members)
        {
            static if(hasUDA!(__traits(getMember, T, m), extras))
            {
                // this is the extras member, it holds any extra data that was not
                // specified as a member.
                static assert(is(typeof(__traits(getMember, T, m)) == JSONValue!S, S));
                enum extrasMember = m;
            }
        }

        // Check each member to see if it matches
        switch(key.data)
        {
            static foreach(i, memberName; members) {
                { // Add a block scope to contain each declaration, avoiding duplicate jsonName
                  // declarations in the foreach loop.
                    static if(!hasUDA!(__traits(getMember, T, memberName), extras)) {
                        static if(hasUDA!(__traits(getMember, T, memberName), alternateName)) {
                            enum jsonName = getUDAs!(__traits(getMember, T, memberName), alternateName)[0].name;
                        }
                        else {
                            enum jsonName = memberName;
                        }

                        case jsonName:
                        // Choose appropriate deserialization method based on member type
                        policy.deserializeImpl(tokenizer, __traits(getMember, item, memberName));
                        context[i] = true; // Mark as visited
                        return;  // ← IMPORTANT: Returns immediately!
                    }
                }
            }

            enum ignoreExtras = !is(typeof(extrasMember)) && hasUDA!(T, .ignoreExtras);


            static if(ignoredMembers.length > 0)
            {
                static foreach(m; ignoredMembers)
                {
                    static foreach(s; m.ignoredMembers)
                    {
                        case s:
                    }
                }
                // ignored members are ignored if they show up
                tokenizer.skipItem();
                break;
            }

            default:
            static if(caseInsensitive)
            {
                // Case-insensitive comparison
                import std.string : icmp;
                auto keyStr = key.data;
                static foreach(i, memberName; members) {
                    {
                        static if(hasUDA!(__traits(getMember, T, memberName), alternateName)) {
                            enum jsonName = getUDAs!(__traits(getMember, T, memberName), alternateName)[0].name;
                        }
                        else {
                            enum jsonName = memberName;
                        }
                        if (icmp(keyStr, jsonName) == 0) {
                            goto case jsonName;
                        }
                    }
                }
            }
            static if(ignoreExtras)
            {
                tokenizer.skipItem();
                return; // Ignore unknown fields
            }

            static if(is(typeof(extrasMember)) && is(typeof(__traits(getMember, item, extrasMember)) == JSONValue!SType, SType))
            {
                // recurse with the JSONValue as the object. Note the change in
                // context, as the AA field deserializer takes a ubyte.
                ubyte _fakeContext;
                onField!false(policy, tokenizer, __traits(getMember, item, extrasMember).object, key, _fakeContext);
            } else {
                // If we get here, it's truly an unknown field
                throw new JSONIopipeException(format("No member named '%s' in type `%s`", key.data, T.stringof));
            }
        }
    }
}

void onArrayElement(P, JT, T)(ref P policy, ref JT tokenizer, ref T item, size_t idx, ubyte unused)
{
    static if(__traits(isStaticArray, T))
    {
        // Check bounds explicitly
        if (idx >= T.length) {
            throw new JSONIopipeException(format("Array index %s out of bounds for static array of length %s", idx, T.length));
        }

        // Deserialize the item at the given index
        policy.deserializeImpl(tokenizer, item[idx]);
    }
    else // output range? support appender only for now
    {
        typeof(item[][0]) newElem;
        // Deserialize the item at the given index
        policy.deserializeImpl(tokenizer, newElem);

        // append to the output range
        item.put(newElem);
    }
}

void onObjectEnd(P, JT, T, C)(ref P policy, ref JT tokenizer, ref T item, ref C context) {
    static if(is(T == V[K], V, K)) {
    }
    else {
        static assert(is(C == bool[N], size_t N) && N == SerializableMembers!T.length);
        alias members = SerializableMembers!T;
        // ensure all members visited
        static if(members.length)
        {
            import std.algorithm : canFind, map, filter;
            if(context[].canFind(false))
            {
                // this is a bit ugly, but gives a nicer message.
                static immutable marr = [members];
                import std.range : enumerate;
                throw new JSONIopipeException(format("The following members of `%s` were not specified: `%-(%s` `%)`", T.stringof, context[].enumerate.filter!(a => !a[1]).map!(a => marr[a[0]])));
            }
        }
    }
}

void onArrayEnd(P, JT, T)(ref P policy, ref JT tokenizer, ref T item, size_t length, ubyte unused)
{
    static if(__traits(isStaticArray, T))
    {
        if(T.length != length) // didn't serialize all elements
        {
            throw new JSONIopipeException(format("Expected array of length %s for type `%s`, but only got length %s", T.length, T.stringof, length));
        }
    }
}

// define some UDAs to affect serialization
struct IgnoredMembers { string[] ignoredMembers; }

/**
 * This UDA will cause the specified member to be substituted in for
 * serializing a struct or class type.
 *
 * See_Also:
 *     $(LREF ignoreExtras)
 */
enum serializeAs;

///

unittest {
    static struct T {
        @serializeAs string s;
        int x;
    }

    T t = deserialize!(T)(`"hi"`);
    assert(t.s == "hi");
    assert(t.x == int.init);

    assert(t.serialize == `"hi"`);

    import std.exception;
    // This would only work with @ignoreExtras
    assert(
		    deserialize!(T)(`{"s": "invalid", "x": 1}`).collectExceptionMsg
		    == "Parsing string: expected String, got ObjectStart"
	    );
}


/**
 * UDA: Ignore the member when serializing a struct or class.
 */
enum ignore;

/**
 * UDA: If this member is not present when deserializing a type, do not consider
 * it an error.
 */
enum optional;

/**
 * UDA: if on an enum, this serializes the enum as the base type instead of the
 * enum name (default).
 */
enum enumBaseType;

/**
 * enum Y gets serialized as an int here
 */
unittest {
    enum X { a, b, c }
    @enumBaseType enum Y: int {a, b, c,}
    static struct S { X x; Y y; }
    auto s = S(X.a, Y.a);
    auto sstr = s.serialize;
    assert(sstr == `{"x" : "a", "y" : 0}`);
    auto s2 = sstr.deserialize!S;
    assert(s2.x == X.a && s2.y == Y.a);
}

/**
 * This UDA, when applied to a JSONValue type member, will consume all items
 * that do not have a member name in that aggregate. Only one of these should
 * be in a type.
 */
enum extras;

///
unittest {
    static struct T {
        string name;
	@extras JSONValue!string stuff;
    }

    T t = deserialize!(T)(`{"name": "valid", "a": "another string", "b": 2, "c": 8.5}`);
    assert(t.name == "valid");
    assert(t.stuff.object["a"].type == JSONType.String);
    assert(t.stuff.object["a"].str == "another string");
    assert(t.stuff.object["b"].type == JSONType.Integer);
    assert(t.stuff.object["b"].integer == 2);
    assert(t.stuff.object["c"].type == JSONType.Floating);
    assert(t.stuff.object["c"].floating == 8.5);
}

/**
 * UDA: Use a different name in json than in the D struct
 */
struct alternateName
{
    string name;
}

/**
 * The alternate name is not optional
 */
unittest {
    import std.exception;
    static struct T {
        @alternateName("alternate") string name;
    }

    T t = deserialize!(T)(`{"alternate": "valid"}`);
    assert(t.name == "valid");
    assert(t.serialize == `{"alternate" : "valid"}`);
    deserialize!(T)(`{"name": "invalid"}`).assertThrown;
}

/**
 * Apply this to a struct or class for members of a JSON object that you want
 * to be ignored. For example, metadata that aids in deciding a concrete class
 * type.
 */
IgnoredMembers ignoredMembers(string[] m...) { return IgnoredMembers(m.dup); }

/**
 * When applied to a struct or class, all members that are not present in the
 * struct will be ignored.
 * If an extras member exists, then this has no effect.
 */
enum ignoreExtras;

///
unittest {
    import std.exception;

    static struct S {
        string s;
        int x;
    }

    @ignoreExtras
    static struct T {
        string s;
        int x;
    }

    deserialize!(S)(`{"s" : "hi", "x": 1, "extra": "errors"}`).assertThrown!JSONIopipeException;
    T t = deserialize!(T)(`{"s" : "hi", "x": 1, "extra": "ignored"}`);
    assert(t.s == "hi");
    assert(t.x == 1);

    assert(t.serialize == `{"s" : "hi", "x" : 1}`);

    deserialize!S(`{"s" : "hi", "x": 1, "extra": "errors"}`).assertThrown!JSONIopipeException;
    auto tt = deserialize!T(`{"s" : "hi", "x": 1, "extra": "errors"}`);
    assert(tt.s == "hi");
    assert(tt.x == 1);
    assert(tt.serialize == `{"s" : "hi", "x" : 1}`);

}

/**
 * Expect the given JSONItem to be a specific token.
 * Parameters:
 *      msg: Optional error message in case of mismatch
 * Throws:
 *	JSONIopipeException on violation.
 * Returns:
 *      the input item
 */
auto jsonExpect(Item)(Item item, JSONToken expectedToken, string msg="Error", string file = __FILE__, size_t line = __LINE__) @safe
{
    if(item.token != expectedToken)
    {
        //import std.stdio;
        //import std.algorithm;
        //writeln(item.owner.window.map!(e => e.data));
        throw new JSONIopipeException(format("%s: expected %s, got %s", msg, expectedToken, item.token), file, line);
    }
    return item;
}

void deserializeImpl(P, T, JT)(ref P policy, ref JT tokenizer, ref T item) if (is(T == enum))
{
    // enums are special, we can serialize them based on the enum name, or the
    // base type.
    static if(hasUDA!(T, enumBaseType))
    {
        policy.deserializeImpl(tokenizer, *(cast(OriginalType!T*)&item));
    }
    else
    {
        // convert to the enum via the string name
        auto jsonItem = tokenizer.nextSignificant
            .jsonExpect(JSONToken.String, "Parsing " ~ T.stringof);
        item = jsonItem.data.to!T;
    }
}

// Keys currently must be some string type. Future versions may allow
// conversion from character data.
void deserializeImpl(P, T, JT)(ref P policy, ref JT tokenizer, ref T item) if (is(T == V[K], V, K))
{
    deserializeObject(policy, tokenizer, item);
}

unittest
{
    // validate deserializing AAs
    assert(deserialize!(int[string])(`{"a": 1, "b": 2}`)  == ["a" : 1, "b" : 2]);
    assert(deserialize!(int[wstring])(`{"a": 1, "b": 2}`) == ["a"w : 1, "b" : 2]);
    assert(deserialize!(int[dstring])(`{"a": 1, "b": 2}`) == ["a"d : 1, "b" : 2]);
}

void deserializeImpl(P, T, JT)(ref P policy, ref JT tokenizer, ref T item) if (!is(T == enum) && isNumeric!T)
{
    auto jsonItem = tokenizer.nextSignificant
        .jsonExpect(JSONToken.Number, "Parsing " ~ T.stringof);

    auto str = jsonItem.data;
    static if(isIntegral!T)
    {
        if(jsonItem.hint != JSONParseHint.Int &&
                !(tokenizer.config.JSON5 && jsonItem.hint == JSONParseHint.Hex))
        {
            throw new JSONIopipeException(format("Cannot parse `%s` from '%s'", T.stringof, jsonItem.data));
        }
    }
    else
    {
        static if(tokenizer.config.JSON5)
        {
            // if it's +/- infinity, phobos doesn't parse this properly.
            if(jsonItem.hint == JSONParseHint.Infinity)
            {
                auto window = jsonItem.data;
                if(window[0] == '-')
                    item = -T.infinity;
                else
                    item = T.infinity;
                return;
            }
        }
    }

    // get the string from the buffer that contains the number
    auto window = jsonItem.data;
    static if(isIntegral!T && tokenizer.config.JSON5)
    {
        if(jsonItem.hint == JSONParseHint.Hex)
        {
            // have to do this somewhat manually, as there is no function to
            // properly parse the "0x" in phobos
            int sign = 1;
            switch(window[0])
            {
                case '0':
                    window = window[2 .. $];
                    break;
                case '-':
                    sign = -1;
                    window = window[3 .. $];
                    break;
                case '+':
                    window = window[3 .. $];
                    break;
                default:
                    assert(false, "invalid hex number detected! " ~ window);
            }
            item = window.parse!T(16) * sign;
        }
        else
            item = window.parse!T;
    }
    else
        item = window.parse!T;
    if(!window.empty)
    {
        throw new JSONIopipeException(format("Parsing of `%s` from source '%s' failed near '%s'", T.stringof, jsonItem.data, window));
    }
}

void deserializeImpl(P, T, JT)(ref P policy, ref JT tokenizer, ref T item) if (is(T == bool))
{
    auto jsonItem = tokenizer.nextSignificant;
    if(jsonItem.token == JSONToken.True)
    {
        item = true;
    }
    else if(jsonItem.token == JSONToken.False)
    {
        item = false;
    }
    else
    {
        throw new JSONIopipeException(format("Parsing bool: expected %s or %s , but got %s", JSONToken.True, JSONToken.False, jsonItem.token));
    }
}

void deserializeImpl(P, T, JT)(ref P policy, ref JT tokenizer, ref T item) if (isSomeString!T)
{
    // Use phobos `to`, we want to duplicate the string if necessary.

    auto jsonItem = tokenizer.nextSignificant
        .jsonExpect(JSONToken.String, "Parsing " ~ T.stringof);

    // this should not fail unless the data is non-unicode
    item = extractString!T(jsonItem);
}

private template SerializableMembers(T)
{
    enum WithoutIgnore(string s) = !hasUDA!(__traits(getMember, T, s), ignore);
    static if(is(T == struct))
        enum SerializableMembers = Filter!(WithoutIgnore, FieldNameTuple!T);
    else
        enum SerializableMembers = Filter!(WithoutIgnore, staticMap!(FieldNameTuple, T, BaseClassesTuple!T));
}

private template AllIgnoredMembers(T)
{
    static if(is(T == struct))
        enum AllIgnoredMembers = getUDAs!(T, IgnoredMembers);
    else
        enum AllIgnoredMembers = staticMap!(ApplyRight!(getUDAs, IgnoredMembers), T, BaseClassesTuple!T);
}

void deserializeImpl(P, T, JT)(ref P policy, ref JT tokenizer, ref T item) if (is(T == struct) && __traits(hasMember, T, "fromJSON"))
{
    enum isRef(string s) = s == "ref";
    static if(__traits(compiles, T.fromJSON(tokenizer, policy))) {
        // use policy object
        static assert(anySatisfy!(isRef, __traits(getParameterStorageClasses, item.fromJSON!(P, JT), 0)),
            "fromJSON must take tokenizer by ref, otherwise it can't advance the read position.");
        item = T.fromJSON(tokenizer, policy);
    } else static if(__traits(compiles, T.fromJSON(tokenizer, ReleasePolicy.init))) {
        // use ReleasePolicy
         static assert(anySatisfy!(isRef, __traits(getParameterStorageClasses, item.fromJSON!JT, 0)),
            "fromJSON must take tokenizer by ref, otherwise it can't advance the read position.");
        item = T.fromJSON(tokenizer, policy.relPol);
    } else static if(__traits(compiles, T.fromJSON(tokenizer))) {
        // use one parameter only
         static assert(anySatisfy!(isRef, __traits(getParameterStorageClasses, item.fromJSON!JT, 0)),
            "fromJSON must take tokenizer by ref, otherwise it can't advance the read position.");
        item = T.fromJSON(tokenizer);
    } else {
        // This will fail to compile. Try to nudge the user to use a policy
        item = T.fromJSON(tokenizer, policy);
    }
}

void deserializeImpl(P, T, JT)(ref P policy, ref JT tokenizer, ref T item) if (isInstanceOf!(JSONValue, T))
{
    auto token = tokenizer.peekSignificant();
    with(JSONToken) switch (token)
    {
        case ObjectStart:
            item.type = JSONType.Obj;
            item.object = null;
            policy.deserializeImpl(tokenizer, item.object);
            break;
        case ArrayStart:
            item.type = JSONType.Array;
            item.array = null;
            policy.deserializeImpl(tokenizer, item.array);
            break;
        case String:
        case Symbol:
            item.type = JSONType.String;
            policy.deserializeImpl(tokenizer, item.str);
            break;
        case Number:
            auto jsonItem = tokenizer.next;
            tokenizer.jumpBack(1);
            if(jsonItem.hint == JSONParseHint.Int) {
                item.type = JSONType.Integer;
                policy.deserializeImpl(tokenizer, item.integer);
            } else {
                item.type = JSONType.Floating;
                policy.deserializeImpl(tokenizer, item.floating);
            }
            break;
        case True:
        case False:
            item.type = JSONType.Bool;
            policy.deserializeImpl(tokenizer, item.boolean);
            break;
        case Null:
            item.type = JSONType.Null;
            tokenizer.nextSignificant(); // consume the null
            break;
        default:
            throw new JSONIopipeException(format("Cannot deserialize JSONValue from %s", token));
    }
}

unittest {
    // Test emptyObject first
    auto jsonStr1 = `{"emptyObject": {}}`;
    auto jv1 = deserialize!(JSONValue!string)(jsonStr1);
    
    assert(jv1.type == JSONType.Obj);
    assert("emptyObject" in jv1.object);
    
    auto emptyObj = jv1.object["emptyObject"];
    assert(emptyObj.type == JSONType.Obj);
    assert(emptyObj.object.length == 0);

    auto jsonStr2 = `{"emptyArray": []}`;
    auto jv2 = deserialize!(JSONValue!string)(jsonStr2);
    
    assert(jv2.type == JSONType.Obj);
    assert("emptyArray" in jv2.object);
    
    auto emptyArr = jv2.object["emptyArray"];
    assert(emptyArr.type == JSONType.Array);
    assert(emptyArr.array.length == 0);
}



void deserializeAllMembers(T, JT)(ref JT tokenizer, ref T item, ReleasePolicy relPol)
{
    // expect an object in JSON. We want to deserialize the JSON data
    alias members = SerializableMembers!T;
    alias ignoredMembers = AllIgnoredMembers!T;

    // TODO use bit array instead and core.bitop
    //size_t[(members.length + (size_t.sizeof * 8 - 1)) / size_t.sizeof / 8] visited;
    bool[members.length] visited;

    // any members that are optional, mark as already visited
    static foreach(idx, m; members)
    {
        static if(hasUDA!(__traits(getMember, T, m), optional))
            visited[idx] = true;
        static if(hasUDA!(__traits(getMember, T, m), extras))
        {
            // this is the extras member, it holds any extra data that was not
            // specified as a member.
            static assert(is(typeof(__traits(getMember, T, m)) == JSONValue!S, S));
            enum extrasMember = m;
            // initialize it for use
            __traits(getMember, item, m).type = JSONType.Obj;
            __traits(getMember, item, m).object = null;
            // extras is always optional.
            visited[idx] = true;
        }
    }

    enum ignoreExtras = !is(typeof(extrasMember)) && hasUDA!(T, .ignoreExtras);

    auto jsonItem = tokenizer.nextSignificant
        .jsonExpect(JSONToken.ObjectStart, "Parsing " ~ T.stringof);

    // look at each string, then parse the given values
    jsonItem = tokenizer.nextSignificant();
    while(jsonItem.token != JSONToken.ObjectEnd)
    {
        if(jsonItem.token == JSONToken.Comma)
        {
            jsonItem = tokenizer.nextSignificant();
            static if(tokenizer.config.JSON5)
            {
                // JSON5 allows trailing commas
                if(jsonItem.token == JSONToken.ObjectEnd)
                    break;
            }
        }

        static if(tokenizer.config.JSON5)
        {
            if(jsonItem.token != JSONToken.String && jsonItem.token != JSONToken.Symbol)
                jsonExpect(jsonItem, JSONToken.String, "Expecting member name of " ~ T.stringof);
        }
        else
            jsonExpect(jsonItem, JSONToken.String, "Expecting member name of " ~ T.stringof);

        // Have to call nameItem.data() on each access, as the returned string would get invalidated by calls to tokenizer.next()
        auto nameItem = jsonItem;
        scope name = () => nameItem.data(tokenizer.chain);
        // TODO: handle names with unicode escapes

        jsonItem = tokenizer.nextSignificant()
            .jsonExpect(JSONToken.Colon, "Expecting colon when parsing " ~ T.stringof);
OBJ_MEMBER_SWITCH:
        switch(name())
        {
            static foreach(i, m; members)
                static if(!hasUDA!(__traits(getMember, item, m), extras))
                {{
                    static if(hasUDA!(__traits(getMember, item, m), alternateName))
                    {
                        enum jsonName = getUDAs!(__traits(getMember, item, m), alternateName)[0].name;
                    }
                    else
                        enum jsonName = m;
                case jsonName:
                    auto policy = DefaultDeserializationPolicy!false(relPol);
                    policy.deserializeImpl(tokenizer, __traits(getMember, item, m));
                    visited[i] = true;
                    break OBJ_MEMBER_SWITCH;
                }}

            static if(ignoredMembers.length > 0)
            {
                static foreach(m; ignoredMembers)
                {
                    static foreach(s; m.ignoredMembers)
                    {
                    case s:
                    }
                }
                // ignored members are ignored if they show up
                tokenizer.skipItem();
                break OBJ_MEMBER_SWITCH;
            }

        default:
            static if(ignoreExtras)
            {
                tokenizer.skipItem();
                break OBJ_MEMBER_SWITCH;
            }
            else static if(is(typeof(extrasMember)) && is(typeof(__traits(getMember, item, extrasMember)) == JSONValue!SType, SType))
            {{
                // any extras should be put in here
                JSONValue!SType newItem;
                // Need to save name() before deserializeImpl potentially calls release and invalidates the window
                auto key = name().to!(immutable(SType));
                auto policy = DefaultDeserializationPolicy!false(relPol);
                policy.deserializeImpl(tokenizer, newItem);
                __traits(getMember, item, extrasMember).object[key] = newItem;
                break OBJ_MEMBER_SWITCH;
            }}
            else
            {
                throw new JSONIopipeException(format("No member named '%s' in type `%s`", name, T.stringof));
            }
        }
        // shut up compiler
        static if(members.length > 0 || ignoredMembers.length > 0)
        {
            if(relPol == ReleasePolicy.afterMembers)
                tokenizer.releaseParsed();
            jsonItem = tokenizer.nextSignificant();
        }
    }
    // ensure all members visited
    static if(members.length)
    {
        import std.algorithm : canFind, map, filter;
        if(visited[].canFind(false))
        {
            // this is a bit ugly, but gives a nicer message.
            static immutable marr = [members];
            import std.range : enumerate;
            throw new JSONIopipeException(format("The following members of `%s` were not specified: `%-(%s` `%)`", T.stringof, visited[].enumerate.filter!(a => !a[1]).map!(a => marr[a[0]])));
        }
    }
}

// if type is Nullable, first check for JSONToken.Null, and if not, try and
// parse real item.
void deserializeImpl(P, T, JT)(ref P policy, ref JT tokenizer, ref T item) if (isInstanceOf!(Nullable, T))
{
    if(tokenizer.peekSignificant == JSONToken.Null)
    {
        item.nullify;
        // skip the null value
        cast(void)tokenizer.nextSignificant;
    }
    else
    {
        typeof(item.get()) result;
        policy.deserializeImpl(tokenizer, result);
        item = result;
    }
}

// deserialize a class or interface. The type must either provide a static
// function that returns the deserialized type, or have a zero-arg constructor.
void deserializeImpl(P, T, JT)(ref P policy, ref JT tokenizer, ref T item) if ((is(T == class) || is(T == interface)))
{
    // NOTE: checking to see if it's callable doesn't help, because there could
    // be a bug, and in that case, it tries the other branch.
    static if(__traits(hasMember, T, "fromJSON"))
        // && is(typeof(item = T.fromJSON(tokenizer, relPol))))
    {
        static if(__traits(compiles, T.fromJSON(tokenizer, policy)))
            item = T.fromJSON(tokenizer, policy);
        else static if(__traits(compiles, T.fromJSON(tokenizer, ReleasePolicy.init)))
            item = T.fromJSON(tokenizer, policy.relPol);
        else
            // This will fail to compile. Try to nudge the user to use a policy
            item = T.fromJSON(tokenizer, policy);
    }
    else static if(is(T == class))
    {
        // peek for a null token.
        if(tokenizer.peekSignificant == JSONToken.Null)
        {
            item = null;
            cast(void)tokenizer.nextSignificant;
        }
        else
        {
            auto t = new T();
            deserializeObject(policy, tokenizer, t);
            item = t;
        }
    }
    else static assert(false, "Cannot deserialize interface " ~ T.stringof ~ " without a static `fromJson` member");
}

// deserialize null class member
unittest
{
    static class C {
        int x;
    }
    static struct S {
        C c;
    }

    auto s = deserialize!S(`{"c" : null}`);
    assert(s.c is null);
}

// TODO: this really is pure, but there is a cycle in the DOM parser so
// compiler doesn't infer it.
/*pure*/ unittest
{
    static struct S
    {
        int x;
        string y;
        double d;
        bool b;
    }

    static class C
    {
        int x;
        string y;
    }

    static class D : C
    {
        double d;
        bool b;
    }

    auto json = q"{
        {
            "x" : 5,
            "y" : "foo",
            "d" : 8.5,
            "b" : true
        }}";

    auto s = json.deserialize!S;
    assert(s.x == 5);
    assert(s.y == "foo");
    assert(s.d == 8.5);
    assert(s.b);

    auto c = json.deserialize!D;
    assert(c.x == 5);
    assert(c.y == "foo");
    assert(c.d == 8.5);
    assert(c.b);

    // test arrays and sub-objects
    static struct S2
    {
        int[4] arr1;
        int[] arr2;
        S[] arr3;
    }
    auto json2= q"{
        {
            "arr1" : [5,6,7,8],
            "arr2" : [1,2,3,4,5,6,7,8],
            "arr3" : [
            {
                "x" : 5,
                "y" : "foo",
                "d" : 8.5,
                "b" : true
            },
            {
                "x" : 7,
                "y" : "bar",
                "d" : 10.0,
                "b" : false
            }]
        }}";
    auto s2 = json2.deserialize!S2;

    assert(s2.arr1 == [5,6,7,8]);
    assert(s2.arr2 == [1,2,3,4,5,6,7,8]);
    assert(s2.arr3 == [S(5, "foo", 8.5, true), S(7, "bar", 10.0, false)]);

    static struct S3
    {
        Nullable!int x;
        Nullable!int y;
        JSONValue!string json;
    }

    auto json3 = q"{
        {
            "x" : 5,
            "y" : null,
            "json" : {
            "arr1" : [5,6,7,8],
            "arr2" : [1,null,true,"hi"],
            "arr3" : [
            {
                "x" : 5,
                "y" : "foo",
                "d" : 8.5,
                "b" : true
            },
            {
                "x" : 7,
                "y" : "bar",
                "d" : 10.0,
                "b" : false
            }]
            }
        }
    }";

    auto s3 = json3.deserialize!S3;
    assert(!s3.x.isNull);
    assert(s3.x == 5);
    assert(s3.y.isNull);
    assert(s3.json.type == JSONType.Obj);
    assert(s3.json.object.length == 3);
    auto jv = s3.json.object["arr1"];
    assert(jv.type == JSONType.Array);
    assert(jv.array.length == 4);
    assert(jv.array[0].type == JSONType.Integer);
    assert(jv.array[1].type == JSONType.Integer);
    assert(jv.array[2].type == JSONType.Integer);
    assert(jv.array[3].type == JSONType.Integer);
    assert(jv.array[0].integer == 5);
    assert(jv.array[1].integer == 6);
    assert(jv.array[2].integer == 7);
    assert(jv.array[3].integer == 8);
    jv = s3.json.object["arr2"]; // array of different types
    assert(jv.type == JSONType.Array);
    assert(jv.array.length == 4);
    assert(jv.array[0].type == JSONType.Integer);
    assert(jv.array[1].type == JSONType.Null);
    assert(jv.array[2].type == JSONType.Bool);
    assert(jv.array[3].type == JSONType.String);
    assert(jv.array[0].integer == 1);
    //jv.array[1] no value for "Null" type
    assert(jv.array[2].boolean);
    assert(jv.array[3].str == "hi");
    jv = s3.json.object["arr3"];
    assert(jv.type == JSONType.Array);
    assert(jv.array.length == 2);
    foreach(v; jv.array)
    {
        assert(v.type == JSONType.Obj);
        assert(v.object.length == 4);
        assert("x" in v.object);
        assert("y" in v.object);
        assert("d" in v.object);
        assert("b" in v.object);
    }

    // ensure extras works
    static struct S4
    {
        int x;
        @extras JSONValue!string extraData;
    }

    auto s4 = json.deserialize!S4;
    assert(s4.x == 5);
    assert(s4.extraData.type == JSONType.Obj);
    assert(s4.extraData.object.length == 3);
    assert(s4.extraData.object["y"].type == JSONType.String);
    assert(s4.extraData.object["y"].str == "foo");
    assert(s4.extraData.object["d"].type == JSONType.Floating);
    assert(s4.extraData.object["d"].floating == 8.5);
    assert(s4.extraData.object["b"].type == JSONType.Bool);
    assert(s4.extraData.object["b"].boolean == true);

    // test ignoring extras
    @ignoreExtras static struct S5 {
        int x;
    }

    auto s5 = json.deserialize!S5;
    assert(s5.x == 5);
}


// test attributes and empty struct
unittest
{
    static struct S
    {
        int x;
        @ignore bool foo;
    }
    auto s = deserialize!S(`{"x": 5}`);
    assert(s.x == 5);

    static struct Y
    {
    }

    auto y = deserialize!Y(`{}`);

    static struct T
    {
        @serializeAs string s;
        int x;
    }

    T[] arr = deserialize!(T[])(`["hi", "there"]`);
    assert(arr.length == 2);
    assert(arr[0].s == "hi");
    assert(arr[1].s == "there");

}

// test serializing of class hierarchy
unittest
{
    @ignoredMembers("type")
    static class C
    {
        int x;
        static C fromJSON(JT)(ref JT tokenizer, ReleasePolicy relPol)
        {
            auto idx = tokenizer.index;
            C doDeserialize(T)()
            {
                tokenizer.index = idx;
                auto item = new T;
                tokenizer.deserializeAllMembers(item, relPol);
                return item;
            }

            if(tokenizer.parseTo("type"))
            {
                int type;
                tokenizer.deserialize(type);
                switch(type)
                {
                case 0:
                    // it's a C
                    return doDeserialize!C;
                case 1:
                    return doDeserialize!D;
                case 2:
                    return doDeserialize!E;
                default:
                    assert(false, "Unknown type");
                }
            }
            assert(false, "Couldn't find type");
        }
    }

    static class D : C
    {
        string s;
    }

    static class E : C
    {
        double y;
    }

    auto msg1 = `[{"type" : 0, "x" : 1}, {"type" : 1, "x" : 2, "s" : "hi"}, {"type" : 2, "x" : 3, "y": 5.6}]`;

    auto cArr = msg1.deserialize!(C[]);
    assert(cArr.length == 3);
}

// test serializing of class hierarchy with policy
unittest
{
    @ignoredMembers("type")
    static class C
    {
        int x;
        static C fromJSON(P, JT)(ref JT tokenizer, ref P policy)
        {
            auto idx = tokenizer.index;
            C doDeserialize(T)()
            {
                tokenizer.index = idx;
                auto item = new T;
                deserializeObject(policy, tokenizer, item);
                return item;
            }

            if(tokenizer.parseTo("type"))
            {
                int type;
                tokenizer.deserialize(type);
                switch(type)
                {
                case 0:
                    // it's a C
                    return doDeserialize!C;
                case 1:
                    return doDeserialize!D;
                case 2:
                    return doDeserialize!E;
                default:
                    assert(false, "Unknown type");
                }
            }
            assert(false, "Couldn't find type");
        }
    }

    static class D : C
    {
        string s;
    }

    static class E : C
    {
        double y;
    }

    auto msg1 = `[{"type" : 0, "x" : 1}, {"type" : 1, "x" : 2, "s" : "hi"}, {"type" : 2, "x" : 3, "y": 5.6}]`;

    auto cArr = msg1.deserialize!(C[]);
    assert(cArr.length == 3);
}

void deserializeObject(T, JT, Policy)(
    ref Policy policy,
    ref JT tokenizer,
    ref T item,
)
{
    // Begin deserialization
    tokenizer.nextSignificant
        .jsonExpect(JSONToken.ObjectStart, "Parsing " ~ T.stringof);
    auto context = policy.onObjectBegin(tokenizer, item);

    if (tokenizer.peekSignificant() == JSONToken.ObjectEnd) {
        tokenizer.nextSignificant
          .jsonExpect(JSONToken.ObjectEnd, "Parsing " ~ T.stringof);
        return; // empty object, nothing to do
    }
    // Process fields
    auto jsonItem = tokenizer.nextSignificant();
    while(true)
    {
        if(jsonItem.token == JSONToken.Comma)
        {
            static if(tokenizer.config.JSON5)
            {
                if(tokenizer.peekSignificant() == JSONToken.ObjectEnd)
                    break;
            }
            jsonItem = tokenizer.nextSignificant();
            continue;
        }

        // Validate key
        static if(tokenizer.config.JSON5)
        {
            if(jsonItem.token != JSONToken.String && jsonItem.token != JSONToken.Symbol)
                jsonExpect(jsonItem, JSONToken.String, "Expecting member name of " ~ T.stringof);
        }
        else
            jsonExpect(jsonItem, JSONToken.String, "Expecting member name of " ~ T.stringof);

        auto key = jsonItem;

        // Expect colon
        jsonItem = tokenizer.nextSignificant()
            .jsonExpect(JSONToken.Colon, "Expecting colon when parsing " ~ T.stringof);

        policy.onField(tokenizer, item, key, context);

        if (tokenizer.peekSignificant() == JSONToken.ObjectEnd)
        {
            // If we hit the end of the object, break
            break;
        }

        jsonItem = tokenizer.nextSignificant();
    }

    // End deserialization
    policy.onObjectEnd(tokenizer, item, context);
    tokenizer.nextSignificant
        .jsonExpect(JSONToken.ObjectEnd, "Parsing " ~ T.stringof);
}

void deserializeImpl(T, JT, Policy)(
    ref Policy policy,
    ref JT tokenizer,
    ref T item,
) if (is(T == struct) && !isInstanceOf!(JSONValue, T) && !isInstanceOf!(Nullable, T) && !__traits(hasMember, T, "fromJSON"))
{

    // check to see if any member is defined as the representation
    alias representers = getSymbolsByUDA!(T, serializeAs);
    static if(representers.length > 0)
    {
        static assert(representers.length == 1, "Only one field can be used to represent an object");
        policy.deserializeImpl(tokenizer, __traits(getMember, item, __traits(identifier, representers[0])));
    }
    else
    {
        deserializeObject(policy, tokenizer, item);
    }
}

void deserializeArray(T, JT, Policy)(
    ref Policy policy,
    ref JT tokenizer,
    ref T item
)
{
    auto jsonItem = tokenizer.nextSignificant
        .jsonExpect(JSONToken.ArrayStart, "Parsing " ~ T.stringof);
    auto context = onArrayBegin(policy, tokenizer, item);

    // Parse array elements
    size_t elementCount = 0;
    while(true) {

        if (tokenizer.peekSignificant() == JSONToken.ArrayEnd) {
            // Handle empty array case
            break;
        }
      
        policy.onArrayElement(tokenizer, item, elementCount, context);
        elementCount++;

        if (tokenizer.peekSignificant() == JSONToken.ArrayEnd) {
            // If we hit the end of the array, break
            break;
        }

        // verify and consume the comma
        jsonItem = tokenizer.nextSignificant()
                    .jsonExpect(JSONToken.Comma, "Parsing " ~ T.stringof);

        static if (tokenizer.config.JSON5)
        {
            if (tokenizer.peekSignificant() == JSONToken.ArrayEnd)
                break;
        }

    }

    // verify we got an end array element
    jsonItem = tokenizer.nextSignificant
        .jsonExpect(JSONToken.ArrayEnd, "Parsing " ~ T.stringof);

    policy.onArrayEnd(tokenizer, item, elementCount, context);
}

void deserializeImpl(T, JT, Policy)(
    ref Policy policy,
    ref JT tokenizer,
    ref T item
) if (__traits(isStaticArray, T))
{
    deserializeArray(policy, tokenizer, item);
}

void deserializeImpl(T, JT, Policy)(
    ref Policy policy,
    ref JT tokenizer,
    ref T item
) if (isDynamicArray!T && !isSomeString!T && !is(T == enum))
{
    import std.array : Appender;
    // Deserialize into an appender
    auto app = Appender!T();

    deserializeArray(policy, tokenizer, app);

    // store the appender data into the actual item
    item = app[];
}

unittest
{
    // Test a simple staticArray with a default policy
    auto jsonStr = `[1, 2, 3]`;
    int[3] arr;
    arr = deserialize!(int[3])(jsonStr);

    assert(arr[0] == 1);
    assert(arr[1] == 2);
    assert(arr[2] == 3);

    import std.exception;
    // Test handling of JSON arrays with extra elements
    auto jsonWithExtra = `[1, 2, 3, 4, 5]`;  // Has 5 elements
    int[3] smallerArray;  // Only has space for 3 elements

    // This should throw an exception - can't deserialize more elements than array size
    assertThrown!JSONIopipeException(
        deserialize!(int[3])(jsonWithExtra)
    );

    auto json5WithTrailingComma = `[1, 2, 3,]`; // JSON5 allows trailing commas
    int[3] trailingCommaArray;
    auto json5Tokenizer = json5WithTrailingComma.jsonTokenizer!(ParseConfig(false,true, false, false));
    trailingCommaArray = deserialize!(int[3])(json5Tokenizer);
    assert(trailingCommaArray[0] == 1);
    assert(trailingCommaArray[1] == 2);
    assert(trailingCommaArray[2] == 3);
}

unittest
{
    import std.datetime.date;
    // Test custom handling of a type with a policy.

    static struct DTStringPolicy {
        void deserializeImpl(JT, T)(ref JT tokenizer, ref T item) {
            static if (is(T == DateTime))
            {
                // Deserialize DateTime from a string
                auto jsonItem = tokenizer.nextSignificant
                    .jsonExpect(JSONToken.String, "Parsing DateTime");
                item = DateTime.fromSimpleString(extractString!string(jsonItem));
            }
            else {
                // default to module behavior
                .deserializeImpl(this, tokenizer, item);
            };
        }
    }

    auto pol = DTStringPolicy();
    auto dt = deserialize!DateTime(`"2001-Jan-01 12:00:00"`, pol);
    assert(dt == DateTime(2001, 1, 1, 12, 0, 0));

    static struct Person {
        string name;
        int age;
        DateTime dob;
    }

    auto jsonStr = `{
        "name": "John Doe",
        "age": 30,
        "dob" : "2001-Jan-01 12:00:00"
    }`;
    auto p = deserialize!Person(jsonStr, pol);
    assert(p.name == "John Doe");
    assert(p.age == 30);
    assert(p.dob == DateTime(2001, 1, 1, 12, 0, 0));
}

unittest
{
    // Test deserializing DateTime with a custom policy
    import std.datetime.date;

    static struct DTStringPolicy {
        void deserializeImpl(JT, T)(ref JT tokenizer, ref T item) {
            static if (is(T == DateTime))
            {
                // Deserialize DateTime from a string
                auto jsonItem = tokenizer.nextSignificant
                    .jsonExpect(JSONToken.String, "Parsing DateTime");
                item = DateTime.fromSimpleString(extractString!string(jsonItem));
            }
            else {
                // default to module behavior
                .deserializeImpl(this, tokenizer, item);
            };
        }
    }

    auto policy = DTStringPolicy();
    auto jsonStr = `["2001-Jan-01 12:00:00", "2002-Feb-15 13:30:45", "2003-Mar-20 14:15:30"]`;
    DateTime[3] dates;
    dates = deserialize!(DateTime[3])(jsonStr, policy);

    assert(dates[0] == DateTime(2001, 1, 1, 12, 0, 0));
    assert(dates[1] == DateTime(2002, 2, 15, 13, 30, 45));
    assert(dates[2] == DateTime(2003, 3, 20, 14, 15, 30));

     static struct Worker {
        string name;
        int age;
        DateTime[3] workSchedule;  // Array of work datetime entries
    }

    auto jsonWorker = `{
        "name": "Alice Smith",
        "age": 32,
        "workSchedule": [
            "2023-Jun-15 09:00:00",
            "2023-Jun-16 09:30:00",
            "2023-Jun-17 08:45:00"
        ]
    }`;

    auto worker = deserialize!Worker(jsonWorker, policy);
    assert(worker.name == "Alice Smith");
    assert(worker.age == 32);
    assert(worker.workSchedule[0] == DateTime(2023, 6, 15, 9, 0, 0));
    assert(worker.workSchedule[1] == DateTime(2023, 6, 16, 9, 30, 0));
    assert(worker.workSchedule[2] == DateTime(2023, 6, 17, 8, 45, 0));
}

unittest
{
    // simple dynamic array test with policy
    static struct Person {
        string name;
        int age;
    }


    auto jsonStr = `[
        {"name": "Alice", "age": 30},
        {"name": "Bob", "age": 25}
    ]`;

    // Deserialize with policy
    auto persons = deserialize!(Person[])(jsonStr);
    assert(persons.length == 2);
    assert(persons[0].name == "Alice");
    assert(persons[0].age == 30);
    assert(persons[1].name == "Bob");
    assert(persons[1].age == 25);
}

void deserialize(T, JT, Policy)(
    ref JT tokenizer,
    ref T item,
    Policy policy
) if (isInstanceOf!(JSONTokenizer, JT))
{
    policy.deserializeImpl(tokenizer, item);
}

T deserialize(T, JT, Policy)(
    ref JT tokenizer,
    Policy policy
) if (isInstanceOf!(JSONTokenizer, JT))
{
    T result;
    deserialize(tokenizer, result, policy);
    return result;
}

T deserialize(T, JT)(
    ref JT tokenizer,
) if (isInstanceOf!(JSONTokenizer, JT))
{
    auto policy = DefaultDeserializationPolicy!false();
    return deserialize!T(tokenizer, policy);
}

void deserialize(T, JT)(
    ref JT tokenizer,
    ref T item
) if (isInstanceOf!(JSONTokenizer, JT))
{
    auto policy = DefaultDeserializationPolicy!false();
    deserialize(tokenizer, item, policy);
}

T deserialize(T, Chain)(
    auto ref Chain c
) if (isIopipe!Chain)
{
    enum shouldReplaceEscapes = is(typeof(c.window[0] = c.window[1]));
    auto tokenizer = c.jsonTokenizer!(ParseConfig(shouldReplaceEscapes));
    return deserialize!T(tokenizer);
}

T deserialize(T, Policy, Chain)(
    auto ref Chain c,
    Policy policy
) if (isIopipe!Chain)
{
    enum shouldReplaceEscapes = is(typeof(c.window[0] = c.window[1]));
    auto tokenizer = c.jsonTokenizer!(ParseConfig(shouldReplaceEscapes));
    return deserialize!T(tokenizer, policy);
}

void deserialize(T, Chain)(
    auto ref Chain c,
    ref T item
) if (isIopipe!Chain)
{
    enum shouldReplaceEscapes = is(typeof(c.window[0] = c.window[1]));
    auto tokenizer = c.jsonTokenizer!(ParseConfig(shouldReplaceEscapes));
    deserialize(tokenizer, item);
}



unittest
{
    struct Person {
        string firstName;
        string lastName;
        int age;
    }
    auto jsonStr = `{"firstName": "John", "lastName": "Doe", "age": 30}`;
    auto person = deserialize!Person(jsonStr);
    assert(person.firstName == "John");
    assert(person.lastName == "Doe");
    assert(person.age == 30);
}


unittest
{
    // Test deserializing a struct with nested objects

    import std.exception;

    struct Pet {
        string name;
        int age;
    }

    struct Person {
        string firstName;
        string lastName;
        int age;
        Pet pet;
    }
    auto jsonStr = `{
        "firstName": "John",
        "lastName": "Doe",
        "age": 30,
        "pet": {
            "name": "Fido",
            "age": 5
        }
    }`;

    auto person = deserialize!Person(jsonStr);
    assert(person.firstName == "John");
    assert(person.lastName == "Doe");
    assert(person.age == 30);
    assert(person.pet.name == "Fido");
    assert(person.pet.age == 5);
}

unittest
{
    // Test on new design policy in extra members.

    static struct T {
        string name;
        @extras JSONValue!string stuff;
    }

    auto jsonStr = `{
        "name": "valid",
        "a": "another string",
        "b": 2,
        "c": 8.5,
        "pet": {
            "name": "Fido",
            "age": 5
        }}`;

    auto t = deserialize!T(jsonStr);
    assert(t.name == "valid");
    assert(t.stuff.type == JSONType.Obj);
    assert(t.stuff.object["a"].type == JSONType.String);
    assert(t.stuff.object["a"].str == "another string");
    assert(t.stuff.object["b"].type == JSONType.Integer);
    assert(t.stuff.object["b"].integer == 2);
    assert(t.stuff.object["c"].type == JSONType.Floating);
    assert(t.stuff.object["c"].floating == 8.5);
    assert(t.stuff.object["pet"].type == JSONType.Obj);
    assert(t.stuff.object["pet"].object["name"].type == JSONType.String);
    assert(t.stuff.object["pet"].object["name"].str == "Fido");
    assert(t.stuff.object["pet"].object["age"].type == JSONType.Integer);
    assert(t.stuff.object["pet"].object["age"].integer == 5);


    // Test that @extras member is not directly overwritten by JSON field with same name
    static struct ExtrasTest {
        int a, b, c;
        @extras JSONValue!string extras1;
    }

    // JSON that has both regular fields AND an "extras" field that should go into extras member
    auto jsonExtraStr = `{"a": 1, "b": 2, "c": 3, "d": 4, "extras1": "foo"}`;

    auto oldResult = deserialize!ExtrasTest(jsonExtraStr);
    assert(oldResult.a == 1);
    assert(oldResult.b == 2);
    assert(oldResult.c == 3);
    assert(oldResult.extras1.type == JSONType.Obj);
    assert(oldResult.extras1.object.length == 2);
    // The unknown field "d" should be in extras
    assert("d" in oldResult.extras1.object);
    assert(oldResult.extras1.object["d"].type == JSONType.Integer);
    assert(oldResult.extras1.object["d"].integer == 4);
    // The "extras" field from JSON should ALSO be in the extras member (not overwrite it)
    assert("extras1" in oldResult.extras1.object);
    assert(oldResult.extras1.object["extras1"].type == JSONType.String);
    assert(oldResult.extras1.object["extras1"].str == "foo");

    auto result = deserialize!ExtrasTest(jsonExtraStr);

    // Verify regular fields are set correctly
    assert(result.a == 1);
    assert(result.b == 2);
    assert(result.c == 3);

    // Verify extras member contains the unknown field "d" AND the "extras" field
    assert(result.extras1.type == JSONType.Obj);
    assert(result.extras1.object.length == 2);

    // The unknown field "d" should be in extras
    assert("d" in result.extras1.object);
    assert(result.extras1.object["d"].type == JSONType.Integer);
    assert(result.extras1.object["d"].integer == 4);

    // The "extras" field from JSON should ALSO be in the extras member (not overwrite it)
    assert("extras1" in result.extras1.object);
    assert(result.extras1.object["extras1"].type == JSONType.String);
    assert(result.extras1.object["extras1"].str == "foo");
}

unittest
{
    import std.exception;

    // First test: Without @ignoredMembers - should throw exception
    static struct PersonNoIgnore {
        string name;
        int age;
    }

    // JSON with extra "type" field that doesn't exist in our struct
    auto jsonWithExtra = `{"name": "John", "age": 30, "type": "customer"}`;

    // This should throw an exception for the unknown "type" field
    assertThrown!JSONIopipeException(
        deserialize!(PersonNoIgnore)(jsonWithExtra)
    );

    // Second test: With @ignoredMembers - should work fine
    @ignoredMembers("type")
    static struct PersonWithIgnore {
        string name;
        int age;
    }

    // Same JSON with the extra field
    auto person = deserialize!(PersonWithIgnore)(jsonWithExtra);

    // Verify the deserialization worked correctly
    assert(person.name == "John");
    assert(person.age == 30);
}


struct DefaultSerializationPolicy {

}


void serializeImpl(P, T, Char)(ref P policy, scope void delegate(const(Char)[]) w, ref T val) if (__traits(isStaticArray, T))
{
    auto arr = val;
    policy.serializeImpl(w, val[]);
}

unittest
{
    // ensure static array serialization works
    int[5] arr = [1,2,3,4,5];
    assert(serialize(arr) == "[1, 2, 3, 4, 5]");
}

void serializeImpl(P, T, Char)(ref P policy, scope void delegate(const(Char)[]) w, ref T val) if (is(T == enum))
{
    // enums are special, serialize based on the name. Unless there's a UDA
    // saying to serialize as the base type.
    static if(hasUDA!(T, enumBaseType))
    {
        policy.serializeImpl(w, *cast(OriginalType!T*)&val);
    }
    else
    {
        auto enumName = val.to!string;
        policy.serializeImpl(w, enumName);
    }
}

void serializeImpl(P, T, Char)(ref P policy, scope void delegate(const(Char)[]) w, T val) if (isDynamicArray!T && !isSomeString!T && !is(T == enum))
{
    // open brace
    w("[");
    bool first = true;
    foreach(ref item; val)
    {
        if(first)
            first = false;
        else
            w(", ");
        policy.serializeImpl(w, item);
    }
    w("]");
}

void serializeImpl(P, T, Char)(ref P policy, scope void delegate(const(Char)[]) w, T val) if (is(T == V[K], V, K) /* && isSomeString!K */)
{
    assert(is(T == V[K], V, K));
    enum useKW = !isSomeString!K;
    // provide a specialized key serialization function if the type is not a string
    static if(useKW)
    {
        // "key write" function. key must always start and end with a quote
        // (sorry, json5, we are doing it this way for now with AAs)
        bool keyStart;
        bool keyEnd;
        void kw(const(Char)[] str)
        {
            if(!keyStart)
            {
                // validate that the key starts with "
                if(str[0] != '"')
                    throw new JSONIopipeException("Key in AA of type " ~ T.stringof ~ " must serialize to string that starts with a quote");
                keyStart = true;
            }
            keyEnd = str[$-1] == '"';
            w(str);
        }
    }

    // open brace
    w("{");
    bool first = true;
    foreach(k, v; val)
    {
        if(first)
            first = false;
        else
            w(", ");
        static if(useKW)
        {
            policy.serializeImpl(&kw, k);

            // validate the key ended
            if(!keyEnd)
                throw new JSONIopipeException("Key of type " ~ T.stringof ~ " must serialize to a string that ends with a quote");
        }
        else
            policy.serializeImpl(w, k);

        w(" : ");

        policy.serializeImpl(w, v);
    }
    w("}");
}

unittest
{
    auto serialized = serialize(["a" : 1, "b": 2]);
    assert(serialized == `{"a" : 1, "b" : 2}` || serialized == `{"b" : 2, "a" : 1}`);
    enum X
    {
        a,
        b
    }
    serialized = serialize([X.a : 1, X.b: 2]);
    assert(serialized == `{"a" : 1, "b" : 2}` || serialized == `{"b" : 2, "a" : 1}`);
}

/** Eponymous template that generates an argument list for std.algorithm.substitute to correctly escape json strings
 * Result:
 *      AliasSeq!("<char>", "<escapesequence>", "<char>", "<escapesequence>", ...);
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
        import std.format;
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
        import std.conv: to;
        // Must convert char to string for use with substitute
        enum string staticCast(char c) = c.to!string;
        enum string staticCast(string s) = s;
        alias UnpackStruct = staticMap!(staticCast, jem.tupleof);
    }

    import std.meta;
    private alias jsonEscapeSubstitutions = staticMap!(UnpackStruct, aliasSeqOf!(charactersToEscape.map!escapeSingleChar));
}

/// Instantiate the template without arguments so it's not necessary at the callsite anymore
void serializeImpl(P, T, Char)(ref P Policy, scope void delegate(const(Char)[]) w, T val) if (isSomeString!T)
{
    import std.algorithm.iteration: substitute;
    w(`"`);
    put(w, val.substitute!(jsonEscapeSubstitutions!()));
    w(`"`);
}

// Escape special characters
unittest
{
    string raw = `\ and " must be escaped!`;
    assert(raw.serialize == `"\\ and \" must be escaped!"`);
}

// Special characters must survive roundtrip.
unittest
{
    string raw = `\ and " must be escaped!`;
    assert(raw.serialize.deserialize!string == raw);
}

// Some characters get escaped to unicode escape sequences and some don't, depending on whether the spec allowed special escape sequences for them
unittest
{
    string raw = "\0\a\u001f\t";
    assert(raw.serialize == `"\u0000\u0007\u001f\t"`);
    assert(raw.serialize.deserialize!string == raw);
}

void serializeAllMembers(P, T, Char)(ref P policy, scope void delegate(const(Char)[]) w, auto ref T val)
{
    // serialize as an object
    bool first = true;
    static foreach(n; SerializableMembers!T)
    {
        static if(hasUDA!(__traits(getMember, T, n), extras))
        {
            // this is the extras member, It should be an object, with all the information inside there.
            foreach(k, ref v; __traits(getMember, val, n).object)
            {
                if(first)
                    first = false;
                else
                    w(", ");
                w(`"`);
                w(k);
                w(`" : `);
                serializeImpl(w, v);
            }
        }
        else
        {
            if(first)
                first = false;
            else
                w(", ");
            w(`"`);
            static if(hasUDA!(__traits(getMember, T, n), alternateName))
                w(getUDAs!(__traits(getMember, T, n), alternateName)[0].name);
            else
                w(n);
            w(`" : `);
            policy.serializeImpl(w, __traits(getMember, val, n));
        }
    }
}

///compatibility shim
void serializeAllMembers(T, Char)(scope void delegate(const(Char)[]) w, auto ref T val)
{
    auto policy = DefaultSerializationPolicy;
    serializeAllmembers(policy, w, val);
}

void serializeImpl(P, T, Char)(ref P policy, scope void delegate(const(Char)[]) w, ref T val) if (is(T == struct))
{
    static if(isInstanceOf!(Nullable, T))
    {
        if(val.isNull)
            w("null");
        else
            policy.serializeImpl(w, val.get);
    }
    else static if(isInstanceOf!(JSONValue, T))
    {
        with(JSONType) final switch(val.type)
        {
        case Integer:
            policy.serializeImpl(w, val.integer);
            break;
        case Floating:
            policy.serializeImpl(w, val.floating);
            break;
        case String:
            policy.serializeImpl(w, val.str);
            break;
        case Array:
            policy.serializeImpl(w, val.array);
            break;
        case Obj:
            // serialize as if it was an object
            w("{");
            {
                bool first = true;
                foreach(k, ref v; val.object)
                {
                    if(first)
                        first = false;
                    else
                        w(", ");
                    w(`"`);
                    w(k);
                    w(`" : `);
                    policy.serializeImpl(w, v);
                }
            }
            w("}");
            break;
        case Null:
            w("null");
            break;
        case Bool:
            w(val.boolean ? "true" : "false");
            break;
        }
    }
    else static if(__traits(hasMember, T, "toJSON"))
    {
        val.toJSON(w);
    }
    else static if(getSymbolsByUDA!(T, serializeAs).length > 0)
    {
        alias representers = getSymbolsByUDA!(T, serializeAs);
        // serialize as the single item
        static assert(representers.length == 1, "Only one field can be used to represent an object");
        policy.serializeImpl(w, __traits(getMember, val, __traits(identifier, representers[0])));
    }
    else static if(isInputRange!T)
    {
        // open brace
        w("[");
        bool first = true;
        foreach(ref item; val)
        {
            if(first)
                first = false;
            else
                w(", ");
            serializeImpl(w, item);
        }
        w("]");
    }
    else
    {
        w("{");
        policy.serializeAllMembers(w, val);
        w("}");
    }
}

void serializeImpl(P, T, Char)(ref P policy, scope void delegate(const(Char)[]) w, T val) if (is(T == class) || is(T == interface))
{
    if(val is null)
    {
        w("null");
        return;
    }
    // If the class defines a method toJSON, then use that. Otherwise, we will
    // just serialize the data as we can.
    static if(__traits(hasMember, T, "toJSON"))
    {
        val.toJSON(w);
    }
    else
    {
        w("{");
        policy.serializeAllMembers(w, val);
        w("}");
    }
}

// null class members
unittest
{
    static class C {
        int x;
    }
    static struct S {
        C c;
    }

    S s;
    assert(serialize(s) == `{"c" : null}`);
}


void serializeImpl(P, Char)(ref P policy, scope void delegate(const(Char)[]) w, typeof(null) val)
{
    w("null");
}

// allow serializing null
unittest
{
    static struct S {
        typeof(null) n;
    }
    S s;
    assert(serialize(s) == `{"n" : null}`);
}

void serializeImpl(P, T, Char)(ref P policy, scope void delegate(const(Char)[]) w, ref T val) if (!is(T == enum) && isNumeric!T)
{
    formattedWrite(w, "%s", val);
}

void serializeImpl(P, Char)(ref P policy, scope void delegate(const(Char)[]) w, bool val)
{
    w(val ? "true" : "false");
}

// serialize an item to an iopipe.
// The behavior flag specifies whether the json serializer should release data
// in the iopipe as it writes, or if it should keep it in the buffer (no
// releases are called).
//
// Use case for releasing is for an output pipe that will be written to a file
// for instance as it's released. Use case for not releasing is when you are
// writing to a string.
//
// Returns: number of elements written in the output iopipe. If
// release-on-write is specified, none of the data will remain in the immediate
// iopipe buffer.
// If no-release is specified, then the return value indicates the number of
// elements that are in the buffer. If offset is specified, then that is where
// the data will begin to be written.
//
size_t serialize(ReleaseOnWrite relOnWrite = ReleaseOnWrite.yes, Chain, T)(auto ref Chain chain, auto ref T val, size_t offset = 0)
if (isIopipe!Chain && isSomeChar!(ElementType!(WindowType!Chain)))
{
    size_t result = 0;
    alias Char = ElementEncodingType!(WindowType!Chain);
    void w(const(Char)[] data)
    {
        auto nWritten = chain.writeBuf!relOnWrite(data, offset);
        result += nWritten;
        static if(relOnWrite)
            offset = 0;
        else
            offset += nWritten;
    }

    // serialize the item, recursively
    auto policy = DefaultSerializationPolicy();
    serializeImpl(policy, &w, val);

    return result;
}

// convenience, using normal serialization to write to a string.
string serialize(T)(auto ref T val)
{
    import std.exception;
    auto outBuf = bufd!char;
    auto dataSize = outBuf.serialize!(ReleaseOnWrite.no)(val);
    auto result = outBuf.window[0 .. dataSize];
    result.assumeSafeAppend;
    return result.assumeUnique;
}

unittest
{
    auto str1 = serialize(1);
    assert(str1 == "1");
    int item1;
    auto strpipe = str1;
    strpipe.deserialize(item1);
    assert(item1 == 1);
    auto str2 = serialize([1,2,3,4]);
    assert(str2 == "[1, 2, 3, 4]");
    int[4] item2;
    strpipe = str2;
    strpipe.deserialize(item2);
    assert(item2[] == [1, 2, 3, 4]);
    assert(str2 == "[1, 2, 3, 4]");
    static struct S
    {
        int x;
        float y;
        string s;
        bool b;
    }

    assert(serialize(S(1, 2.5, "hi", true)) == `{"x" : 1, "y" : 2.5, "s" : "hi", "b" : true}`);

    // serialize nested arrays and objects
    auto str3 = serialize([S(1, 3.0, "foo", false), S(2, 8.5, "bar", true)]);
    assert(str3 == `[{"x" : 1, "y" : 3, "s" : "foo", "b" : false}, {"x" : 2, "y" : 8.5, "s" : "bar", "b" : true}]`, str3);
    auto arr = str3.deserialize!(S[]);
    assert(arr.length == 2);
    assert(arr[0].s == "foo");
    assert(arr[1].b);
    assert(arr[1].x == 2);
}

unittest
{
    static struct S
    {
        int x;
        @ignore bool y;
    }

    auto s = S(1, true);
    auto str = s.serialize;
    assert(str == `{"x" : 1}`, str);

    static struct T
    {
        @serializeAs string s;
        int x;
    }

    static struct U
    {
        T t;
    }
    auto u = U(T("hello", 1));
    auto str2 = u.serialize;
    assert(str2 == `{"t" : "hello"}`, str2);
}

unittest
{
    // serialization of classes
    static class C
    {
        int x;
    }

    static class D : C
    {
        string s;
        void toJSON(scope void delegate(const(char)[]) w)
        {
            //FIXME: should this be needed?
            auto policy = DefaultSerializationPolicy();

            w("{");
            policy.serializeAllMembers(w, this);
            w("}");
        }
    }

    static class E : D
    {
        double d;
        override void toJSON(scope void delegate(const(char)[]) w)
        {
            //FIXME: should this be needed?
            auto policy = DefaultSerializationPolicy();
            w("{");
            policy.serializeAllMembers(w, this);
            w("}");
        }
    }

    auto c = new C;
    c.x = 1;
    auto cstr = c.serialize;
    assert(cstr == `{"x" : 1}`, cstr);

    auto d = new D;
    d.x = 2;
    d.s = "str";
    auto dstr = d.serialize;
    assert(dstr == `{"s" : "str", "x" : 2}`, dstr);

    auto e = new E;
    e.x = 3;
    e.s = "foo";
    e.d = 1.5;
    auto estr = e.serialize;
    assert(estr == `{"d" : 1.5, "s" : "foo", "x" : 3}`, estr);
    d = e;
    assert(d.serialize == estr);
}

unittest
{
    // test serializing JSONValue
    auto j = deserialize!(JSONValue!string)(`{"a": [1, 2, 3], "b": null}`);
    assert(j.serialize == `{"a" : [1, 2, 3], "b" : null}`);
}

// JSON5 tests
unittest
{
    auto jsonStr = `
    {
        obj1: { a: .123, b: /*comment!*/ 'str', c: NaN, d: +Infinity, e: -0x42,},
        arr: ['abc', "def",],
    }`;
    // try with all the JSON5 configs
    static struct S1 {
        float a;
        string b;
        double c;
        real d;
        int e;
    }
    static struct S2 {
        S1 obj1;
        string[] arr;
    }
    static foreach(config; [
            ParseConfig(false, true, false, false),
            ParseConfig(false, true, true, false),
            ParseConfig(false, true, false, true),
            ParseConfig(false, true, true, true),
            ])
    {{
        import std.stdio;
        scope(failure) stderr.writeln("failing config is ", config);
        import std.math : isNaN, isClose;
        auto tokenizer = jsonStr.jsonTokenizer!config;
        auto s2 = tokenizer.deserialize!S2;
        assert(s2.obj1.a.isClose(0.123));
        assert(s2.obj1.b == "str");
        assert(isNaN(s2.obj1.c));
        assert(s2.obj1.d == real.infinity);
        assert(s2.obj1.e == -0x42);
        assert(s2.arr == ["abc", "def"]);

        // test with policy
        auto tokenizerWithPolicy = jsonStr.jsonTokenizer!config;
        auto s2WithPolicy = tokenizerWithPolicy.deserialize!S2;
        assert(s2WithPolicy.obj1.a.isClose(0.123));
        assert(s2WithPolicy.obj1.b == "str");
        assert(isNaN(s2WithPolicy.obj1.c));
        assert(s2WithPolicy.obj1.d == real.infinity);
        assert(s2WithPolicy.obj1.e == -0x42);
        assert(s2WithPolicy.arr == ["abc", "def"]);
    }}
}

// validate fromJSON works with structs
unittest
{
    static struct S
    {
        int x;
        int xDouble; /// redundant value derived from x, not in json representation
        static S fromJSON(JT)(ref JT tokenizer, ReleasePolicy relPol)
        {
            assert(tokenizer.nextSignificant.token == JSONToken.ObjectStart);
            auto xname = tokenizer.nextSignificant;
            assert(xname.token == JSONToken.String && xname.data(tokenizer.chain) == "x");
            assert(tokenizer.nextSignificant.token == JSONToken.Colon);
            auto val = tokenizer.nextSignificant;
            assert(val.token == JSONToken.Number);
            assert(val.hint == JSONParseHint.Int);
            assert(tokenizer.nextSignificant.token == JSONToken.ObjectEnd);
	    int x = val.data(tokenizer.chain).to!int;
            return S(x, x*2);
        }
    }

    assert(`{"x": 5}`.deserialize!S == S(5,10));
}

// fromJSON releasePolicy is optional
unittest
{
    static struct S
    {
        int x;
        static S fromJSON(JT)(ref JT tokenizer)
        {
            tokenizer.nextSignificant;
            tokenizer.nextSignificant;
            tokenizer.nextSignificant;
            auto val = tokenizer.nextSignificant;
	    int x = val.data(tokenizer.chain).to!int;
            return S(x);
        }
    }

    assert(`{"x": 5}`.deserialize!S == S(5));
}

/** This example demonstrates the invariants of fromJSON
 * Assuming valid JSON, the first token will be ObjectStart.
 * The ObjectEnd token in the end must be consumed by this function, but no further.
 * That way, the Struct/Class will just work as a JSONArray or a member of another object.
 */
unittest
{
    import std.exception;
    static struct S
    {
        int x;
        static S fromJSON(JT)(ref JT tokenizer, ReleasePolicy relPol)
        {
            jsonExpect(tokenizer.nextSignificant, JSONToken.ObjectStart, "First token must be ObjectStart");
            auto xname = tokenizer.nextSignificant;
            enforce!JSONIopipeException(xname.data(tokenizer.chain) == "x", "Unknown key");
            jsonExpect(tokenizer.nextSignificant, JSONToken.Colon, "Colon must follow key");
            auto val = tokenizer.nextSignificant;
	    // ObjectEnd must be consumed by fromJSON
            jsonExpect(tokenizer.nextSignificant, JSONToken.ObjectEnd, "Last token shall be be ObjectStart");
            return S(val.data(tokenizer.chain).to!int);
        }
    }
    auto tokenizer = `[{"x": 1},{"x": 2}]`.jsonTokenizer;

    assert(tokenizer.deserialize!(S[2]) == [S(1), S(2)]);
}

unittest
{
    import std.exception;
    static struct S
    {
        int x;
        static S fromJSON(P, JT)(ref JT tokenizer, ref P policy)
        {
            jsonExpect(tokenizer.nextSignificant, JSONToken.ObjectStart, "First token must be ObjectStart");
            auto xname = tokenizer.nextSignificant;
            enforce!JSONIopipeException(xname.data(tokenizer.chain) == "x", "Unknown key");
            jsonExpect(tokenizer.nextSignificant, JSONToken.Colon, "Colon must follow key");
            auto val = tokenizer.nextSignificant;
	    // ObjectEnd must be consumed by fromJSON
            jsonExpect(tokenizer.nextSignificant, JSONToken.ObjectEnd, "Last token shall be be ObjectStart");
            return S(val.data(tokenizer.chain).to!int);
        }
    }
    auto tokenizer = `[{"x": 1},{"x": 2}]`.jsonTokenizer;

    assert(tokenizer.deserialize!(S[2]) == [S(1), S(2)]);
}

// issue #22
unittest
{
    string json = `"\"test\n1\\2\""`;
	string str = deserialize!string(json);
	assert(str == "\"test\n1\\2\"");
}

// PR #23
unittest
{
    auto jsonstr = `"\"hello, world\n\""`;
    char[2048] buf;
    buf[0 .. jsonstr.length] = jsonstr;
    char[] str = buf[].deserialize!(char[]);
    assert(str == "\"hello, world\n\"");
    assert(str.ptr >= buf.ptr && str.ptr < buf.ptr + buf.length);

    str = null;
    buf[0 .. jsonstr.length] = jsonstr;
    buf[].deserialize(str);
    assert(str == "\"hello, world\n\"");
    assert(str.ptr >= buf.ptr && str.ptr < buf.ptr + buf.length);
}
