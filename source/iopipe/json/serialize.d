module iopipe.json.serialize;
import iopipe.json.parser;
import iopipe.json.dom;
import iopipe.traits;
import iopipe.bufpipe;
import std.range.primitives;

import std.traits;
import std.typecons : Nullable;

// define some UDAs to affect serialization
struct SerializeAs {}
struct Ignore {}
struct Optional {}

SerializeAs serializeAs() { return SerializeAs.init; }
Ignore ignore() { return Ignore.init; }
Optional optional() { return Optional.init; }

void jsonExpect(ref JSONItem item, JSONToken expectedToken, string msg, string file = __FILE__, size_t line = __LINE__) pure @safe
{
    if(item.token != expectedToken)
    {
        import std.format;
        throw new Exception(format("%s: expected %s, got %s", msg, expectedToken, item.token), file, line);
    }
}

private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (__traits(isStaticArray, T))
{
    auto jsonItem = tokenizer.next;
    jsonExpect(jsonItem, JSONToken.ArrayStart, "Parsing " ~ T.stringof);

    bool first = true;
    foreach(ref elem; item)
    {
        if(!first)
        {
            // verify there's a comma
            jsonItem = tokenizer.next;
            jsonExpect(jsonItem, JSONToken.Comma, "Parsing " ~ T.stringof);
        }
        first = false;
        deserializeImpl(tokenizer, elem);
        tokenizer.releaseParsed();
    }

    // verify we got an end array element
    jsonItem = tokenizer.next;
    jsonExpect(jsonItem, JSONToken.ArrayEnd, "Parsing " ~ T.stringof);
}

private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (is(T == enum))
{
    // enums are special, we can serialize them based on strings or integral values
    auto jsonItem = tokenizer.next;
    import std.conv : to;
    static if(is(T : int))
    {
        // int based enum. If the next token is a number, parse it and convert
        if(jsonItem.token == JSONToken.Number)
        {
            // it's a number, parse as an integer, and see if it converts.
            auto intval = jsonItem.data(tokenizer.chain).to!int;
            // parse into the enum.
            item = intval.to!T;
            return;
        }
    }

    // convert to the enum via the string name
    jsonExpect(jsonItem, JSONToken.String, "Parsing " ~ T.stringof);
    item = jsonItem.data(tokenizer.chain).to!T;
}

// TODO: should deal with writable input ranges and output ranges
private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (isDynamicArray!T && !isSomeString!T && !is(T == enum))
{
    auto jsonItem = tokenizer.next;
    jsonExpect(jsonItem, JSONToken.ArrayStart, "Parsing " ~ T.stringof);

    import std.array : Appender;
    auto app = Appender!T();

    // check for an empty array (special case)
    if(tokenizer.peek == JSONToken.ArrayEnd)
    {
        // parse it off
        jsonItem = tokenizer.next;
        // nothing left to do
        return;
    }
    // parse items and commas until we get an array end.
    while(true)
    {
        typeof(item[0]) elem;
        deserializeImpl(tokenizer, elem);
        app ~= elem;
        tokenizer.releaseParsed();
        jsonItem = tokenizer.next;
        if(jsonItem.token == JSONToken.ArrayEnd)
            break;
        jsonExpect(jsonItem, JSONToken.Comma, "Parsing " ~ T.stringof);
    }

    // fill in the data.
    item = app.data;
}

private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (isNumeric!T)
{
    import std.conv : parse;
    import std.format : format;
    auto jsonItem = tokenizer.next;
    jsonExpect(jsonItem, JSONToken.Number, "Parsing " ~ T.stringof);

    auto str = jsonItem.data(tokenizer.chain);
    static if(isIntegral!T)
    {
        if(jsonItem.hint != JSONParseHint.Int)
        {
            throw new Exception(format("Cannot parse `%s` from '%s'", T.stringof, jsonItem.data(tokenizer.chain)));
        }
    }

    // get the string from the buffer that contains the number
    auto window = jsonItem.data(tokenizer.chain);
    item = window.parse!T;
    if(!window.empty)
    {
        throw new Exception(format("Parsing of `%s` from source '%s' failed near '%s'", T.stringof, jsonItem.data(tokenizer.chain), window));
    }
}

private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (is(T == bool))
{
    import std.conv : parse;
    import std.format : format;
    auto jsonItem = tokenizer.next;
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
        import std.format;
        throw new Exception(format("Parsing bool: expected %s or %s , but got %s", JSONToken.True, JSONToken.False, jsonItem.token));
    }
}

private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (isSomeString!T)
{
    // Use phobos `to`, we want to duplicate the string if necessary.
    import std.conv : to;
    import std.format : format;

    auto jsonItem = tokenizer.next;
    jsonExpect(jsonItem, JSONToken.String, "Parsing " ~ T.stringof);

    // this should not fail unless the data is non-unicode
    // TODO: may need to copy the data if not immutable
    item = jsonItem.data(tokenizer.chain).to!T;
}

// TODO: need to ignore function members
// TODO: need to use UDAs to drive how this works
private template SerializableMembers(T)
{
    import std.traits;
    import std.meta;
    enum WithoutIgnore(string s) = !hasUDA!(__traits(getMember, T, s), Ignore());
    enum SerializableMembers = Filter!(WithoutIgnore, FieldNameTuple!T);
}

private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (is(T == struct) && __traits(hasMember, T, "fromJSON"))
{
    item.fromJSON(tokenizer);
}

private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (is(T == struct) && !isInstanceOf!(JSONValue, T) && !isInstanceOf!(Nullable, T) && !__traits(hasMember, T, "fromJSON"))
{
    // check to see if any member is defined as the representation
    import std.traits;
    alias representers = getSymbolsByUDA!(T, SerializeAs());
    static if(representers.length > 0)
    {
        static assert(representers.length == 1, "Only one field can be used to represent an object");
        deserializeImpl(tokenizer, __traits(getMember, item, __traits(identifier, representers[0])));
    }
    else
    {
        // expect an object. We want to deserialize the JSON data
        alias members = SerializableMembers!T;
        // TODO use bit array instead and core.bitop
        //size_t[(members.length + (size_t.sizeof * 8 - 1)) / size_t.sizeof / 8] visited;
        bool[members.length] visited;

        // any members that are optional, mark as already visited
        static foreach(idx, m; members)
        {
            static if(hasUDA!(__traits(getMember, T, m), Optional()))
                visited[idx] = true;
        }

        auto jsonItem = tokenizer.next;
        jsonExpect(jsonItem, JSONToken.ObjectStart, "Parsing " ~ T.stringof);

        // look at each string, then parse the given values
        jsonItem = tokenizer.next();
        static if(members.length)
        {
            while(jsonItem.token != JSONToken.ObjectEnd)
            {
                if(jsonItem.token == JSONToken.Comma)
                {
                    jsonItem = tokenizer.next();
                    continue;
                }

                jsonExpect(jsonItem, JSONToken.String, "Expecting member name of " ~ T.stringof);
                auto name = jsonItem.data(tokenizer.chain);

                jsonItem = tokenizer.next();
                jsonExpect(jsonItem, JSONToken.Colon, "Expecting colon when parsing " ~ T.stringof);
OBJ_MEMBER_SWITCH:
                switch(name)
                {
                    static foreach(i, m; members)
                    {
                    case m:
                        tokenizer.deserializeImpl(__traits(getMember, item, m));
                        visited[i] = true;
                        break OBJ_MEMBER_SWITCH;
                    }

                default:
                    import std.format : format;
                    throw new Exception(format("No member named '%s' in type `%s`", name, T.stringof));
                }
                tokenizer.releaseParsed();
                jsonItem = tokenizer.next();
            }
            // ensure all members visited
            import std.algorithm : canFind, map, filter;
            if(visited[].canFind(false))
            {
                // this is a bit ugly, but gives a nicer message.
                static immutable marr = [members];
                import std.format;
                import std.range : enumerate;
                throw new Exception(format("The following members of `%s` were not specified: `%-(%s` `%)`", T.stringof, visited[].enumerate.filter!(a => !a[1]).map!(a => marr[a[0]])));
            }
        }
        else
        {
            // no members, expect an object end
            jsonExpect(jsonItem, JSONToken.ObjectEnd, "Expecting end of memberless object " ~ T.stringof);
        }
    }
}

private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (isInstanceOf!(JSONValue, T))
{
    item = tokenizer.parseJSON!(typeof(T.str));
}

// if type is Nullable, first check for JSONToken.Null, and if not, try and
// parse real item.
private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (isInstanceOf!(Nullable, T))
{
    if(tokenizer.peek == JSONToken.Null)
    {
        item.nullify;
        // skip the null value
        cast(void)tokenizer.next;
    }
    else
    {
        typeof(item.get()) result;
        deserializeImpl(tokenizer, result);
        item = result;
    }
}

// Given a JSON tokenizer, deserialize the given type from the JSON data.
T deserialize(T, JT)(ref JT tokenizer) if (isInstanceOf!(JSONTokenizer, JT))
{
    T result;
    deserializeImpl(tokenizer, result);
    return result;
}

T deserialize(T, Chain)(auto ref Chain c) if (isIopipe!Chain)
{
    enum shouldReplaceEscapes = is(typeof(chain.window[0] = chain.window[1]));
    auto tokenizer = c.jsonTokenizer!(shouldReplaceEscapes);
    return tokenizer.deserialize!T;
}

void deserialize(T, JT)(ref JT tokenizer, ref T item) if (isInstanceOf!(JSONTokenizer, JT))
{
    deserializeImpl(tokenizer, item);
}

void deserialize(T, Chain)(auto ref Chain c, ref T item) if (isIopipe!Chain)
{
    enum shouldReplaceEscapes = is(typeof(chain.window[0] = chain.window[1]));
    auto tokenizer = c.jsonTokenizer!(shouldReplaceEscapes);
    return tokenizer.deserialize(item);
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

void serializeImpl(T, Char)(scope void delegate(const(Char)[]) w, ref T val) if (__traits(isStaticArray, T))
{
    auto arr = val;
    serializeImpl(w, val[]);
}

void serializeImpl(T, Char)(scope void delegate(const(Char)[]) w, ref T val) if (isDynamicArray!T && !isSomeString!T)
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

void serializeImpl(T, Char)(scope void delegate(const(Char)[]) w, ref T val) if (isSomeString!T)
{
    w(`"`);
    put(w, val);
    w(`"`);
}

void serializeImpl(T, Char)(scope void delegate(const(Char)[]) w, ref T val) if (is(T == struct))
{
    static if(isInstanceOf!(Nullable, T))
    {
        if(val.isNull)
            w("null");
        else
            serializeImpl(w, val.get);
    }
    else static if(isInstanceOf!(JSONValue, T))
    {
        with(JSONType) final switch(val.type)
        {
        case Integer:
            serializeImpl(w, val.integer);
            break;
        case Floating:
            serializeImpl(w, val.floating);
            break;
        case String:
            serializeImpl(w, val.str);
            break;
        case Array:
            serializeImpl(w, val.array);
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
                    w(n);
                    w(`" : `);
                    serializeImpl(w, v);
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
    else static if(getSymbolsByUDA!(T, SerializeAs()).length > 0)
    {
        alias representers = getSymbolsByUDA!(T, SerializeAs());
        // serialize as the single item
        static assert(representers.length == 1, "Only one field can be used to represent an object");
        serializeImpl(w, __traits(getMember, val, __traits(identifier, representers[0])));
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
        // serialize as an object
        w("{");
        bool first = true;
        static foreach(n; SerializableMembers!T)
        {
            if(first)
                first = false;
            else
                w(", ");
            w(`"`);
            w(n);
            w(`" : `);
            serializeImpl(w, __traits(getMember, val, n));
        }
        w("}");
    }
}

void serializeImpl(T, Char)(scope void delegate(const(Char)[]) w, ref T val) if (isNumeric!T)
{
    import std.format;
    formattedWrite(w, "%s", val);
}

void serializeImpl(Char)(scope void delegate(const(Char)[]) w, bool val)
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
// if T does not evaluate to an array or object type, then it wraps it in a
// single-element array to make the result a valid JSON string.
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

    static if(isInstanceOf!(JSONValue, T))
    {
        // json value. Check to see if it's an object or array. If not, write the 
        bool needClosingBrace =
            !(val.type == JSONType.Obj || val.type == JSONType.Array);
        if(needClosingBrace)
            w("[");
    }
    else static if(isInstanceOf!(Nullable, T))
    {
        if(val.isNull)
        {
            w("[null]");
            return result;
        }
        else
            return chain.serialize!relOnWrite(val, offset);
    }
    else static if(is(T == struct) || isArray!T)
    {
        enum needClosingBrace = false;
    }
    else
    {
        enum needClosingBrace = true;
        w("[");
    }

    // serialize the item, recursively
    serializeImpl(&w, val);

    if(needClosingBrace)
        w("]");
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
    assert(str1 == "[1]");
    int item1;
    auto strpipe = str1;
    strpipe.deserialize(*(cast(int[1]*)&item1));
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
