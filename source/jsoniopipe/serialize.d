module jsoniopipe.serialize;
import jsoniopipe.parser;
import jsoniopipe.dom;
import iopipe.traits;
import std.range.primitives;

import std.traits;
import std.typecons : Nullable;

private void jsonExpect(ref JSONItem item, JSONToken expectedToken, string msg, string file = __FILE__, size_t line = __LINE__)
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

private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (isDynamicArray!T && !isSomeString!T)
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
    enum SerializableMembers = __traits(allMembers, T);
}

private void deserializeImpl(T, JT)(ref JT tokenizer, ref T item) if (is(T == struct) && !isInstanceOf!(JSONValue, T) && !isInstanceOf!(Nullable, T))
{
    // expect an object. We want to deserialize the JSON data
    alias members = SerializableMembers!T;
    // TODO use bit array instead and core.bitop
    //size_t[(members.length + (size_t.sizeof * 8 - 1)) / size_t.sizeof / 8] visited;
    bool[members.length] visited;

    auto jsonItem = tokenizer.next;
    jsonExpect(jsonItem, JSONToken.ObjectStart, "Parsing " ~ T.stringof);

    // look at each string, then parse the given values
    jsonItem = tokenizer.next();
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
        // this is a big ugly, but necessary.
        static immutable marr = [members];
        import std.format;
        import std.range : enumerate;
        throw new Exception(format("The following members of `%s` were not specified: `%(%s` `%)`", T.stringof, visited[].enumerate.filter!(a => a[1]).map!(a => marr[a[0]])));
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

T deserialize(T, Chain)(Chain c) if (isIopipe!Chain)
{
    enum shouldReplaceEscapes = is(typeof(chain.window[0] = chain.window[1]));
    auto tokenizer = c.jsonTokenizer!(shouldReplaceEscapes);
    return tokenizer.deserialize!T;
}

unittest
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
}
