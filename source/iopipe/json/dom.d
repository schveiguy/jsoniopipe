/**
 * Mechanism to parse JSON data into a JSON object tree. Some aspects borrowed
 * from std.json.
 */
module iopipe.json.dom;
import iopipe.json.parser;
public import iopipe.json.common;
import iopipe.traits;
import std.traits;
import std.conv : to;

enum JSONType
{
    Integer,        // Now a hint only, stored as string
    Floating,       // Now a hint only, stored as string
    String,
    Obj,
    Array,
    Null,
    Bool,
    StringSSO,      // Small string optimization for strings
    NumberSSO,      // Small string optimization for numbers
}

struct JSONValue
{
    // Tagged union
    JSONType type;
    
    // Small string optimization - 16 bytes should cover many common cases
    private enum SSOSize = 16;
    
    union
    {
        // String storage (either SSO or allocated)
        struct {
            union {
                char[SSOSize] sso;
                string allocatedString;
            }
            // For SSO types, first byte can indicate length used
            ubyte ssoLength;
        }
        
        // Original value types
        JSONValue[] array;
        JSONValue[string] object;
        bool boolean;
    }
    
    // Get the string representation of the value
    string stringForm() const
    {
        with(JSONType) final switch(type)
        {
            case String:
                return allocatedString;
            case StringSSO:
            case NumberSSO:
                return sso[0..ssoLength];
            case Integer:
            case Floating:
                return allocatedString;
            case Obj:
            case Array:
            case Null:
            case Bool:
                throw new JSONIopipeException("Cannot get string form of non-string/non-number type");
        }
    }
    
    // Convert the value to type T
    T get(T)() const
    {
        import std.conv : to;
        
        static if (is(T == string))
        {
            return stringForm();
        }
        else static if (is(T == bool))
        {
            if (type == JSONType.Bool)
                return boolean;
            else
                return stringForm().to!bool;
        }
        else static if (isNumeric!T)
        {
            // For numeric types, convert from the string representation
            if (type == JSONType.Integer || type == JSONType.Floating || 
                type == JSONType.NumberSSO)
                return stringForm().to!T;
            else
                throw new JSONIopipeException("Cannot convert non-numeric JSON value to numeric type");
        }
        else
        {
            // For other types, attempt generic conversion
            return stringForm().to!T;
        }
    }
    
    // Create a JSONValue from a string, using SSO when possible
    static JSONValue fromString(string value, bool isNumber = false)
    {
        JSONValue result;
        
        if (value.length < SSOSize)
        {
            // Use SSO
            result.type = isNumber ? JSONType.NumberSSO : JSONType.StringSSO;
            result.ssoLength = cast(ubyte)value.length;
            result.sso[0..value.length] = value[];
        }
        else
        {
            // Use allocated string
            result.type = isNumber ? JSONType.Floating : JSONType.String;
            result.allocatedString = value.idup;
        }
        
        return result;
    }
    
    // Convenience to create numeric JSONValue
    static JSONValue number(string value, bool isInteger = false)
    {
        auto result = fromString(value, true);
        // Set the hint type if it's allocated
        if (result.type == JSONType.Floating && isInteger)
            result.type = JSONType.Integer;
        return result;
    }
}

private JSONValue buildValue(Tokenizer)(ref Tokenizer parser, JSONItem item, ReleasePolicy relPol)
{
    import std.conv;

    with(JSONToken) switch (item.token)
    {
    case ObjectStart:
        return parser.buildObject(relPol);
    case ArrayStart:
        return parser.buildArray(relPol);
    case String:
        {
            // Extract string and use SSO when possible
            auto strData = extractString!string(item, parser.chain);
            return JSONValue.fromString(strData);
        }
    case Number:
        {
            // Store the number as string
            auto numStr = item.data(parser.chain).to!string;
            return JSONValue.number(numStr, item.hint == JSONParseHint.Int);
        }
    case Null:
        {
            JSONValue result;
            result.type = JSONType.Null;
            return result;
        }
    case True:
        {
            JSONValue result;
            result.type = JSONType.Bool;
            result.boolean = true;
            return result;
        }
    case False:
        {
            JSONValue result;
            result.type = JSONType.Bool;
            result.boolean = false;
            return result;
        }
    default:
        throw new JSONIopipeException("Error in JSON data");
    }
}

private JSONValue buildObject(Tokenizer)(ref Tokenizer parser, ReleasePolicy relPol)
{
    auto item = parser.next();
    JSONValue obj;
    obj.type = JSONType.Obj;
    while(item.token != JSONToken.ObjectEnd)
    {
        if(item.token == JSONToken.Comma)
        {
            item = parser.next;
            continue;
        }
        // the item must be a string
        assert(item.token == JSONToken.String);
        auto nameVal = parser.buildValue(item, relPol);
        string name = nameVal.stringForm().idup;
        
        item = parser.next();
        // should always be colon
        assert(item.token == JSONToken.Colon);
        item = parser.next();
        obj.object[name] = parser.buildValue(item, relPol);
        // release any parsed data.
        if(relPol == ReleasePolicy.afterMembers)
            parser.releaseParsed();
        item = parser.next();
    }
    return obj;
}

private JSONValue buildArray(Tokenizer)(ref Tokenizer parser, ReleasePolicy relPol)
{
    auto item = parser.next();
    JSONValue arr;
    arr.type = JSONType.Array;
    while(item.token != JSONToken.ArrayEnd)
    {
        arr.array ~= parser.buildValue(item, relPol);
        if(relPol == ReleasePolicy.afterMembers)
            parser.releaseParsed();
        item = parser.next();
        if(item.token == JSONToken.Comma)
            item = parser.next();
    }
    return arr;
}

/** Parse into JSONValue tree until end of root object.
 * Throws:
 * 	JSONIopipeException on parser error.
 */
auto parseJSON(Tokenizer)(ref Tokenizer tokenizer, ReleasePolicy relPol = ReleasePolicy.afterMembers) if (isInstanceOf!(JSONTokenizer, Tokenizer))
{
    return parseJSON!(string)(tokenizer, relPol);
}

auto parseJSON(SType, Tokenizer)(ref Tokenizer tokenizer, ReleasePolicy relPol = ReleasePolicy.afterMembers) if (isInstanceOf!(JSONTokenizer, Tokenizer))
{
    auto item = tokenizer.next();
    auto result = tokenizer.buildValue(item, relPol);
    if(relPol == ReleasePolicy.afterMembers)
        tokenizer.releaseParsed();
    return result;
}

auto parseJSON(SType = void, Chain)(Chain chain) if (isIopipe!Chain && is(SType == void))
{
    return parseJSON!(string)(chain);
}

auto parseJSON(SType, Chain)(Chain chain) if (isIopipe!Chain)
{
    enum shouldReplaceEscapes = is(typeof(chain.window[0] = chain.window[1]));
    auto tokenizer = (chain).jsonTokenizer!(ParseConfig(shouldReplaceEscapes));
    return tokenizer.parseJSON!SType(ReleasePolicy.afterMembers);
}

void printTree(JT)(JT item)
{
    import std.stdio;
    with (JSONType)
    {
        final switch(item.type)
        {
        case Obj:
            {
                write("{");
                bool first = true;
                foreach(n, v; item.object)
                {
                    if(first)
                        first = false;
                    else
                        write(", ");
                    writef(`"%s" : `, n);
                    printTree(v);
                }
                write("}");
            }
            break;
        case Array:
            {
                write("[");
                bool first = true;
                foreach(v; item.array)
                {
                    if(first)
                        first = false;
                    else
                        write(", ");
                    printTree(v);
                }
                write("]");
            }
            break;
        case Integer:
        case NumberSSO:
            // For consistency, just display the string form for numbers
            write(item.stringForm());
            break;
        case Floating:
            write(item.stringForm());
            break;
        case Null:
            write("null");
            break;
        case Bool:
            write(item.boolean);
            break;
        case String:
        case StringSSO:
            writef(`"%s"`, item.stringForm());
            break;
        }
    }
}

unittest
{
    auto jt = parseJSON(q"{{"a" : [1, 2.5, "x", true, false, null]}}");
    //printTree(jt);
    
    // Test numbers that would be problematic for native types
    auto bigNumber = parseJSON(q"{{"bigint": 10000000000000000000}}");
    assert(bigNumber.object["bigint"].stringForm() == "10000000000000000000");
    
    // Test small string optimization
    auto smallString = parseJSON(q"{{"small": "abc"}}");
    assert(smallString.object["small"].type == JSONType.StringSSO);
    assert(smallString.object["small"].stringForm() == "abc");
    
    // Test the get() method for type conversion
    auto numberTest = parseJSON(q"{{"num": 42, "str": "hello"}}");
    assert(numberTest.object["num"].get!int() == 42);
    assert(numberTest.object["str"].get!string() == "hello");
}