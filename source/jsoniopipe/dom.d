/**
 * Mechanism to parse JSON data into a JSON object tree. Some aspects borrowed from std.json.
 */
module jsoniopipe.dom;
import jsoniopipe.parser;
import iopipe.traits;

enum JSONType
{
    Integer,
    Floating,
    String,
    Obj,
    Array,
    Null,
    Bool,
}

struct JSONValue(SType)
{
    // basically a tagged union.
    JSONType type;
    union
    {
        long integer;
        real floating;
        JSONValue[] array;
        JSONValue[SType] object;
        SType str;
        bool boolean;
    }
}

template parseJSON(bool inPlace = false, bool duplicate = true, Chain)
{
    alias SType = WindowType!Chain;
    alias JT = JSONValue!SType;
    struct DOMParser
    {
        JSONTokenizer!(Chain, inPlace) parser;
        JT buildValue(JSONItem item)
        {
            switch (item.token) with (JSONToken)
            {
            case ObjectStart:
                return buildObject();
            case ArrayStart:
                return buildArray();
            case String:
                // See if we require copying.
                {
                    JT result;
                    result.type = JSONType.String;
                    if(item.hint == JSONParseHint.InPlace)
                    {
                        static if(!duplicate)
                        {
                            // can just slice the string
                            result.str = item.data(parser.chain);
                            return result;
                        }
                        else
                        {
                            // need to copy anyway, but much easier than replacing escapes
                            result.str = cast(typeof(result.str))item.data(parser.chain).dup;
                            return result;
                        }
                    }
                    else
                    {
                        // put the quotes back
                        item.offset--;
                        item.length += 2;

                        // re-parse, this time replacing escapes. This is so ugly...
                        auto newpipe = item.data(parser.chain).dup;
                        size_t pos = 0;
                        item.length = parseString(newpipe, pos, item.hint);
                        ++item.offset;
                        result.str = cast(typeof(result.str))item.data(newpipe);
                        return result;
                    }
                }
            case Number:
                {
                    // if it's an integer, parse as an integer. If not, parse as a float.
                    // TODO: really this should be done while parsing, not that hard.
                    import std.conv: parse;
                    JT result;
                    auto str = item.data(parser.chain);
                    if(item.hint == JSONParseHint.Int)
                    {
                        result.type = JSONType.Integer;
                        result.integer = parse!long(str);
                        return result;
                    }
                    else
                    {
                        // floating point or with exponent
                        result.type = JSONType.Floating;
                        result.floating = parse!real(str);
                        return result;
                    }
                }
            case Null:
                {
                    JT result;
                    result.type = JSONType.Null;
                    return result;
                }
            case True:
                {
                    JT result;
                    result.type = JSONType.Bool;
                    result.boolean = true;
                    return result;
                }
            case False:
                {
                    JT result;
                    result.type = JSONType.Bool;
                    result.boolean = false;
                    return result;
                }
            default:
                throw new Exception("Error in JSON data");
            }
        }
        JT buildObject()
        {
            auto item = parser.next();
            JT obj;
            obj.type = JSONType.Obj;
            while(item.token != JSONToken.ObjectEnd)
            {
                if(item.token == JSONToken.Comma)
                    continue;
                // the item must be a string
                auto name = buildValue(item);
                item = parser.next();
                if(item.token != JSONToken.Colon)
                    throw new Exception("Expected colon");
                item = parser.next();
                obj.object[name.str] = buildValue(item);
                // release any data if we are always duplicating strings
                static if(duplicate)
                    parser.release(parser.position);
                item = parser.next();
            }
            return obj;
        }

        JT buildArray()
        {
            auto item = parser.next();
            JT arr;
            arr.type = JSONType.Array;
            while(item.token != JSONToken.ArrayEnd)
            {
                if(item.token == JSONToken.Comma)
                {
                    item = parser.next();
                    continue;
                }
                arr.array ~= buildValue(item);
                static if(!inPlace) // release some data so we can give the buffer more space
                    parser.release(parser.position);
                item = parser.next();
            }
            return arr;
        }
    }

    auto parseJSON(Chain c)
    {
        auto dp = DOMParser(JSONTokenizer!(Chain, inPlace)(c));
        switch(dp.parser.next.token) with (JSONToken)
        {
        case ObjectStart:
            return dp.buildObject();
        case ArrayStart:
            return dp.buildArray();
        default:
            throw new Exception("Expected object or array");
        }
    }
}

void printTree(JT)(JT item)
{
    import std.stdio;
    final switch(item.type) with (JSONType)
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
        write(item.integer);
        break;
    case Floating:
        write(item.floating);
        break;
    case Null:
        write("null");
        break;
    case Bool:
        write(item.boolean);
        break;
    case String:
        writef(`"%s"`, item.str);
        break;
    }
}

unittest
{
    auto jt = parseJSON(q"{{"a" : [1, 2.5, "x", true, false, null]}}");
    //printTree(jt);
    auto jt2 = parseJSON!(false, false)(q"{{"a" : [1, 2.5, "x", true, false, null]}}");
    //printTree(jt2);
}
