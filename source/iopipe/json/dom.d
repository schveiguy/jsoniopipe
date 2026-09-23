/**
 * Mechanism to parse JSON data into a JSON object tree. Some aspects borrowed
 * from std.json.
 */
module iopipe.json.dom;
import iopipe.json.parser;
public import iopipe.json.common;
import iopipe.traits;
import std.traits;

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
        JSONValue[immutable(SType)] object;
        SType str;
        bool boolean;
    }
}

/** Parse into JSONValue tree until end of root object.
 * Throws:
 * 	JSONIopipeException on parser error.
 */
deprecated("Use iopipe.json.serialize.deserialize directly.")
auto parseJSON(Tokenizer)(ref Tokenizer tokenizer, ReleasePolicy relPol = ReleasePolicy.afterMembers) if (isInstanceOf!(JSONTokenizer, Tokenizer))
{
    alias SType = typeof(tokenizer.Element.init.data());
    return parseJSON!SType(tokenizer, relPol);
}

deprecated("Use iopipe.json.serialize.deserialize directly.")
auto parseJSON(SType, Tokenizer)(ref Tokenizer tokenizer, ReleasePolicy relPol = ReleasePolicy.afterMembers) if (isInstanceOf!(JSONTokenizer, Tokenizer))
{
    import iopipe.json.serialize: deserialize, DefaultDeserializationPolicy;
    return tokenizer.deserialize!(JSONValue!SType)(DefaultDeserializationPolicy!()(relPol: relPol));
}

deprecated("Use iopipe.json.serialize.deserialize directly.")
auto parseJSON(SType = void, Chain)(Chain chain) if (isIopipe!Chain && is(SType == void))
{
    return parseJSON!(WindowType!Chain)(chain);
}

deprecated("Use iopipe.json.serialize.deserialize directly.")
auto parseJSON(SType, Chain)(Chain chain) if (isIopipe!Chain)
{
    import iopipe.json.serialize: deserialize, DefaultDeserializationPolicy;
    return chain.deserialize!(JSONValue!SType)(DefaultDeserializationPolicy!()());
}

deprecated("Use iopipe.json.serialize.serialize directly.")
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

deprecated
unittest
{
    auto jt = parseJSON(q"{{"a" : [1, 2.5, "x", true, false, null]}}");
    //printTree(jt);
    auto jt2 = parseJSON!(wstring)(q"{{"a" : [1, 2.5, "x\ua123", true, false, null]}}");
    //printTree(jt2);
}
