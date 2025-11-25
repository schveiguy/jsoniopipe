import std.io;
import std.io.driver;
import iopipe.refc: refCounted;
import iopipe.stream;
import iopipe.bufpipe;
import iopipe.textpipe;
import iopipe.valve;
import iopipe.json.parser;
import iopipe.buffer;
import iopipe.json.serialize;
import iopipe.json.dom;

void main(string[] args)
{
    auto parser = File(args[1]).refCounted.bufd.assumeText.jsonTokenizer;

    //import std.mmfile;
    //scope mmf = new MmFile(args[1]);
    //auto x = cast(immutable(char)[])mmf[];
    //auto parser = x.jsonTokenizer!false;

    auto outputter = bufd!(char).push!(c => c
                                .encodeText!(UTFType.UTF8)
                                .outputPipe(File(stdout).refCounted));
    /*
    int spacing;
    enum indent = 4;
    void putIndent()
    {
        outputter.ensureElems(indent * spacing + 1);
        outputter.window[0] = '\n';
        outputter.window[1 .. 1+indent*spacing] = ' ';
        outputter.release(indent * spacing + 1);
    }

    void putStr(const(char)[] s)
    {
        outputter.ensureElems(s.length);
        outputter.window[0 .. s.length] = s;
        outputter.release(s.length);
    }

    auto item = parser.next;
    while(item.token != JSONToken.EOF)
    {
        with(JSONToken) final switch(item.token)
        {
        case ObjectStart:
            ++spacing;
            putStr("{");
            putIndent();
            break;
        case ObjectEnd:
            --spacing;
            putIndent();
            putStr("}");
            break;
        case ArrayStart:
            ++spacing;
            putStr("[");
            putIndent();
            break;
        case ArrayEnd:
            --spacing;
            putIndent();
            putStr("]");
            break;
        case Comma:
            putStr(",");
            putIndent();
            break;
        case Colon:
            putStr(": ");
            break;
        case String:
            putStr("\"");
            putStr(item.data);
            putStr("\"");
            break;
        case Number:
        case True:
        case False:
        case Null:
            putStr(item.data);
            break;
        case Error:
        case EOF:
            putStr("***ERROR***");
            return;
	case Symbol, Comment, Space:
	    assert(0);
        }

        item = parser.next;
    }
    putStr("\n");
    */

    JSONValue!string jsonData;
    deserialize(parser, jsonData);
    auto formatter = JSONFormatter!(typeof(outputter))(outputter);
    
    serializeWithFormatter(formatter, jsonData);
    formatter.addWhitespace("\n");
    formatter.flushWritten();
}

void serializeWithFormatter(Formatter, T)(ref Formatter formatter, T val) 
{
    static if (is(T == JSONValue!string))
    {
        with(JSONType) final switch(val.type)
        {
        case Obj:
            formatter.beginObject();
            bool first = true;
            foreach(key, value; val.object)
            {
                formatter.addMember(key);
                serializeWithFormatter(formatter, value);
            }
            formatter.endObject();
            break;
        case Array:
            formatter.beginArray();
            foreach(item; val.array)
            {
                formatter.beginArrayValue();
                serializeWithFormatter(formatter, item);
            }
            formatter.endArray();
            break;
        case String:
            formatter.beginString();
            formatter.addStringData(val.str);
            formatter.endString();
            break;
        case Integer:
            formatter.addNumericData(val.integer);
            break;
        case Floating:
            formatter.addNumericData(val.floating);
            break;
        case Bool:
            if(val.boolean)
                formatter.addKeywordValue(KeywordValue.True);
            else
                formatter.addKeywordValue(KeywordValue.False);
            break;
        case Null:
            formatter.addKeywordValue(KeywordValue.Null);
            break;
        }
    }
}
