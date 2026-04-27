import std.io;
import std.io.driver;
import iopipe.refc: refCounted;
import iopipe.bufpipe;
import iopipe.textpipe;
import iopipe.valve;
import iopipe.buffer;
import iopipe.json.parser;
import iopipe.json.formatter; 

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
    auto fmt = outputter.jsonFormatter;

    auto item = parser.next;
    while(item.token != JSONToken.EOF)
    {
        with(JSONToken) final switch(item.token)
        {
        case ObjectStart:
            fmt.beginObject();
            break;
        case ArrayStart:
            fmt.beginArray();
            break;
        case ObjectEnd:
        case ArrayEnd:
            fmt.endAggregate();
            break;
        case Comma:
            fmt.addComma();
            break;
        case Colon:
            fmt.addColon();
            break;
        case String:
            fmt.beginString();
            fmt.addStringData(item.data);
            fmt.endString();
            break;
        case Number:
            fmt.addNumber(item.data);
            break;
        case True:
            fmt.addKeywordValue(KeywordValue.True);
            break;
        case False:
            fmt.addKeywordValue(KeywordValue.False);
            break;
        case Null:
            fmt.addKeywordValue(KeywordValue.Null);
            break;
        case Error:
            throw new Exception("Error!");
            return;
        case Symbol, Comment, Space, EOF:
            assert(0);
        }

        item = parser.next;
        fmt.flushWritten();
    }
}
