import std.io;
import iopipe.refc: refCounted;
import iopipe.stream;
import iopipe.bufpipe;
import iopipe.textpipe;
import iopipe.valve;
import iopipe.json.parser;
import iopipe.buffer;

void main(string[] args)
{
    auto parser = File(args[1]).refCounted.bufd.assumeText.jsonTokenizer;

    //import std.mmfile;
    //scope mmf = new MmFile(args[1]);
    //auto x = cast(immutable(char)[])mmf[];
    //auto parser = x.jsonTokenizer!false;

    auto outputter = bufd!(char).push!(c => c
                                .encodeText!(UTFType.UTF8)
                                .outputPipe(File(1).refCounted));
    
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
            putStr(item.data(parser.chain));
            putStr("\"");
            break;
        case Number:
        case True:
        case False:
        case Null:
            putStr(item.data(parser.chain));
            break;
        case Error:
        case EOF:
            putStr("***ERROR***");
            return;
	case Symbol, Comment, Space:
	    break;
        }

        item = parser.next;
    }
    putStr("\n");
}
