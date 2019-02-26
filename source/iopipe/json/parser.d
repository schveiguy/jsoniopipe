/**
Copyright: Copyright Steven Schveighoffer 2017
License:   Boost License 1.0. (See accompanying file LICENSE_1_0.txt or copy at
           http://www.boost.org/LICENSE_1_0.txt)
Authors: Steven Schveighoffer
*/
module iopipe.json.parser;
import iopipe.traits;
import iopipe.bufpipe;
import std.range.primitives;
import std.traits;

/**
 * Tokens as parsed from the stream. This indicates what the next token is
 * supposed to be, and doesn't necessarily validate the next item is in the
 * correct format.
 */
enum JSONToken : char
{
    ObjectStart = '{', /// {
    ObjectEnd = '}',   /// }
    String = '"',      /// "
    Colon = ':',       /// :
    Comma = ',',       /// ,
    ArrayStart = '[',  /// [
    ArrayEnd = ']',    /// ]
    Number = '0',      /// - or 0-9
    True = 't',        /// t
    False = 'f',       /// f
    Null = 'n',        /// n
    EOF = '\0',         /// end of stream
    Error = 0xff,       /// unexpected data in stream
}

/**
 * Hint on how to parse this value. If the item is a Number or String, then
 * this gives hints on how to parse it. It's a bitfield, with the first bit
 * defining integer or float, the second bit defining 
 */
enum JSONParseHint : ubyte
{
    InPlace, /// Item is not a value, or is a string that can be used in place.
    Int,     /// Item is integral (no decimal or exponent).
    Float,   /// number has decimal place, but no exponent
    Exp,     /// number has exponent (and is float).
    Escapes, /// string has escapes
}

/**
 * Returns: `true` if the token can be used in place of a "value". Useful for
 * validation.
 */
bool isValue(JSONToken token) pure @safe nothrow
{
    switch(token) with(JSONToken)
    {
    case ObjectStart:
    case ArrayStart:
    case String:
    case Number:
    case True:
    case False:
    case Null:
        return true;
    default:
        return false;
    }
}

/**
 * Search for the next token in the iopipe c, ignoring white space. This does
 * not validate data, it simply searches for the beginning of a valid JSON
 * token. Since each token is definitive based on the first character, only the
 * first character is looked at.
 *
 * Params:
 *    c = The iopipe in which to search for tokens. extend may be used on the
 *    iopipe to find the next token.
 *    pos = Current position in the window. Taken via ref so it can be updated
 *    to point at the new token.
 * Returns: The expected type of token at the new position.
 */
JSONToken jsonTok(Chain)(ref Chain c, ref size_t pos) if (isIopipe!Chain && isSomeChar!(ElementEncodingType!(WindowType!Chain)))
{
    import std.ascii: isWhite;
    // strip any leading whitespace. If no data is left, we need to extend
    while(true)
    {
        while(pos < c.window.length && isWhite(c.window[pos]))
            ++pos;
        if(pos < c.window.length)
            break;
        if(c.extend(0) == 0)
            return JSONToken.EOF;
    }

    immutable cur = c.window[pos];
    switch(cur) with(JSONToken)
    {
    case ObjectStart:
    case ObjectEnd:
    case String:
    case Colon:
    case Comma:
    case ArrayStart:
    case ArrayEnd:
    case True:
    case False:
    case Null:
        return cast(JSONToken)cur;
    case '-':
    case '0': .. case '9':
        return Number;
    default:
        return Error;
    }
}

/**
 * JSON item from a specific iopipe. This is like a slice into the iopipe, but
 * only contains offsets so as to make it easy to manipulate. No whitespace is
 * included. Strings do not include the surrounding quotes.
 */
struct JSONItem
{
    /**
     * If the token is a standard token, offset into the current iopipe window
     * of where this item begins. If you release data from the beginning, this
     * can be updated manually by subtracting the number of items released.
     */
    size_t offset;

    /**
     * Length of the item.
     */
    size_t length; // length of the item in the stream.

    /**
     * The type of token this item contains.
     */
    JSONToken token;

    /**
     * A parsing hint on what is inside the item. This is determined for
     * Strings or Numbers during validation. Other token types do not set this
     * member to anything.
     */
    JSONParseHint hint;

    /**
     * Given an iopipe from which this token came, returns the exact window
     * data for the item.
     */
    auto data(Chain)(ref Chain c)
    {
        return c.window[offset .. offset + length];
    }
}

/**
 * Parse and validate a string from an iopipe. This functions serves several
 * purposes. First, it determines how long the string actually is, so it can be
 * properly parsed out. Second, it verifies any escapes inside the string.
 * Third, if requested, it can replace any escapes with the actual characters
 * in-place, so the string result can just be used. Note that this does not
 * verify any UTF code units are valid. However, any unicode escapes using the
 * `\uXXXX` sequence are validated.
 * 
 * Params:
 *     replaceEscapes = If true, then any encountered escapes will be replaced with the actual utf characters they represent, properly encoded for the given element type.
 *     c = The iopipe to parse the string from. If the end of the string is not
 *     present in the current window, it will be extended until the end is
 *     found.
 *     pos = Upon calling, this should be the position in the stream window
 *     where the first quotation mark is for this string. Upon exit, if
 *     successfully parsed, pos is updated to the position just after the final
 *     quotation mark. If an error occurs, pos should be at the spot where the
 *     invalid sequence occurred.
 *     hint = Set to `InPlace` if the string no longer contains escapes (either
 *     there weren't any, or they have been replaced). Set to `Escapes` if the
 *     string still contains escapes that need to be properly parsed.
 * Returns: number of elements in the resulting string if successfully parsed,
 * or -1 if there is an error. Note that if escapes are not replaced, then this
 * number includes the escape character sequences as-is.
 */
int parseString(bool replaceEscapes = true, Chain)(ref Chain c, ref size_t pos, ref JSONParseHint hint)
{
    hint = JSONParseHint.InPlace;
    // the first character must be a quote
    auto src = c.window;
    if(src.length == 0 || src[pos] != '"')
        return -1;
    ++pos;

    immutable origPos = pos;
    static if(replaceEscapes)
        auto targetPos = pos;
    bool isEscaped = false;
    wchar surrogate;
    while(true)
    {
        if(pos == src.length)
        {
            // need more data from the pipe
            if(c.extend(0) == 0)
                // EOF.
                return -1;
            src = c.window;
        }
        auto elem = src[pos];
        if(isEscaped)
        {
            isEscaped = false;
            if(elem == 'u') // unicode sequence. 
            {
                // ensure there are at least 4 characters available.
                ++pos;
                if(pos + 4 > src.length)
                {
                    c.ensureElems(pos + 4);
                    // may need to re-assign src.
                    src = c.window;
                    if(pos + 4 > src.length)
                    {
                        // invalid sequence.
                        pos = src.length;
                        return -1;
                    }
                }

                // parse the hex chars
                import std.conv: parse;
                auto chars = src[pos .. pos + 4];

                wchar value = parse!ushort(chars, 16);
                pos += 4;
                if(chars.length)
                {
                    // some characters not proper hex
                    pos -= chars.length;
                    return -1;
                }
                alias Char = typeof(src[0]);

                static if(replaceEscapes)
                {
                    // function to encode a dchar into the target stream.
                    void enc(dchar d)
                    {
                        // insert the given dchar into the stream
                        static if(is(Char == dchar))
                        {
                            src[targetPos++] = d;
                        }
                        else static if(is(Char == wchar))
                        {
                            // this only happens if we have a dchar cast
                            // from a non-surrogate wchar. So cheat and just
                            // copy it.
                            src[targetPos++] = cast(wchar)d;
                        }
                        else // char
                        {
                            // potentially need to encode it. Most of the
                            // time, anyone using the \u escape sequence is
                            // not going to be encoding ascii data. So
                            // don't worry about that shortcut.
                            import std.utf : encode;
                            char[4] data;
                            foreach(i; 0 .. encode(data, d))
                                src[targetPos++] = data[i];
                        }
                    }
                }

                // if we have a surrogate pair cached from the last
                // element parsed, then this must be the matching pair.
                if(surrogate != wchar.init)
                {
                    // need to parse out this into a dchar. First,
                    // determine that they match.
                    if(value < 0xdc00 || value > 0xdfff)
                        // invalid sequence
                        return -1;

                    static if(replaceEscapes)
                    {
                        // valid sequence, put it into the stream.
                        static if(is(Char == wchar))
                        {
                            // just copy the two surrogates to the stream
                            src[targetPos++] = surrogate;
                            src[targetPos++] = value;
                        }
                        else
                        {
                            // convert to dchar
                            dchar converted = ((surrogate & 0x3ff) << 10) + (value & 0x3ff);
                            enc(converted);
                        }
                    }
                    // reset the surrogate pair
                    surrogate = wchar.init;
                }
                else
                {
                    if(value >= 0xd800 && value <= 0xdbff)
                    {
                        // this is the first half of a surrogate pair
                        surrogate = value;
                    }
                    else
                    {
                        if(value >= 0xdc00 && value <= 0xdfff)
                        {
                            // second surrogate pair, but we didn't get
                            // a first one. Error.
                            return -1;
                        }
                        // need to encode this into the stream
                        static if(replaceEscapes)
                            enc(value);
                    }
                }
            }
            else
            {
                static if(replaceEscapes)
                {
                    switch(elem)
                    {
                    case '\\':
                    case '/':
                    case '"':
                        src[targetPos++] = elem;
                        break;
                    case 'n':
                        src[targetPos++] = '\n';
                        break;
                    case 'b':
                        src[targetPos++] = '\b';
                        break;
                    case 'f':
                        src[targetPos++] = '\f';
                        break;
                    case 'r':
                        src[targetPos++] = '\r';
                        break;
                    case 't':
                        src[targetPos++] = '\t';
                        break;
                    default:
                        // unknown escape
                        return -1;
                    }
                }
                else
                {
                    // just make sure it's a valid escape character
                    switch(elem)
                    {
                    case '\\': case '/': case 'n': case 'b':
                    case 'f': case 'r': case 't':
                        break;
                    default:
                        return -1;
                    }
                }
                ++pos;
            }
        }
        else if(elem == '\\')
        {
            static if(!replaceEscapes)
                hint = JSONParseHint.Escapes;
            isEscaped = true;
            ++pos;
        }
        else if(surrogate != wchar.init)
        {
            // we were expecting another surrogate pair, error.
            return -1; 
        }
        else if(elem == '"')
        {
            // finished
            ++pos;
            static if(replaceEscapes)
                return cast(int)(targetPos - origPos);
            else
                return cast(int)(pos - origPos - 1);
        }
        else
        {
            static if(replaceEscapes)
            {
                // simple copy
                if(targetPos != pos)
                    src[targetPos] = elem;
                ++targetPos;
            }
            ++pos;
        }
    }
}

unittest
{
    void testParse(bool replaceEscape, C)(C[] jsonString, bool shouldFail, JSONParseHint expectedHint = JSONParseHint.InPlace, int expectedResult = -1, const(C)[] expectedString = null)
    {
        size_t pos;
        JSONParseHint hint;
        if(expectedString == null)
            expectedString = jsonString[1 .. $-1].dup;
        auto result = parseString!replaceEscape(jsonString, pos, hint);
        if(shouldFail)
        {
            assert(result == -1, jsonString);
        }
        else
        {
            assert(result == (expectedResult < 0 ? jsonString.length - 2 : expectedResult), jsonString);
            assert(pos == jsonString.length, jsonString);
            assert(hint == expectedHint, jsonString);
            assert(jsonString[1 .. 1 + result] == expectedString, jsonString);
        }
    }

    testParse!false(q"{"abcdef"}", false);
    testParse!false(q"{"abcdef}", true);
    testParse!true(q"{"abcdef"}".dup, false);
    testParse!true(q"{"abcdef\n"}".dup, false, JSONParseHint.InPlace, 7, "abcdef\n");
    testParse!true(q"{"abcdef\ua123\n"}".dup, false, JSONParseHint.InPlace, 10, "abcdef\ua123\n");
    testParse!false(q"{"abcdef\ua123\n"}", false, JSONParseHint.Escapes);
}

/**
 * Parse/validate a number from the given iopipe. This is used to validate the
 * number follows the correct grammar from the JSON spec, and also to find out
 * how many elements in the stream are used for this number.
 *
 * Params:
 *     c = The iopipe the number is being parsed from.
 *     pos = Upon calling, the position in the iopipe window where this number
 *     should start. Upon exit, if successfully parsed, this is the position
 *     after the last number element. If there was a parsing error, this is the
 *     position where the parsing error occurred.
 *     hint = Indicates upon return whether this number is integral, floating
 *     point, or a floating point with exponent. This can be used to parse the
 *     correct type using standard parsing techniques. Note that no attempt is
 *     made to verify the number will fit within, or can be properly
 *     represented by any type.
 *
 * Returns: The number of elements in the iopipe that comprise this number, or
 * -1 if there was a parsing error.
 */
int parseNumber(Chain)(ref Chain c, ref size_t pos, ref JSONParseHint hint)
{
    auto src = c.window;
    immutable origPos = pos;
    enum state
    {
        begin,
        sign,
        leadingzero,
        anydigit1,
        decimal,
        anydigit2,
        exponent,
        expsign,
        anydigit3,
    }
    hint = JSONParseHint.Int;

    state s;
    while(true)
    {
        if(pos == src.length)
        {
            // need more data from the pipe
            if(c.extend(0) == 0) with(state)
            {
                // end of the item. However, not necessarily an error. Make
                // sure we are in a state that allows ending the number.
                if(s == leadingzero || s == anydigit1 || s == anydigit2 || s == anydigit3)
                    return cast(int)(pos - origPos); // finished.
                // error otherwise, the number isn't complete.
                return -1;
            }
            src = c.window;
        }
        auto elem = src[pos];
        final switch(s) with(state)
        {
        case begin:
            // only accept sign or digit
            if(elem == '-')
            {
                s = sign;
                break;
            }
            goto case sign;
        case sign:
            if(elem == '0')
                s = leadingzero;
            else if(elem >= '1' && elem <= '9')
                s = anydigit1;
            else
                // error
                return -1;
            break;
        case leadingzero:
            if(elem == '.')
            {
                hint = JSONParseHint.Float;
                s = decimal;
            }
            else if(elem == 'e' || elem == 'E')
            {
                hint = JSONParseHint.Exp;
                s = exponent;
            }
            else 
                return cast(int)(pos - origPos); // finished
            break;
        case anydigit1:
            if(elem >= '0' && elem <= '9')
                // stay in this state
                break;
            goto case leadingzero;
        case decimal:
            if(elem >= '0' && elem <= '9')
                s = anydigit2;
            else
                // error
                return -1;
            break;
        case anydigit2:
            if(elem >= '0' && elem <= '9')
                break;
            else if(elem == 'e' || elem == 'E')
            {
                hint = JSONParseHint.Exp;
                s = exponent;
            }
            else
                return cast(int)(pos - origPos); // finished
            break;
        case exponent:
            if(elem == '+' || elem == '-')
            {
                s = expsign;
                break;
            }
            goto case expsign;
        case expsign:
            if(elem >= '0' && elem <= '9')
                s = anydigit3;
            else
                // error
                return -1;
            break;
        case anydigit3:
            if(elem >= '0' && elem <= '9')
                break;
            else
                return cast(int)(pos - origPos); // finished
        }
        ++pos;
    }
    
    // all returns should happen in the infinite loop.
    assert(0);
}

unittest
{
    void testParse(string jsonString, bool shouldFail, JSONParseHint expectedHint = JSONParseHint.Int)
    {
        size_t pos;
        JSONParseHint hint;
        auto result = parseNumber(jsonString, pos, hint);
        if(shouldFail)
        {
            assert(result == -1, jsonString);
        }
        else
        {
            assert(result == jsonString.length, jsonString);
            assert(pos == jsonString.length, jsonString);
            assert(hint == expectedHint, jsonString);
        }
    }
    testParse("e1", true);
    testParse("0", false);
    testParse("12345", false);
    testParse("100.0", false, JSONParseHint.Float);
    testParse("0.1e-1", false, JSONParseHint.Exp);
    testParse("-0.1e-1", false, JSONParseHint.Exp);
    testParse("-.1e-1", true);
    testParse("123.", true);
    testParse("--123", true);
    testParse(".1", true);
    testParse("0.1e", true);
}

/**
 * Obtain one parsing item from the given iopipe. This has no notion of
 * context, so it does not actually validate the overall structure of the JSON
 * stream. It only confirms that the next item is a valid JSON item.
 *
 * Params:
 *     replaceEscapes = Boolean passed to string parser to specify how escapes
 *     should be handled. See parseString for details.
 *     c = iopipe from which to parse item. If needed, it may be extended.
 *     pos = Current position in the iopipe's window from which the next item
 *     should start. Leading whitespace is allowed.
 *
 * Returns: If the stream contains a valid JSON item, the details about that
 * item are returned. If the stream does not contain any more items, then EOF
 * is returned. If there is an error parsing data from the stream for any
 * reason, then Error is returned.
 *
 */
JSONItem jsonItem(bool replaceEscapes = true, Chain)(ref Chain c, ref size_t pos)
{
    // parse a json item out of the chain
    JSONItem result;
    result.token = jsonTok(c, pos);
    result.offset = pos;

    void validateToken(string expected)
    {
        if(pos + expected.length > c.window.length)
        {
            // need to extend
            c.ensureElems(pos + expected.length);
        }

        auto w = c.window[pos .. $];

        if(expected.length > w.length)
        {
            // error, cannot be valid json.
            result.offset = c.window.length;
            result.token = JSONToken.Error;
            return;
        }

        // can't use std.algorithm.equal here, because of autodecoding...
        foreach(i, c; expected)
        {
            if(w[i] != c)
            {
                // doesn't match
                result.offset = pos + i;
                result.token = JSONToken.Error;
                return;
            }
        }

        result.length = expected.length;
        pos += expected.length;
    }

    final switch(result.token) with (JSONToken)
    {
    case ObjectStart:
    case ObjectEnd:
    case Colon:
    case Comma:
    case ArrayStart:
    case ArrayEnd:
        result.length = 1;
        ++pos; // skip over the single character item
        break;
    case EOF:
    case Error:
        break; // no changes to result needed.
    case True:
        validateToken("true");
        break;
    case False:
        validateToken("false");
        break;
    case Null:
        validateToken("null");
        break;
    case String:
        // string
        {
            auto numChars = parseString!replaceEscapes(c, pos, result.hint);
            if(numChars < 0)
            {
                result.token = Error;
                result.length = pos - result.offset;
            }
            else
            {
                // skip over initial quote
                result.offset++;
                result.length = numChars;
            }
        }
        break;
    case Number:
        // ensure the number is correct.
        {
            auto numChars = parseNumber(c, pos, result.hint);
            if(numChars < 0)
            {
                result.token = Error;
                result.length = pos - result.offset;
            }
            else
            {
                result.length = numChars;
            }
        }
        break;
    }
    return result;
}

/**
 * An object used to parse JSON items from a given iopipe chain. As the items
 * are parsed, the structure of the JSON data is validated. Note that the data
 * returned is simply references to within the iopipe window.
 *
 * Each new item/token can be obtained by calling the `next` method.
 */
struct JSONTokenizer(Chain, bool replaceEscapes)
{
    import std.bitmanip : BitArray;

    /**
     * The iopipe source. Use this to parse the data returned. Do not call
     * chain.release directly, use the release method instead to make sure the
     * internal state is maintained.
     */
    Chain chain;

    private
    {
        private enum State : ubyte
        {
            Begin,  // next item should be either an Object or Array
            First,  // Just started a new object or array.
            Member, // Expect next member (name for object, value for array_
            Colon,  // Expect colon (Object only)
            Value,  // Expect value
            Comma,  // Expect comma or end of collection.
            End     // there shouldn't be any more items
        }

        // bit array indicates structure of JSON parser (nesting).
        // 0 = array, 1 = object
        BitArray stack;
        size_t stackLen;
        size_t pos;
        State state;
        bool inObj()
        {
            return stackLen == 0 ? false : stack[stackLen - 1];
        }

        void pushContainer(bool isObj)
        {
            if(stackLen == stack.length)
                stack ~= isObj;
            else
                stack[stackLen] = isObj;
            ++stackLen;
        }

        void popContainer()
        {
            state = (--stackLen == 0) ? State.End : State.Comma;
        }

        // caching items allows us to parse only once, yet review the items later.
        bool caching;
        JSONItem[] cache;
        size_t cIdx;
    }

    @property bool finished()
    {
        return state == State.End;
    }

    // start caching elements. When this is enabled, rewind will jump back to
    // the first element and replay from the cache instead of parsing. Make
    // sure to call endCache when you are done with the replay cache.
    void startCache()
    {
        caching = true;
        if(cIdx != 0)
        {
            // remove all the elements before the current one, or else the
            // rewind command will not work right.
            import std.algorithm.mutation : copy;
            copy(cache[cIdx .. $], cache[0 .. $ - cIdx]);
            cache = cache[0 .. $ - cIdx];
            cache.assumeSafeAppend;
            cIdx = 0;
        }
    }

    /**
     * stop caching elements (the cache will be freed when no longer needed)
     */
    void endCache()
    {
        caching = false;
        if(cIdx == cache.length)
        {
            // deallocate the cache.
            cache.length = 0;
            cache.assumeSafeAppend;
            cIdx = 0;
        }
    }

    // this specialized function will skip the current item, taking into
    // account the nested nature of JSON. The return value is the next JSONItem
    // after the skipped data.
    //
    // If at the beginning of the JSON stream, the entire JSON stream is parsed
    // until the end of the JSON data in the stream.
    //
    // If at a member name, colon, or value expected, the entire member is skipped.
    //
    // If at a comma, the comma is skipped.
    //
    // If an error is encountered, it is returned immediately
    //
    JSONItem skipItem()
    {
        size_t depth = 0;
        // parse until we see the stack length get less than our current depth,
        // until we see a comma, error, or end of stream.
        while(true)
        {
            auto item = next();
            with(JSONToken) switch(item.token)
            {
            case ObjectStart:
            case ArrayStart:
                ++depth;
                break;
            case ObjectEnd:
            case ArrayEnd:
                if(!depth)
                    // at the end of the current object
                    return item;
                --depth;
                break;
            case Comma:
                if(depth == 0)
                    return item;
                break;
            case Error:
            case EOF:
                return item;
            default:
                // everything else we ignore
                break;
            }
        }
    }

    // where are we in the buffer
    @property size_t position()
    {
        if(cIdx < cache.length)
            return cache[cIdx].offset;
        return pos;
    }

    /**
     * Obtain the next JSONItem from the stream.
     */
    JSONItem next()
    {
        if(cIdx < cache.length)
        {
            auto item = cache[cIdx++];
            if(cIdx == cache.length && !caching)
            {
                // done with the cache
                cache.length = 0;
                cache.assumeSafeAppend;
                cIdx = 0;
            }
            return item;
        }

        if(state == State.End)
            // return an EOF item, even if the stream is not done.
            return JSONItem(pos, 0, JSONToken.EOF);

        // else, not cached, parse item from the chain.
        auto item = chain.jsonItem!replaceEscapes(pos);

        final switch(state) with(JSONToken)
        {
        case State.Begin:
            // item needs to be an ObjectStart or ArrayStart
            if(item.token == ObjectStart || item.token == ArrayStart)
            {
                state = State.First;
                pushContainer(item.token == ObjectStart);
            }
            else
                item.token = Error;
            break;
        case State.First:
            // allow ending of the container
            if(item.token == (inObj ? ObjectEnd : ArrayEnd))
            {
                popContainer();
                break;
            }
            goto case State.Member;
        case State.Member:
            if(inObj)
            {
                if(item.token == String)
                    state = State.Colon;
                else
                    item.token = Error;
                break;
            }
            goto case State.Value;
        case State.Colon:
            // requires colon
            if(item.token == Colon)
                state = State.Value;
            else
                item.token = Error;
            break;
        case State.Value:
            if(item.token.isValue)
            {
                if(item.token == ObjectStart || item.token == ArrayStart)
                {
                    pushContainer(item.token == ObjectStart);
                    state = State.First;
                }
                else
                    state = State.Comma;
            }
            else
                item.token = Error;
            break;
        case State.Comma:
            // can end the object here, or get a comma
            if(item.token == (inObj ? ObjectEnd : ArrayEnd))
                popContainer();
            else if(item.token == Comma)
                state = State.Member;
            else
                item.token = Error;
            break;
        case State.End:
            // this is handled outside the switch statement
            assert(0);
        }

        if(caching)
        {
            cache ~= item;
            ++cIdx;
        }
        return item;
    }

    void rewind()
    {
        assert(caching);
        cIdx = 0;
    }

    /**
     * Peek at the input stream and see what's coming next.
     */
    JSONToken peek()
    {
        if(cIdx < cache.length)
            return cache[cIdx].token;
        return chain.jsonTok(pos);
    }

    /**
     * Release the given number of stream elements from the stream.
     * Note: you are only allowed to release elements that are ALREADY parsed.
     *
     * Params: elements = the number of code units to release from the stream.
     */
    void release(size_t elements)
    {
        // not compatible while we are caching. You can still have a cache, but
        // the caching needs to be turned off.
        assert(!caching);

        // release items from the chain window.
        assert(position >= elements);
        chain.release(elements);
        pos -= elements;

        // update the cache if it exists
        if(cache.length > 0)
        {
            size_t toRemove = 0;
            foreach(ref ci; cache)
            {
                if(ci.offset < elements)
                    ++toRemove;
                else
                    ci.offset -= elements;
            }
            if(toRemove > 0)
            {
                // we shouldn't be removing any elements still in use.
                assert(toRemove <= cIdx);

                import std.algorithm.mutation : copy;
                auto validElems = cache.length - toRemove;
                copy(cache[toRemove .. $], cache[0 .. validElems]);
                cache = cache[0 .. validElems];
                cache.assumeSafeAppend;
                cIdx -= toRemove;
            }
        }
    }


    /**
     * Release all elements that have been parsed completely. The return value
     * is the number of elements that were released. Note that this can be done
     * at any point, it doesn't matter if the parser is partway through an
     * object, the rest of that object can still be parsed.
     *
     * The goal is to free up buffer space for more incoming data.  It is
     * better to call this function than release directly if you just want to
     * free up parsed data.
     *
     * Note that any string elements that refer to the buffer are invalidated,
     * since the buffer space will be gone.
     *
     * Returns: The number of elements that were released.
     */
    size_t releaseParsed()
    {
        auto result = position;
        if(result)
            release(result);
        return result;
    }
}

/**
 * Wrap a text iopipe into a JSONParser struct. 
 */
auto jsonTokenizer(bool replaceEscapes = true, Chain)(Chain c)
{
    return JSONTokenizer!(Chain, replaceEscapes)(c);
}

unittest
{
    with(JSONToken)
    {
        import std.typecons: Tuple, tuple;
        alias Check = Tuple!(JSONToken, string);
        void verifyJson(bool replaceEscapes, C)(C[] jsonData, Check[] verifyAgainst)
        {
            // use a simple pipe to simulate not having all the data available at once.
            auto pipeAdapter = SimplePipe!(C[])(jsonData);
            auto parser = jsonTokenizer!replaceEscapes(pipeAdapter);
            JSONItem[] items;
            while(true)
            {
                auto item = parser.next;
                items ~= item;
                if(item.token == EOF || item.token == Error)
                    break;
            }

            assert(items.length == verifyAgainst.length);
            if(items[$-1].token == EOF)
                assert(parser.pos == jsonData.length);
            foreach(idx, item; items)
            {
                assert(item.token == verifyAgainst[idx][0]);
                auto expected = verifyAgainst[idx][1];
                import std.algorithm.comparison: equal;
                import std.format: format;
                assert(equal(item.data(jsonData), expected), format("(C = %s, replace = %s, curdata = %s) %s != %s", C.stringof, replaceEscapes, jsonData[0 .. parser.pos], item.data(jsonData), expected));
            }
        }
        auto jsonData = q"{   {
            "abc" : 123.456,
                "def": [1,0.5, 8e10, "hi", "\r\n\f\b\u0025", {}, true, false, null] }}";
        auto checkitems = [ 
            Check(ObjectStart, "{"),
            Check(String, "abc"),
            Check(Colon, ":"),
            Check(Number, "123.456"),
            Check(Comma, ","),
            Check(String, "def"),
            Check(Colon, ":"),
            Check(ArrayStart, "["),
            Check(Number, "1"),
            Check(Comma, ","),
            Check(Number, "0.5"),
            Check(Comma, ","),
            Check(Number, "8e10"),
            Check(Comma, ","),
            Check(String, "hi"),
            Check(Comma, ","),
            Check(String, "\\r\\n\\f\\b\\u0025")];
        auto replaceItem = checkitems.length - 1;
        checkitems ~= [
            Check(Comma, ","),
            Check(ObjectStart, "{"),
            Check(ObjectEnd, "}"),
            Check(Comma, ","),
            Check(True, "true"),
            Check(Comma, ","),
            Check(False, "false"),
            Check(Comma, ","),
            Check(Null, "null"),
            Check(ArrayEnd, "]"),
            Check(ObjectEnd, "}"),
            Check(EOF, "")];
        auto checkWithReplaceEscapes = checkitems.dup;
        checkWithReplaceEscapes[replaceItem][1] = "\r\n\f\b%";

        import std.meta: AliasSeq;
        foreach(T; AliasSeq!(char, wchar, dchar))
        {
            import std.conv: to;
            verifyJson!false(jsonData.to!(T[]), checkitems);
            verifyJson!true(jsonData.to!(T[]), checkWithReplaceEscapes);
        }

        // now, test to make sure the parser fails properly
        verifyJson!false(q"{123.456}", [Check(Error, "123.456")]);
        verifyJson!false(q"{{123.456}}", [Check(ObjectStart, "{"), Check(Error, "123.456")]);
    }
}

unittest
{
    // test caching
    auto jsonData = q"{{"a": 1,"b": 123.456, "c": null}}";
    auto parser = jsonData.jsonTokenizer!false;
    bool check(JSONItem item, JSONToken token, string expected)
    {
        return(item.token == token && item.data(parser.chain) == expected);
    }
    with(JSONToken)
    {
        assert(check(parser.next, ObjectStart, "{"));
        assert(check(parser.next, String, "a"));
        assert(check(parser.next, Colon, ":"));

        // cache the start of a value
        parser.startCache;
        assert(check(parser.next, Number, "1"));
        assert(check(parser.next, Comma, ","));
        assert(check(parser.next, String, "b"));
        assert(check(parser.next, Colon, ":"));

        // replay the cache for the value of a
        parser.rewind();
        assert(check(parser.next, Number, "1"));
        assert(check(parser.next, Comma, ","));

        // now with the cache still in there, restart the cache
        parser.startCache;
        assert(check(parser.next, String, "b"));
        assert(check(parser.next, Colon, ":"));
        assert(check(parser.next, Number, "123.456"));
        assert(check(parser.next, Comma, ","));

        // replay b again
        parser.rewind();
        parser.endCache();
        assert(check(parser.next, String, "b"));
        assert(check(parser.next, Colon, ":"));
        // test out releasing cached data
        parser.releaseParsed();
        assert(check(parser.next, Number, "123.456"));
        assert(check(parser.next, Comma, ","));
        assert(check(parser.next, String, "c"));
        assert(check(parser.next, Colon, ":"));
        assert(check(parser.next, Null, "null"));
        // the cache should now be exhausted
        assert(parser.cache.length == 0);
        assert(parser.cIdx == 0);
        assert(check(parser.next, ObjectEnd, "}"));
    }
}

unittest
{
    // test skipping items
    auto jsonData = q"{{"a" : 1, "b" : {"c" : [1,2,3], "d" : { "hello" : "world" }}}}";

    auto parser = jsonData.jsonTokenizer!false;
    bool check(JSONItem item, JSONToken token, string expected)
    {
        if(item.token == token && item.data(parser.chain) == expected)
            return true;
        import std.stdio;
        writeln(item);
        return false;
    }
    parser.startCache;
    auto item = parser.skipItem(); // skip it all;
    assert(check(item, JSONToken.EOF, ""));
    assert(parser.position == jsonData.length);

    // start over
    parser.rewind;
    assert(check(parser.next, JSONToken.ObjectStart, "{"));
    assert(check(parser.skipItem, JSONToken.Comma, ","));
    assert(check(parser.next, JSONToken.String, "b"));
    assert(check(parser.skipItem, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.EOF, ""));
    assert(parser.position == jsonData.length);

    // another try
    parser.rewind;
    assert(check(parser.next, JSONToken.ObjectStart, "{"));
    assert(check(parser.skipItem, JSONToken.Comma, ","));
    assert(check(parser.next, JSONToken.String, "b"));
    assert(check(parser.next, JSONToken.Colon, ":"));
    assert(check(parser.next, JSONToken.ObjectStart, "{"));
    assert(check(parser.next, JSONToken.String, "c"));
    assert(check(parser.skipItem, JSONToken.Comma, ","));
    assert(check(parser.next, JSONToken.String, "d"));
    assert(check(parser.skipItem, JSONToken.ObjectEnd, "}"));
    assert(check(parser.skipItem, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.EOF, ""));
    assert(parser.position == jsonData.length);
}
