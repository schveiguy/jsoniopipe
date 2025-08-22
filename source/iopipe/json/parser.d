/**
 * Streaming pull-parser for json.
 * 
 * This module contains the low-level API of this package. You use 
 * $(LREF JSONTokenizer, next ) and $(LREF JSONTokenizer, peek ) to 
 * manually consume the Tokens one by one, checking $(LREF JSONItem, token)
 * against $(LREF JSONToken) and the value via $(LREF JSONItem, data). If
 * lookahead of one token isn't sufficient, $(LREF JSONTokenizer, startCache)
 * and $(LREF JSONTokenizer, rewind) and $(LREF JSONTokenizer, endCache) can
 * be used to create Checkpoint to jump back to in the buffered input stream.
 * 
 * For the high level templated api that automatically deserializes a text stream
 * into a user-provided type, see $(LREF iopipe, json, serialize).
 * 
 * Copyright: Copyright Steven Schveighoffer 2017
 * License:   Boost License 1.0. (See accompanying file LICENSE_1_0.txt or copy at
 *            http://www.boost.org/LICENSE_1_0.txt)
 * Authors: Steven Schveighoffer
*/
module iopipe.json.parser;
import iopipe.traits;
import iopipe.bufpipe;
import iopipe.buffer;
import std.range.primitives;
import std.traits;
import std.meta: AliasSeq;

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
    Number = '0',      /// - or 0-9, (Also + or Inifinty or NaN in json5)
    True = 't',        /// true
    False = 'f',       /// false
    Null = 'n',        /// null
    Symbol = 's',      /// some symbol (only valid for json5 object keys)
    Comment = '/',     /// Comment (json5 only)
    Space = ' ',       /// Spacing
    EOF = '\0',        /// end of stream
    Error = 0xff,      /// unexpected data in stream
}

struct ParseConfig
{
    /**
     * For mutable pipes, replace unicode/string escapes with the correct
     * character before returning the json item.
     */
    bool replaceEscapes;

    /// Allow parsing of JSON5, not just JSON. See https://json5.org
    bool JSON5;

    /// Parsing should include spaces in the parsed output.
    bool includeSpaces;

    /**
     * Parsing should include comments in the parsed output. Comments include
     * the comment delimeters.
     */
    bool includeComments;
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
    Exp,     /// float number has exponent.
    Hex,     /// integer in hex format (JSON5 only)
    Infinity,/// positive or negative infinity. (JSON5 only)
    NaN,     /// positive or negative NaN. (JSON5 only)
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

bool isSymbolStart(dchar firstChar)
{
    import std.uni: isAlpha;
    return firstChar == '\\' || firstChar == '_' || firstChar == '$' || isAlpha(firstChar);
}

/**
 * Search for the next token in the iopipe c, ignoring white space. This does
 * not validate data, it simply searches for the beginning of a valid JSON
 * token.
 * For JSON, Since each token is definitive based on the first character, only
 * the first character is looked at.
 *
 * For JSON5, tokens that might be symbols (e.g. nullThing) are validated
 * before returning. This means for JSON5 mode, this function isn't as trivial.
 *
 * Params:
 *    config = parser configuration. if JSON5 mode, then includeComments and
 *    includeSpaces are taken into account
 *    c = The iopipe in which to search for tokens. extend may be used on the
 *    iopipe to find the next token.
 *    pos = Current position in the window. Taken via ref so it can be updated
 *    to point at the new token.
 *    JSON5 = true if JSON5 is to be used.
 * Returns: The expected type of token at the new position.
 */
JSONToken jsonTok(ParseConfig config = ParseConfig.init, Chain)(ref Chain c, ref size_t pos) if (isIopipe!Chain && isSomeChar!(ElementEncodingType!(WindowType!Chain)))
{
    alias Char = immutable ElementEncodingType!(WindowType!Chain);

    if(pos == c.window.length && c.extend(0) == 0)
        return JSONToken.EOF;
    auto origPos = pos;
    /*static if(config.includeSpaces)
    {
        static if(config.JSON5)
        {{
            import std.utf: decode;
            import std.uni: isWhite;
            // check for utf whitespace
            auto tmpPos = pos;
            auto sp = decode(c.window, tmpPos);
            if(isWhite(sp))
                return JSONToken.Space;
        }}
        else
        {
            import std.ascii: isWhite;
            if(isWhite(c.window[pos]))
                return JSONToken.Space;
        }
    }

    static if(config.JSON5 && config.includeComments)
    {
        if(c.window[pos] == '/')
            return JSONToken.Comment;
    }*/

    // have to skip something
    static if(!config.includeSpaces || (config.JSON5 && !config.includeComments))
    {
        // strip any leading whitespace and/or comments.
        while(true)
        {
            // do things differently in JSON5, because we might be dealing with
            // both comments and spaces. Plus, we need to parse out unicode
            // spaces.
            static if(config.JSON5)
            {
                if(pos == c.window.length)
                {
                    if(c.extend(0) == 0)
                        return JSONToken.EOF;
                }
                static if(!config.includeComments)
                {
                    if(c.window[pos] == '/')
                    {
                        // comment. Parse out the comment
                        if(!parseComment(c, pos))
                        {
                            pos = origPos;
                            return JSONToken.Error;
                        }
                        continue;
                    }
                }
                static if(!config.includeSpaces)
                {
                    import std.utf: decode;
                    import std.uni: isWhite;
                    // need to use unicode to look for whitespaces. Note that it is
                    // assumed the incoming chain has only decodable code points
                    immutable oldpos = pos;
                    auto sp = decode(c.window, pos);
                    if(!isWhite(sp))
                    {
                        pos = oldpos;
                        break;
                    }
                }
                else
                    // including spaces, and the last character wasn't a comment
                    break;
            }
            else static if(!config.includeSpaces)
            {
                // normal JSON. Only need to deal with ASCII spaces.
                import std.ascii: isWhite;
                while(pos < c.window.length && isWhite(c.window[pos]))
                    ++pos;
                if(pos < c.window.length)
                    break;
                if(c.extend(0) == 0)
                    return JSONToken.EOF;
            }
            else
                static assert(0); // should never get here! But don't want an
                                  // infinite loop either...
        }
    }

    immutable cur = c.window[pos];
    static if(config.JSON5)
    {
        import std.utf: decode;
        import std.uni: isAlpha, isWhite;
        import std.ascii: asciiWhite = isWhite;
        // need to do more than look at one character for identifiers.
        JSONToken expectKeyword(Char[] expected)
        {
            immutable need = pos + expected.length + 1;
            c.ensureElems(need);
            if(c.window.length < need - 1)
                return JSONToken.Symbol;
            if(c.window[pos .. pos + expected.length] == expected)
            {
                auto tmppos = pos + expected.length;
                if(c.window.length >= need && isAlpha(decode(c.window, tmppos)))
                    return JSONToken.Symbol;
                // not a continuation of a symbol.
                return cast(JSONToken)expected[0];
            }
            return JSONToken.Symbol;
        }

        with(JSONToken) switch(cur)
        {
        case ObjectStart:
        case ObjectEnd:
        case String:
        case Colon:
        case Comma:
        case ArrayStart:
        case ArrayEnd:
        case Comment:
            return cast(JSONToken)cur;
        case True:
            return expectKeyword("true");
        case False:
            return expectKeyword("false");
        case Null:
            return expectKeyword("null");
        case 'N':
            // this might be "NaN", but could also be an identifier
            return expectKeyword("NaN") == 'N' ? Number : Symbol;
        case 'I':
            // this might be "Infinity", but could also be an identifier
            return expectKeyword("Infinity") == 'I' ? Number : Symbol;
        case '-':
        case '+':
        case '0': .. case '9':
        case '.':
            return Number;
        case '\'':
            return String;
        default:
            // check for symbol starts
            if(cur & 0x80)
            {
                // decode the code point, check to see if it's alphanumeric
                auto tmppos = pos;
                auto decoded = decode(c.window, tmppos);
                if(config.includeSpaces && isWhite(decoded))
                    return Space;
                if(isAlpha(decoded))
                    return Symbol;
            }
            else if(cur == '\\' || cur == '$' || cur == '_' ||
                    (cur >= 'a' && cur <= 'z') ||
                    (cur >= 'A' && cur <= 'Z'))
            {
                return Symbol;
            }
            else if(config.includeSpaces && asciiWhite(cur))
            {
                return Space;
            }
            pos = origPos;
            return Error;
        }
    }
    else
    {
        with(JSONToken) switch(cur)
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
            static if(config.includeSpaces)
            {
                // check for spaces
                import std.ascii : isWhite;
                if(isWhite(cur))
                    return Space;
            }
            pos = origPos;
            return Error;
        }
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
    deprecated("do not use offset, as this value will no longer maintained in a future release")
    @property ref size_t offset() => _offset;

    // private to prevent direct access without going through the deprecated
    // accessor.
    private size_t _offset;

    /**
     * Absolute position from the stream of this JSONItem. This is used with
     * the JSONPipe to determine the exact bytes, even if data is released
     * from the stream when fetching the actual data.
     */
    size_t position;

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
     *
     * WARNING:	A JSONItem can only safely call this function before the item is released with "releaseParsed".
     * 		Additionally, the returned string gets invalidated on calls to peek/next/releaseParsed.
     */
    deprecated("Do not use underlying iopipe directly with JSONItem, always use the JSONPipe")
    auto data(Chain)(ref Chain c) if (!is(Chain == JSONPipe!(Args), Args...))
    {
        return c.window[_offset .. _offset + length];
    }

    /// ditto
    auto data(Chain)(ref Chain c) if (is(Chain == JSONPipe!(Args), Args...))
    {
        // use the jsonbuffer position to calculate the right slice to grab
        immutable base = position - c.sourceOffset;
        return c.source.window[base .. base + length];
    }

    /**
     * Given an iopipe from which this item came, determines whether this item is a single-quoted string. This is only valid for JSON5 parsing, though there is no indication of whether JSON5 was used in the JSONItem.
     * Returns:
     *   true - if the token is a string, and the code unit before the current
     *          position is the single quote character.
     *   false - if the token is not a string, or the code unit before the
     *           current position is either not available or is a double quote.
     */
    deprecated("Do not use underlying iopipe directly with JSONItem, always use the JSONPipe")
    bool isSingleQuoteString(Chain)(ref Chain c) if (!is(Chain == JSONPipe!(Args), Args...))
    {
        return token == JSONToken.String && _offset > 0 && c.window[_offset - 1] == '\'';
    }

    /// ditto
    bool isSingleQuoteString(Chain)(ref Chain c) if (is(Chain == JSONPipe!(Args), Args...))
    {
        immutable base = position - 1 - c.sourceOffset;
        return token == JSONToken.String && base < c.source.window.length && c.source.window[base] == '\'';
    }
}

/**
 * if TP is not empty, then it contains the target position, and we are
 * replacing escapes
 *
 * pos should be set to point at the first hex character
 *
 * returns the dchar parsed.
 */
private dchar parseUnicodeEscape(Chain, TP...)(ref Chain chain, ref bool windowChanged, ref size_t pos, ref TP targetPos) if (TP.length == 0 || is(TP == AliasSeq!(size_t)))
{
    enum replaceEscapes = TP.length > 0;
    bool makeAvailable(size_t codepoints)
    {
        immutable len = pos + codepoints;
        if(len > chain.window.length)
        {
            if(chain.ensureElems(len) < len)
            {
                // invalid sequence.
                pos = chain.window.length;
                return false;
            }
            windowChanged = true;
        }
        return true;
    }

    // ensure there are at least 4 characters available.
    if(!makeAvailable(4)) return dchar.init;

    // parse the hex chars
    import std.conv: parse;
    auto chars = chain.window[pos .. pos + 4];

    dchar value = parse!ushort(chars, 16);
    if(chars.length)
    {
        // some error with parsing
        pos += 4 - chars.length;
        return dchar.init;
    }
    pos += 4;
    alias Char = typeof(chars[0]);

    if(value >= 0xd800 && value <= 0xdbff)
    {
        // this is the first half of a surrogate pair
        // next pair will be of form: \uXXXX
        if(!makeAvailable(6))
            return dchar.init;

        chars = chain.window[pos .. pos + 6];
        if(chars[0] != '\\')
            return dchar.init;
        ++pos;
        if(chars[1] != 'u')
            return dchar.init;
        ++pos;
        chars = chars[2 .. $];
        wchar pair = parse!ushort(chars, 16);
        if(chars.length)
        {
            pos += 4 - chars.length;
            return dchar.init;
        }
        pos += 4;

        if(pair < 0xdc00 || value > 0xdfff)
            // invalid sequence
            return dchar.init;

        static if(replaceEscapes && is(Char == wchar))
        {
            // just copy the two surrogates to the stream
            chain.window[targetPos[0]++] = cast(wchar)value;
            chain.window[targetPos[0]++] = pair;
            value = ((value & 0x3ff) << 10) + (pair & 0x3ff);
            return value;
        }
        else
        {
            // just build the dchar out of it.
            value = ((value & 0x3ff) << 10) + (pair & 0x3ff);
        }
    }

    static if(replaceEscapes)
    {
        // store the resulting dchar back to the stream
        static if(is(Char == dchar))
        {
            chain.window[targetPos[0]++] = value;
        }
        else static if(is(Char == wchar))
        {
            // not a surrogate pair
            chain.window[targetPos[0]++] = cast(wchar)value;
        }
        else  // utf 8
        {
            import std.utf : encode;
            char[4] data;
            foreach(i; 0 .. encode(data, value))
                chain.window[targetPos[0]++] = data[i];
        }
    }
    return value;
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
 *     replaceEscapes = If true, then any encountered escapes will be replaced
 *     with the actual utf characters they represent, properly encoded
 *     for the given element type.
 *     JSON5 = If true, processes JSON5 style strings
 *     config = parser configuration. `config.JSON5` and `config.replaceEscapes`
 *     are used.
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
int validateString(bool replaceEscapes = true, bool JSON5 = false, Chain)(immutable char quotechar, ref Chain c, ref size_t pos, ref JSONParseHint hint)
{
    hint = JSONParseHint.InPlace;

    immutable origPos = pos;
    static if(replaceEscapes)
        auto targetPos = pos;
    else
        alias targetPos = AliasSeq!();
    bool isEscaped = false;
    auto src = c.window;
    while(true)
    {
        if(pos == src.length)
        {
            // need more data from the pipe
            if(c.extend(0) == 0)
            {
                // EOF.
                if(quotechar == '\0')
                {
                    // the chain passed in was an already-parsed string. Treat
                    // this as the closing quote.
                    static if(replaceEscapes)
                        return cast(int)(targetPos - origPos);
                    else
                        return cast(int)(pos - origPos - 1);
                }
                return -1;
            }
            src = c.window;
        }
        auto elem = src[pos];
        if(isEscaped)
        {
            isEscaped = false;
            if(elem == 'u') // unicode sequence. 
            {
                ++pos; // skip the 'u'
                bool winchanged;
                if(parseUnicodeEscape(c, winchanged, pos, targetPos) == dchar.init)
                    return -1;

                if(winchanged)
                    // window was changed, re-slice the window
                    src = c.window;
                goto escapeSequenceDone;
            }
            static if(JSON5)
            {
                if(elem == 'x') // hex sequence.
                {
                    ++pos;
                    if(pos + 2 > src.length)
                    {
                        c.ensureElems(pos + 2);
                        // may need to re-assign src.
                        src = c.window;
                        if(pos + 2 > src.length)
                        {
                            // invalid sequence.
                            pos = src.length;
                            return -1;
                        }
                    }

                    // parse the hex chars
                    import std.conv: parse;
                    auto chars = src[pos .. pos + 2];
                    auto value = parse!ubyte(chars, 16);
                    pos += 2;
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
                        static if(is(Char == dchar) || is(Char == char))
                        {
                            src[targetPos++] = value;
                        }
                        else // char
                        {
                            // only need to encode if it's not ascii.
                            if(value >= 0x80)
                            {
                                src[targetPos++] = 0b1100_0000 | (value >> 6);
                                src[targetPos++] = 0b1000_0000 | (value & 0b0011_1111);
                            }
                            else
                                src[targetPos++] = value;
                        }
                    }
                    goto escapeSequenceDone;
                }
            }
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
                case '\'':
                    static if(JSON5)
                    {
                        // single quote
                        src[targetPos++] = '\'';
                        break;
                    }
                case 'v':
                    static if(JSON5)
                    {
                        // vertical tab;
                        src[targetPos++] = '\v';
                        break;
                    }
                case '0':
                    static if(JSON5)
                    {
                        // null character
                        src[targetPos++] = '\0';
                        break;
                    }
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
                case '\\': case '/': case'"': case 'n':
                case 'b': case 'f': case 'r': case 't':
                    break;
                case '\'': case '0': case 'v':
                    // additional JSON5 escapes
                    static if(JSON5)
                        break;
                default:
                    return -1;
                }
            }
            ++pos;
escapeSequenceDone:
        }
        else if(elem == '\\')
        {
            static if(!replaceEscapes)
                hint = JSONParseHint.Escapes;
            isEscaped = true;
            ++pos;
        }
        /*else if(surrogate != wchar.init)
        {
            // we were expecting another surrogate pair, error.
            return -1; 
        }*/
        else if(elem == quotechar)
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
    assert(0);
}

int parseString(bool replaceEscapes = true, bool JSON5 = false, Chain)(ref Chain c, ref size_t pos, ref JSONParseHint hint)
{
    auto src = c.window;
    static if(JSON5)
    {
        if(src.length == 0 || (src[pos] != '"' && src[pos] != '\''))
            return -1;
        immutable quotechar = cast(char)src[pos];
    }
    else
    {
        if(src.length == 0 || src[pos] != '"')
            return -1;
        enum quotechar = '"';
    }
    ++pos;
    return validateString!(replaceEscapes, JSON5)(quotechar, c, pos, hint);
}

/// ditto
int parseString(ParseConfig config, Chain)(ref Chain c, ref size_t pos, ref JSONParseHint hint)
{
    return parseString!(config.replaceEscapes, config.JSON5)(c, pos, hint);
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
    // testParse!false(q"{"abcdef}", true); // use different string here to molify github syntax highlighting
    testParse!false(`"abcdef`, true);
    testParse!true(q"{"abcdef"}".dup, false);
    testParse!true(q"{"abcdef\n"}".dup, false, JSONParseHint.InPlace, 7, "abcdef\n");
    testParse!true(q"{"abcdef\ua123\n"}".dup, false, JSONParseHint.InPlace, 10, "abcdef\ua123\n");
    testParse!false(q"{"abcdef\ua123\n"}", false, JSONParseHint.Escapes);
}

/// utility function to extract a string from a json element while processing escapes. May or may not make a copy.
T extractString(T, bool forceCopy = false, Element)(Element item) if (isSomeString!T && is(Element : JSONPipe!(Args).Element, Args...))
{
    // for now, the common function uses JSONItem and Chain.
    return extractStringImpl!(T, forceCopy)(*item.owner, item.item);
}

deprecated("Extract strings from Elements, not JSONItems")
T extractString(T, bool forceCopy = false, Chain)(Chain chain, JSONItem item) if (isSomeString!T && is(Chain == JSONPipe!Args, Args...))
{
    return extractStringImpl!(T, forceCopy)(chain, item);
}

private T extractStringImpl(T, bool forceCopy = false, Chain)(Chain chain, JSONItem item) if (isSomeString!T && is(Chain == JSONPipe!Args, Args...))
{
    import std.conv;
    assert(item.token == JSONToken.String);
    if(item.hint == JSONParseHint.InPlace)
    {
        auto buf = item.data(chain);
        static if(forceCopy && is(immutable(T) == immutable(typeof(buf))))
            return cast(T)buf.dup;
        else
            return buf.to!T;
    }

    // re-process, this time replacing escapes.
    alias Char = Unqual!(typeof(T.init[0]));
    auto origData = item.data(chain);
    static if(is(Char == typeof(origData[0])))
    {
        // need to make sure we copy.
        auto newpipe = new Char[origData.length];
        newpipe[] = origData;
    }
    else
    {
        auto newpipe = origData.to!(Char[]);
    }
    size_t pos = 0;
    JSONParseHint hint;
    auto len = validateString!(true, chain.config.replaceEscapes)('\0', newpipe, pos, hint);
    assert(len >= 0);
    return cast(T)newpipe[0 .. len];
}

/**
 * Parse/validate a number from the given iopipe. This is used to validate the
 * number follows the correct grammar from the JSON spec, and also to find out
 * how many elements in the stream are used for this number.
 *
 * Params:
 *     JSON5 = if true, parse JSON5 numbers
 *     config = configuration overload, only `config.JSON5` is used.
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
int parseNumber(bool JSON5, Chain)(ref Chain c, ref size_t pos, ref JSONParseHint hint)
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
        decimaloptdigit,
        anydigit2,
        exponent,
        expsign,
        anydigit3,
        firsthexdigit,
        hexdigits
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
                if(
                        s == leadingzero || s == anydigit1 ||
                        s == anydigit2 || s == anydigit3 ||
                        (JSON5 && s == decimaloptdigit) ||
                        (JSON5 && s == hexdigits))
                {
                    return cast(int)(pos - origPos); // finished.
                }
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
                if(elem == '-' || (JSON5 && elem == '+'))
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
                else if(JSON5 && elem == '.')
                {
                    // json5 allows fractional numbers without leading 0,
                    // but the fractional part must not be missing.
                    hint = JSONParseHint.Float;
                    s = decimal;
                }
                else if(JSON5 && elem == 'I')
                {
                    // validate the rest of the number is 'Infinity'
                    auto reqElems = pos + "Infinity".length + 1;
                    if(c.ensureElems(reqElems) < reqElems - 1)
                        // not enough elements in the stream
                        return -1;
                    if(c.window[pos .. pos + "Infinity".length] != "Infinity")
                        // Doesn't match Infinity
                        return -1;
                    pos += "Infinity".length;
                    hint = JSONParseHint.Infinity;
                    // no possible things valid after this. token over.
                    return cast(int)(pos - origPos);
                }
                else if(JSON5 && elem == 'N')
                {
                    // validate the rest of the number is 'NaN'
                    auto reqElems = pos + "NaN".length + 1;
                    if(c.ensureElems(reqElems) < reqElems - 1)
                        // not enough elements in the stream
                        return -1;
                    if(c.window[pos .. pos + "NaN".length] != "NaN")
                        // Doesn't match NaN
                        return -1;
                    pos += "NaN".length;
                    hint = JSONParseHint.NaN;
                    // no possible things valid after this. token over.
                    return cast(int)(pos - origPos);
                }
                else
                    // error
                    return -1;
                break;
            case leadingzero:
                if(elem == '.')
                {
                    hint = JSONParseHint.Float;
                    s = JSON5 ? decimaloptdigit : decimal;
                }
                else if(JSON5 && (elem == 'x' || elem == 'X'))
                {
                    if(s == leadingzero)
                    {
                        // only valid with 0x, not from jumping from other state.
                        hint = JSONParseHint.Hex;
                        s = firsthexdigit;
                    }
                    else
                        return cast(int)(pos - origPos); // finished
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
            case decimaloptdigit:
                static if(JSON5)
                {
                    if(elem >= '0' && elem <= '9')
                        s = anydigit2;
                    else if(elem == 'e' || elem == 'E')
                    {
                        hint = JSONParseHint.Exp;
                        s = exponent;
                    }
                    else
                        return cast(int)(pos - origPos); // finished
                    break;
                }
                else
                    // JSON requires a digit after decimal, this state should
                    // never be reached
                    assert(0);
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
            case firsthexdigit:
                static if(JSON5)
                {
                    if((elem >= '0' && elem <= '9') ||
                            (elem >= 'a' && elem <= 'f') ||
                            (elem >= 'A' && elem <= 'F'))
                    {
                        s = hexdigits;
                        break;
                    }
                    else
                        return -1; // error, need one hex digit
                }
                else
                    assert(0); // JSON doesn't support hex
            case hexdigits:
                static if(JSON5)
                {
                    if((elem >= '0' && elem <= '9') ||
                            (elem >= 'a' && elem <= 'f') ||
                            (elem >= 'A' && elem <= 'F'))
                    {
                        break;
                    }
                    else
                        return cast(int)(pos - origPos); // finished
                }
                else
                    assert(0); // JSON doesn't support hex
        }
        ++pos;
    }
    // all returns should happen in the infinite loop.
    assert(0);
}

/// ditto
int parseNumber(ParseConfig config, Chain)(ref Chain c, ref size_t pos, ref JSONParseHint hint)
{
    return parseNumber!(config.JSON5)(c, pos, hint);
}

// JSON number parsing
unittest
{
    void testParse(string jsonString, bool shouldFail, JSONParseHint expectedHint = JSONParseHint.Int)
    {
        size_t pos;
        JSONParseHint hint;
        auto result = parseNumber!false(jsonString, pos, hint);
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
    testParse(".e5", true);
    testParse("+Infinity", true);
    testParse("-Infinity", true);
    testParse("Infinity", true);
    // these would parse, but without the 
    //testParse("0x0123456789abcdefABCDEF", false);
    //testParse("0X0123456789abcdefABCDEF", false);
}

// JSON5 number parsing
unittest
{
    void testParse(string jsonString, bool shouldFail, JSONParseHint expectedHint = JSONParseHint.Int)
    {
        size_t pos;
        JSONParseHint hint;
        auto result = parseNumber!true(jsonString, pos, hint);
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
    testParse("+.1E-1", false, JSONParseHint.Exp);
    testParse("123.", false, JSONParseHint.Float);
    testParse("--123", true);
    testParse(".1", false, JSONParseHint.Float);
    testParse("0.1e", true);
    testParse(".e5", true);
    testParse("+Infinity", false, JSONParseHint.Infinity);
    testParse("-Infinity", false, JSONParseHint.Infinity);
    testParse("Infinity", false, JSONParseHint.Infinity);
    testParse("0x0123456789abcdefABCDEF", false, JSONParseHint.Hex);
    testParse("0X0123456789abcdefABCDEF", false, JSONParseHint.Hex);
}

// returns true if comment successfully parsed, false otherwise.
private bool parseComment(Chain)(ref Chain c, ref size_t pos)
{
    // assume the first character is a slash.
    assert(c.window[pos] == '/');
    auto nextChar()
    {
        if(++pos == c.window.length)
            if(c.extend(0) == 0)
                return '\0';
        return c.window[pos];
    }


    auto nc = nextChar;
    if(nc == '/') // single line comment
    {
        // read until the end of the line
        while(true)
        {
            switch(nextChar)
            {
                case '\n':
                    // skip and return
                    ++pos;
                    return true;
                case 0:
                    return false; // eof
                default:
                    break;
            }
        }
    }
    else if(nc == '*')
    {
        // read until we see */
        bool seenstar;
        while(true)
        {
            switch(nextChar)
            {
                case '*':
                    seenstar = true;
                    break;
                case '/':
                    if(seenstar)
                    {
                        ++pos;
                        return true;
                    }
                    break;
                case 0:
                    return false; // eof
                default:
                    seenstar = false;
                    break;
            }
        }
    }
    else
        // not really a comment, invalid token sequence
        return false;
}

/**
 * Obtain one parsing item from the given iopipe. This has no notion of
 * context, so it does not actually validate the overall structure of the JSON
 * stream. It only confirms that the next item is a valid JSON item.
 *
 * Params:
 *     config = Parser config.
 *     c = iopipe from which to parse item. If needed, it may be extended.
 *     pos = Current position in the iopipe's window from which the next item
 *     should start. Leading whitespace is allowed.
 *     chainStartPosition = The position the first element in the chain window
 *     is in the stream. This is considered to be an absolute position. This
 *     value is used to fill in the position field of the JSONItem.
 *
 * Returns: If the stream contains a valid JSON item, the details about that
 * item are returned. If the stream does not contain any more items, then EOF
 * is returned. If there is an error parsing data from the stream for any
 * reason, then Error is returned.
 *
 */
deprecated("Please supply a chainStartPosition parameter to properly set up the json item position")
JSONItem jsonItem(ParseConfig config = ParseConfig.init, Chain)(ref Chain c, ref size_t pos)
{
    return jsonItem!config(c, 0);
}
/// ditto
JSONItem jsonItem(ParseConfig config = ParseConfig.init, Chain)(ref Chain c, ref size_t pos, immutable size_t chainStartPosition)
{
    // parse a json item out of the chain
    JSONItem result;

    void setOffset(size_t newOffset) {
        result._offset = newOffset;
        result.position = newOffset + chainStartPosition;
    }

    result.token = jsonTok!config(c, pos);
    setOffset(pos);

    static if(config.includeSpaces)
    {
        if(result.token == JSONToken.Space)
        {
            // process all spaces
            static if(config.JSON5)
            {
                import std.utf: decode;
                import std.uni: isWhite;
                auto w = c.window;
                while(true)
                {
                    // make sure there's enough data for another character.
                    if(pos == w.length)
                    {
                        // try and get more data
                        if(c.extend(0) == 0)
                        {
                            // done with the space
                            break;
                        }
                        w = c.window;
                    }
                    auto oldPos = pos;
                    if(!isWhite(decode(w, pos)))
                    {
                        // jump back to the start of this character.
                        pos = oldPos;
                        break;
                    }
                }
                result.length = pos - result._offset;
                return result;
            }
            else
            {
                import std.ascii: isWhite;
                auto w = c.window;
                while(true)
                {
                    if(pos == w.length)
                    {
                        if(c.extend(0) == 0)
                            break;
                        w = c.window;
                    }

                    if(!isWhite(c.window[pos]))
                        break;
                    ++pos;
                }
                result.length = pos - result._offset;
                return result;
            }
        }
    }

    static if(config.JSON5)
    {
        // validates a token is as expected, including checking if it could be
        // an identifier instead. if it is not the expected token, or if it
        // starts with the expected token, but continues as an identifier,
        // switch to Identifier token type, and parse the identifier.
        void validateToken(string expected)
        {
            import std.uni : isAlphaNum;
            import std.utf : decode;

            // note, jsonTok will already have validated the token itself.
            // Start off assuming that much is correct.
            auto startpos = pos;
            pos += expected.length;
            static if(config.replaceEscapes)
                auto targetPos = pos;
            else
                alias targetPos = AliasSeq!();
            auto w = c.window;
            bool escaped = false;
            bool hasEscape = false;
            while(true)
            {
                // make sure there's enough data for another character.
                if(pos == c.window.length)
                {
                    // try and get more data
                    if(c.ensureElems(pos + 1) < pos + 1)
                    {
                        // done with the identifier
                        break;
                    }
                }
                auto oldpos = pos;
                auto n = decode(w, pos);
                if(escaped)
                {
                    if(n != 'u')
                    {
                        // error, no other escapes valid when doing a symbol
                        result.token = JSONToken.Error;
                        return;
                    }
                    // decode the unicode escape
                    bool winchanged = false;
                    n = parseUnicodeEscape(c, winchanged, pos, targetPos);
                    if(n == dchar.init)
                    {
                        result.token = JSONToken.Error;
                        return;
                    }
                    if(winchanged)
                        w = c.window;
                    if(n == '\\')
                    {
                        // avoid this debacle.
                        result.token = JSONToken.Error;
                        return;
                    }
                }

                if(n == '\\')
                {
                    escaped = true;
                    continue;
                }
                else if(!isAlphaNum(n))
                {
                    // not part of the symbol
                    if(escaped)
                    {
                        // this is an error, we already replaced the escape though.
                        result.token = JSONToken.Error;
                        break;
                    }
                    pos = oldpos;
                    break;
                }
                else
                {
                    // definitely a symbol.
                    result.token = JSONToken.Symbol;
                    static if(config.replaceEscapes)
                    {
                        if(oldpos == targetPos)
                        {
                            // just move targetPos
                            targetPos = pos;
                        }
                        else
                        {
                            // need to copy the data
                            static if(is(Char == dchar))
                            {
                                w[targetPos++] = w[oldpos];
                            }
                            else
                            {
                                while(oldpos != pos)
                                    w[targetPos++] = w[oldpos++];
                            }
                        }
                    }
                    else
                    {
                        if(escaped)
                            result.hint = JSONParseHint.Escapes;
                    }
                    escaped = false;
                }
            }

            // the length is based on the target position
            static if(config.replaceEscapes)
                result.length = targetPos - startpos;
            else
                result.length = pos - startpos;
        }
    }
    else
    {
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
                setOffset(c.window.length);
                result.token = JSONToken.Error;
                return;
            }

            // can't use std.algorithm.equal here, because of autodecoding...
            foreach(i, c; expected)
            {
                if(w[i] != c)
                {
                    // doesn't match
                    setOffset(pos + i);
                    result.token = JSONToken.Error;
                    return;
                }
            }

            result.length = expected.length;
            pos += expected.length;
        }
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
    case Symbol:
        static if(config.JSON5)
        {
            validateToken("");
            break;
        }
        else
        {
            assert(0, "Should not get here");
        }
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
            auto numChars = parseString!config(c, pos, result.hint);
            if(numChars < 0)
            {
                result.token = Error;
                result.length = pos - result._offset;
            }
            else
            {
                // skip over initial quote
                setOffset(result._offset + 1);
                result.length = numChars;
            }
        }
        break;
    case Number:
        // ensure the number is correct.
        {
            auto numChars = parseNumber!config(c, pos, result.hint);
            if(numChars < 0)
            {
                result.token = Error;
                result.length = pos - result._offset;
            }
            else
            {
                result.length = numChars;
            }
        }
        break;
    case Comment:
        static if(config.JSON5 && config.includeComments)
        {
            if(parseComment(c, pos))
                result.length = pos - result._offset;
            else
                result.token = JSONToken.Error;
            break;
        }
    case Space:
        assert(0);
    }
    return result;
}

/**
 * A JSONPipe is an iopipe which maintains a buffer of Elements. A
 * Element is a combination of a JSONItem with a reference to the
 * underlying stream, to be able to conveniently extract the data.
 *
 * This pipe validates the json stream as it parses, and stop extending once a
 * complete JSON message is parsed. To reset the parsing, use the `reset`
 * method (in the case where the stream has more json messages in it)
 *
 * Note that a JSONPipe should not be copied, because the Elements have a
 * reference to the JSONPipe. This design decision may be enforced mechanically
 * in the future
 */
struct JSONPipe(SourceChain, Allocator = GCNoPointerAllocator, ParseConfig cfg = ParseConfig.init) {
    private import std.bitmanip : BitArray;
    alias Char = typeof(WindowType!SourceChain.init[0]);
    alias config = cfg;
    private {
        SourceChain source;
        AllocatedBuffer!(JSONItem, Allocator, 16) buffer;
        size_t sourceOffset; // position of source.window[0]
        size_t pos; // position in the source window tracking how much data we have parsed.

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
    }

    this(SourceChain source) {
        this.source = source;
        sourceOffset = 0;
    }

    ref SourceChain valve() => source;

    static struct Element {
        private JSONItem item;
        public JSONPipe* owner;
        auto token() => item.token;
        auto hint() => item.hint;
        Char[] data() {
            immutable base = item.position - owner.sourceOffset;
            return owner.source.window[base .. base + item.length];
        }
        deprecated("You do not need to pass in the chain to get the data any more")
        Char[] data(Chain)(ref Chain) => data();

        bool isSingleQuoteString() {
            immutable qidx = item.position - 1 - owner.sourceOffset;
            return qidx < owner.source.window.length && owner.source.window[qidx] == '\'';
        }


        deprecated("You do not need to pass in the chain to get the data any more")
        bool isSingleQuoteString(Chain)(ref Chain) => isSingleQuoteString();
    }

    static struct Window {
        private {
            JSONPipe *owner;
            JSONItem[] items;
        }

        auto front() => this[0];
        auto back() => this[$-1];
        bool empty() => items.empty;
        void popFront() => items.popFront;
        void popBack() => items.popBack;
        size_t length() => items.length;
        alias opDollar = length;

        auto opIndex(size_t idx) {
            auto item = items[idx];
            return Element(items[idx], owner);
        }
    }

    Window window() => Window(&this, buffer.window);

    size_t extend(size_t elements) {
        // if we already hit EOF, stop.
        if(state == State.End && buffer.window.length > 0 && buffer.window[$-1].token == JSONToken.EOF)
            return 0;

        if(buffer.extend(1) == 0)
        {
            // Cannot extend the buffer for some reason, go to end state.
            state = State.End;
            return 0;
        }


        auto item = &(buffer.window[$-1] = jsonItem!config(source, pos, sourceOffset));

        if(item.token == JSONToken.EOF) {
            state = State.End;
            return 1;
        }

        static if(config.includeSpaces)
        {
            if(item.token == JSONToken.Space)
                // spaces are always acceptable
                return 1;
        }
        static if(config.includeComments)
        {
            if(item.token == JSONToken.Comment)
                // comments are always acceptable
                return 1;
        }

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
                state = State.End; // no more things should happen
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
            static if(config.JSON5)
            {
                // allow trailing comma -- we can end the array/object here.
                if(item.token == (inObj ? ObjectEnd : ArrayEnd))
                {
                    popContainer();
                    break;
                }
            }
            if(inObj)
            {
                static if(config.JSON5)
                {
                    if(item.token == String || item.token == Symbol)
                        state = State.Colon;
                    else
                        item.token = JSONToken.Error;
                }
                else
                {
                    if(item.token == String)
                        state = State.Colon;
                    else
                        item.token = JSONToken.Error;
                }
                break;
            }
            goto case State.Value;
        case State.Colon:
            // requires colon
            if(item.token == Colon)
                state = State.Value;
            else
                item.token = JSONToken.Error;
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
                item.token = JSONToken.Error;
            break;
        case State.Comma:
            // can end the object here, or get a comma
            if(item.token == (inObj ? ObjectEnd : ArrayEnd))
                popContainer();
            else if(item.token == Comma)
                state = State.Member;
            else
                item.token = JSONToken.Error;
            break;
        case State.End:
            // this is handled outside the switch statement
            assert(0);
        }

        return 1;
    }

    void release(size_t elements) {
        if(elements == 0) return;
        // release up to the end of the last purged element. We do this because
        // we may need the data before the first element in the buffer (e.g. to
        // look at quote style)
        auto lastPurged = buffer.window[elements - 1];
        auto sourceElementsToPurge = lastPurged.position + lastPurged.length - sourceOffset;
        source.release(sourceElementsToPurge);
        sourceOffset += sourceElementsToPurge;
        pos -= sourceElementsToPurge;
        buffer.releaseFront(elements);

        // recalibrate all the offsets in the buffer (TODO: we will deprecate this)
        foreach(ref item; buffer.window)
            item._offset = item.position - sourceOffset;
    }

    // generate an "EOF" element as if it were at the end of the buffer without
    // having to store it in the buffer.
    private Element eofElement() {
        auto item = JSONItem(source.window.length, source.window.length + sourceOffset, 0, JSONToken.EOF);
        return Element(item, &this);
    }

    JSONToken peekToken() {
        // peek at the next token from the chain.
        return jsonTok!config(source, pos);
    }

    void reset() {
        release(buffer.window.length);
        state = State.Begin;
        stackLen = 0;
    }

    @property bool finished() => state == State.End;
}

/**
 * A JSONTokenizer is a combination of a JSONPipe and a position. This
 * combination is like a buffered file, where you have a position in the file
 * where you are parsing. This provides useful utilities for navigating the
 * buffer while deserializing.
 *
 * Each new item/token can be obtained by calling the `next` method.
 *
 * Construct with $(LREF jsonTokenizer) for template inference.
 */
struct JSONTokenizer(Chain, ParseConfig cfg)
{
    alias config = cfg;

    /**
     * The chain of parsed items. All items fetched from the tokenizer must be
     * in this chain.
     */
    JSONPipe!(Chain, GCNoPointerAllocator, cfg) chain;

    alias Element = typeof(chain).Element;

    private {
        size_t cIdx; // which index are we on in the chain.
        size_t rewindIdx; // which index to rewind to.
    }

    this(Chain chain) {
        this.chain = typeof(this.chain)(chain);
    }

    /// Last token has been consumed
    @property bool finished()
    {
        return chain.finished();
    }

    /** 
     * Start caching elements. When this is enabled, rewind will jump back to
     * the first element and replay from the cache instead of parsing. Make
     * sure to call endCache when you are done with the replay cache.
     *
     * Deprecated: index allows multiple save points, which can be later used
     * to set the index.
     */
    deprecated("Use index instead")
    void startCache()
    {
        rewindIdx = cIdx;
    }

    /**
     * stop caching elements (the cache will be freed when no longer needed)
     *
     * Deprecated: index allows multiple save points, which can be later used
     * to set the index.
     */
    deprecated("Use index instead")
    void endCache()
    {
        rewindIdx = size_t.max;
    }

    /**
     * this specialized function will skip the current item, taking into
     * account the nested nature of JSON. The return value is the next JSONItem
     * after the skipped data.
     *
     * If at the beginning of the JSON stream, the entire JSON stream is parsed
     * until the end of the JSON data in the stream.
     *
     * If at a member name, colon, or value expected, the entire member is skipped.
     *
     * If at a comma, the comma is skipped.
     *
     * If an error is encountered, it is returned immediately
     */
    JSONToken skipItem()
    {
        size_t depth = 0;
        // parse until we see the stack length get less than our current depth,
        // until we see a comma, error, or end of stream.
        while(true)
        {
            auto item = peek();
            with(JSONToken) switch(item)
            {
            case ObjectStart:
            case ArrayStart:
                ++depth;
                break;
            case ObjectEnd:
            case ArrayEnd:
                if(!depth)
                {
                    // this is the end of the *parent* object or array. Don't skip it.
                    return item;
                }
                else if(--depth == 0)
                {
                    // at the end of the current object. Skip the end piece, and move on.
                    auto n = nextSignificant;
                    if(n.token == Error)
                        return n.token;
                    return peekSignificant;
                }
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
            cast(void)next; // skip this item
        }
    }

    /// Consume any spaces and/or comments. Returns the next token after those are skipped.
    JSONToken peekSignificant()
    {
        auto nt = peek;
        while((config.includeSpaces && nt == JSONToken.Space) || (config.includeComments && nt == JSONToken.Comment))
        {
            cast(void)next();
            nt = peek;
        }
        assert(nt != JSONToken.Space && nt != JSONToken.Comment);
        return nt;
    }

    /// skip all the spaces and/or comments, and then return the next token
    auto nextSignificant()
    {
        static if(config.includeSpaces || (config.JSON5 && config.includeComments))
            cast(void)peekSignificant();
        auto result = next;
        assert(result.token != JSONToken.Space && result.token != JSONToken.Comment);
        return result;
    }


    /**
     * Parse until it finds a specific member/submember. The assumption is that
     * the current item is an object start.
    
     * Returns true if the specified submember was found, and the parser is
     * queued to parse the value of that member.
    
     * Returns false if the object was searched, but the submember could not be
     * found. In this case, the stream is left in a location that is
     * potentially partly advanced. Use caching to rewind if you don't wish to
     * lose the current position.
    
     * Also returns false if this is not an object.
     */
    bool parseTo(string[] submember...)
    {
        import std.algorithm : equal;
        while(submember.length > 0)
        {
            // jump into the first member.
            if(peek != JSONToken.ObjectStart)
                return false;
            cast(void)next; // skip the object start
            auto nt = peekSignificant();
            if(nt != JSONToken.String && (!config.JSON5 || nt != JSONToken.Symbol))
                return false;
            auto item = next;
            while(!item.data().equal(submember[0]))
            {
                cast(void)skipItem();
                auto nextItem = peekSignificant();
                if(nextItem == JSONToken.ObjectEnd)
                    return false;
                else if(nextItem == JSONToken.Comma)
                {
                    cast(void)next; // skip the comma
                    item = next; // load the next thing
                    if(config.JSON5 && item.token == JSONToken.ObjectEnd)
                        return false;
                }
                else
                    // something went wrong.
                    return false;
            }
            // found the item
            if(peekSignificant() != JSONToken.Colon)
                return false;
            item = next;
            submember = submember[1 .. $];
        }

        return true;
    }

    /// where are we in the buffer
    deprecated("Use chain instead of using internal position value")
    @property size_t position()
    {
        auto win = chain.window;
        if(cIdx < win.length)
            return win[cIdx].item.position - chain.sourceOffset;
        return chain.valve.window.length; // Ugly, uses the underlying stream chain.
    }

    /**
     * Obtain the next Element from the stream.
     */
    auto next()
    {
        if(cIdx == chain.window.length)
        {
            if(chain.extend(1) == 0)
            {
                if(chain.window.length == 0)
                    return chain.eofElement;
                return chain.window[$-1];
            }
        }

        return chain.window[cIdx++];
    }

    /**
     * Reset the tokenizer to the state the last time startCache was called.
     */
    deprecated("Use index or jumpBack instead")
    void rewind()
    {
        assert(rewindIdx != size_t.max);
        cIdx = rewindIdx;
    }

    /**
     * Jump back N items in the stream. Note that this includes any items,
     * including ones that may have been skipped. If a specific element should
     * be jumped to, store the index and set it later instead.
     */
    void jumpBack(size_t elements)
    {
        index(cIdx - elements);
    }

    @property index() => cIdx;

    @property size_t index(size_t idx)
    {
        assert(idx <= chain.window.length);
        return cIdx = idx;
    }

    /**
     * Peek at the input stream and see what's coming next. Note that if this
     * is at the end of the buffer, a new item is not necessarily loaded into
     * the buffer.
     */
    JSONToken peek()
    {
        auto win = chain.window;
        if(cIdx < win.length)
            return win[cIdx].token;
        return chain.peekToken();
    }

    /**
     * Release the given number of stream elements from the stream.
     * Note: you are only allowed to release elements that are ALREADY parsed.
     *
     * Params: elements = the number of code units to release from the stream.
     */
    deprecated("This function has been derprecated, use flushCache instead")
    void release(size_t elements)
    {
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
    deprecated("use flushCache instead")
    size_t releaseParsed()
    {
        auto origOffset = chain.sourceOffset;
        flushCache();
        return chain.sourceOffset - origOffset;
    }

    /**
     * flush all cached elements, including the last one parsed.
     */
    void flushCache()
    {
        chain.release(cIdx);
        cIdx = 0;
        if(rewindIdx != size_t.max)
            rewindIdx = 0;
    }
}

/**
 * Wrap a text iopipe into a JSONParser struct. 
 */
auto jsonTokenizer(ParseConfig config = ParseConfig.init, Chain)(Chain c)
{
    static if(config.JSON5)
    {
        // ensure the chain has only valid utf code points. JSON5 supports a lot more
        // unicode in the grammar, especially when finding tokens.
        import iopipe.textpipe : ensureDecodeable;
        auto realChain = c.ensureDecodeable;
    }
    else
    {
        alias realChain = c;
    }
    return JSONTokenizer!(typeof(realChain), config)(realChain);
}

unittest
{
    import std.stdio;
    with(JSONToken)
    {
        import std.typecons: Tuple, tuple;
        alias Check = Tuple!(JSONToken, string);
        void verifyJson(bool replaceEscapes, bool JSON5, bool includeSpaces, C)(C[] jsonData, Check[] verifyAgainst)
        {
            // use a simple pipe to simulate not having all the data available at once.
            //auto pipeAdapter = SimplePipe!(C[])(jsonData);
            auto pipeAdapter = jsonData;
            auto parser = jsonTokenizer!(ParseConfig(replaceEscapes, JSON5, includeSpaces))(pipeAdapter);
            parser.Element[] items;
            while(true)
            {
                auto item = parser.next;
                items ~= item;
                if(item.token == EOF || item.token == Error)
                    break;
            }

            import std.algorithm;
            if(items.length != verifyAgainst.length)
                writeln(items.map!(it => it.token));
            scope(failure)
            {
                writeln(items.length);
                writeln(verifyAgainst.length);
                writeln(items);
                writeln(verifyAgainst);
            }
            assert(items.length == verifyAgainst.length);
            if(items[$-1].token == EOF)
                assert(parser.chain.pos == jsonData.length);
            foreach(idx, item; items)
            {
                assert(item.token == verifyAgainst[idx][0]);
                auto expected = verifyAgainst[idx][1];
                import std.algorithm.comparison: equal;
                import std.format: format;
                assert(equal(item.data, expected), format("(C = %s, replace = %s, curdata = %s) %s != %s", C.stringof, replaceEscapes, jsonData[0 .. parser.chain.pos], item.data, expected));
            }
        }
        auto jsonData = q"{   {
            "abc" : 123.456,
                "def": [1,0.5, 8e10, "hi", "\r\n\f\b\u0025", {}, true, false, null] }}";
        auto checkitems = [ 
            Check(Space, "   "),
            Check(ObjectStart, "{"),
            Check(Space, "\n            "),
            Check(String, "abc"),
            Check(Space, " "),
            Check(Colon, ":"),
            Check(Space, " "),
            Check(Number, "123.456"),
            Check(Comma, ","),
            Check(Space, "\n                "),
            Check(String, "def"),
            Check(Colon, ":"),
            Check(Space, " "),
            Check(ArrayStart, "["),
            Check(Number, "1"),
            Check(Comma, ","),
            Check(Number, "0.5"),
            Check(Comma, ","),
            Check(Space, " "),
            Check(Number, "8e10"),
            Check(Comma, ","),
            Check(Space, " "),
            Check(String, "hi"),
            Check(Comma, ","),
            Check(Space, " "),
            Check(String, "\\r\\n\\f\\b\\u0025")];
        auto replaceItem = checkitems.length - 1;
        checkitems ~= [
            Check(Comma, ","),
            Check(Space, " "),
            Check(ObjectStart, "{"),
            Check(ObjectEnd, "}"),
            Check(Comma, ","),
            Check(Space, " "),
            Check(True, "true"),
            Check(Comma, ","),
            Check(Space, " "),
            Check(False, "false"),
            Check(Comma, ","),
            Check(Space, " "),
            Check(Null, "null"),
            Check(ArrayEnd, "]"),
            Check(Space, " "),
            Check(ObjectEnd, "}"),
            Check(EOF, "")];
        auto checkWithReplaceEscapes = checkitems.dup;
        checkWithReplaceEscapes[replaceItem][1] = "\r\n\f\b%";

        import std.meta: AliasSeq;
        Check[] withoutSpaces(Check[] it)
        {
            import std.algorithm;
            import std.array;
            return it.filter!(v => v[0] != Space).array;
        }
        foreach(T; AliasSeq!(char, wchar, dchar))
        {
            import std.conv: to;
            verifyJson!(false, false, false)(jsonData.to!(T[]), withoutSpaces(checkitems));
            verifyJson!(false, true, false)(jsonData.to!(T[]), withoutSpaces(checkitems));
            verifyJson!(true, false, false)(jsonData.to!(T[]), withoutSpaces(checkWithReplaceEscapes));
            verifyJson!(true, true, false)(jsonData.to!(T[]), withoutSpaces(checkWithReplaceEscapes));
            verifyJson!(false, false, true)(jsonData.to!(T[]), checkitems);
            verifyJson!(false, true, true)(jsonData.to!(T[]), checkitems);
            verifyJson!(true, false, true)(jsonData.to!(T[]), checkWithReplaceEscapes);
            verifyJson!(true, true, true)(jsonData.to!(T[]), checkWithReplaceEscapes);
        }

        // We should be able to parse values outside objects.
        verifyJson!(false, false, false)(q"{123.456}", [Check(Number, "123.456"), Check(EOF, "")]);
        verifyJson!(false, true, false)(q"{123.456}", [Check(Number, "123.456"), Check(EOF, "")]);
        // now, test to make sure the parser fails properly
        verifyJson!(false, false, false)(q"{{123.456}}", [Check(ObjectStart, "{"), Check(Error, "123.456")]);
        verifyJson!(false, true, false)(q"{{123.456}}", [Check(ObjectStart, "{"), Check(Error, "123.456")]);
    }
}

unittest
{
    // test caching
    auto jsonData = q"{{"a": 1,"b": 123.456, "c": null}}";
    auto parser = jsonData.jsonTokenizer!(ParseConfig(false));
    bool check(parser.Element item, JSONToken token, string expected)
    {
        return(item.token == token && item.data == expected);
    }
    with(JSONToken)
    {
        assert(check(parser.next, ObjectStart, "{"));
        assert(check(parser.next, String, "a"));
        assert(check(parser.next, Colon, ":"));

        // cache the start of a value
        auto startIdx = parser.index;
        assert(check(parser.next, Number, "1"));
        assert(check(parser.next, Comma, ","));
        assert(check(parser.next, String, "b"));
        assert(check(parser.next, Colon, ":"));

        // replay the cache for the value of a
        parser.index = startIdx;
        assert(check(parser.next, Number, "1"));
        assert(check(parser.next, Comma, ","));

        // now with the cache still in there, restart the cache
        assert(check(parser.next, String, "b"));
        assert(check(parser.next, Colon, ":"));
        assert(check(parser.next, Number, "123.456"));
        assert(check(parser.next, Comma, ","));

        // replay b again, use jumpBack instead
        parser.jumpBack(4);
        assert(check(parser.next, String, "b"));
        assert(check(parser.next, Colon, ":"));
        // test out releasing cached data
        parser.flushCache();
        assert(check(parser.next, Number, "123.456"));
        assert(check(parser.next, Comma, ","));
        assert(check(parser.next, String, "c"));
        assert(check(parser.next, Colon, ":"));
        assert(check(parser.next, Null, "null"));
        // the cache should now be exhausted
        // NOTE: the cache doesn't work the same any more. It's always used.
        //assert(parser.chain.window.length == 0);
        //assert(parser.cIdx == 0);
        assert(check(parser.next, ObjectEnd, "}"));
    }
}

unittest
{
    // test skipping items
    auto jsonData = q"{{"a" : 1, "b" : {"c" : [1,2,3], "d" : { "hello" : "world" }}}}";

    auto parser = jsonData.jsonTokenizer!(ParseConfig(false));
    bool check(parser.Element item, JSONToken token, string expected)
    {
        if(item.token == token && item.data == expected)
            return true;
        import std.stdio;
        writeln(item);
        return false;
    }
    parser.Element doSkip()
    {
        auto token = parser.skipItem();
        return parser.next;
    }
    auto idx = parser.index;
    auto item = doSkip(); // skip it all;
    assert(check(item, JSONToken.EOF, ""));
    assert(parser.chain.pos == jsonData.length);

    // start over
    parser.index = idx;
    assert(check(parser.next, JSONToken.ObjectStart, "{"));
    assert(check(doSkip, JSONToken.Comma, ","));
    assert(check(parser.next, JSONToken.String, "b"));
    assert(check(doSkip, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.EOF, ""));
    assert(parser.chain.pos == jsonData.length);

    // another try
    parser.index = idx;
    assert(check(parser.next, JSONToken.ObjectStart, "{"));
    assert(check(doSkip, JSONToken.Comma, ","));
    assert(check(parser.next, JSONToken.String, "b"));
    assert(check(parser.next, JSONToken.Colon, ":"));
    assert(check(parser.next, JSONToken.ObjectStart, "{"));
    assert(check(parser.next, JSONToken.String, "c"));
    assert(check(doSkip, JSONToken.Comma, ","));
    assert(check(parser.next, JSONToken.String, "d"));
    assert(check(doSkip, JSONToken.ObjectEnd, "}"));
    assert(check(doSkip, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.EOF, ""));
    assert(parser.chain.pos == jsonData.length);

    // test parseTo
    parser.index = idx;
    assert(parser.parseTo("b", "d", "hello"));
    assert(check(parser.next, JSONToken.String, "world"));
    assert(check(parser.next, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.EOF, ""));
    assert(parser.chain.pos == jsonData.length);
}

// JSON5 tests!
unittest
{
    // test skipping items
    auto jsonData = q"{{'a' : 1, b : {c : [1,2,NaN,Infinity, 0x55, 55., .2], t : { false9: 'world' }}}}";

    auto parser = jsonData.jsonTokenizer!(ParseConfig(false, true));
    bool check(parser.Element item, JSONToken token, string expected)
    {
        if(item.token == token && item.data == expected)
            return true;
        import std.stdio;
        writeln(item);
        return false;
    }
    auto idx = parser.index;
    parser.Element doSkip()
    {
        auto token = parser.skipItem();
        return parser.next;
    }
    auto item = doSkip(); // skip it all;
    assert(check(item, JSONToken.EOF, ""));
    assert(parser.chain.pos == jsonData.length);

    // start over
    parser.index = idx;
    assert(check(parser.next, JSONToken.ObjectStart, "{"));
    assert(check(doSkip, JSONToken.Comma, ","));
    assert(check(parser.next, JSONToken.Symbol, "b"));
    assert(check(doSkip, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.EOF, ""));
    assert(parser.chain.pos == jsonData.length);

    // another try
    parser.index = idx;
    assert(check(parser.next, JSONToken.ObjectStart, "{"));
    assert(check(doSkip, JSONToken.Comma, ","));
    assert(check(parser.next, JSONToken.Symbol, "b"));
    assert(check(parser.next, JSONToken.Colon, ":"));
    assert(check(parser.next, JSONToken.ObjectStart, "{"));
    assert(check(parser.next, JSONToken.Symbol, "c"));
    assert(check(doSkip, JSONToken.Comma, ","));
    assert(check(parser.next, JSONToken.Symbol, "t"));
    assert(check(doSkip, JSONToken.ObjectEnd, "}"));
    assert(check(doSkip, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.EOF, ""));
    assert(parser.chain.pos == jsonData.length);

    // test parseTo
    parser.index = idx;
    assert(parser.parseTo("b", "t", "false9"));
    auto n = parser.next;
    assert(check(n, JSONToken.String, "world"));
    assert(n.isSingleQuoteString);
    assert(check(parser.next, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.ObjectEnd, "}"));
    assert(check(parser.next, JSONToken.EOF, ""));
    assert(parser.chain.pos == jsonData.length);
}

// more JSON5 (comments, and other stuff)
unittest
{
    auto jsonData = `
// This is a comment, it should be included
   // and this should also be
{
    /* object that has some comment on it
       a multi-line comment to boot!
     */
    something:
    {
        'abc': 123 // another comment
    },
}`;
    auto parser = jsonData.jsonTokenizer!(ParseConfig(false, true, false, true));
    bool check(parser.Element item, JSONToken token, string expected)
    {
        if(item.token == token && item.data == expected)
            return true;
        import std.stdio;
        writeln(item);
        return false;
    }
    with(JSONToken)
    {
        assert(check(parser.next, Comment, "// This is a comment, it should be included\n"));
        assert(check(parser.next, Comment, "// and this should also be\n"));
        assert(check(parser.next, ObjectStart, "{"));
        assert(check(parser.next, Comment, "/* object that has some comment on it\n       a multi-line comment to boot!\n     */"));
        assert(check(parser.next, Symbol, "something"));
        assert(check(parser.next, Colon, ":"));
        assert(check(parser.next, ObjectStart, "{"));
        auto n = parser.next;
        assert(check(n, String, "abc"));
        assert(n.isSingleQuoteString);
        assert(check(parser.next, Colon, ":"));
        assert(check(parser.next, Number, "123"));
        assert(check(parser.next, Comment, "// another comment\n"));
        assert(check(parser.next, ObjectEnd, "}"));
        assert(check(parser.next, Comma, ","));
        assert(check(parser.next, ObjectEnd, "}"));
        assert(check(parser.next, EOF, ""));
        assert(parser.chain.pos == jsonData.length);
    }
}
