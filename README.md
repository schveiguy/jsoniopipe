# JSON parsing/serialization library for iopipe

This library allows parsing/serializing JSON using the [iopipe](https://github.com/schveiguy/iopipe) library.

## Design Overview

jsoniopipe is a [JSON](https://json.org)- and [JSON5](https://json5.org)-compliant parser and serializer. The library provides a streaming parser, which can be applied to any iopipe of character type. It also provides rich mechanisms to serialize data to and from JSON formats. Because this uses iopipe, the entire data does not need to be held in memory for parsing or serialization. Therefore parsing and serialization speed is a major goal.

The intention is to define JSON parsing at any level needed, the lowest level being extracting tokens from a character stream, up to a buffered tokenizer that can be used to deserialize data.

Because there is no required intermediate format, serialization and deserialization provide a direct conversion between the character stream and your D structures or classes. Both directions of serialization are fully customizable using a supplied policy object.

JSON DOM objects are also provided giving runtime-based serialization if desired.

Lifetime management of data from the stream can be configured or inferred based on the stream data type.

## Tokenizer and Parser

The `iopipe.json.parser` module contains all code that is used to lex and parse the incoming stream into JSON tokens. The names and descriptions of the types are as follows:

* `JSONToken` - an enumeration identifying the type of a token. In many cases, the type of the next token can be determined before fully lexing the next item.
* `JSONItem` - the representation of a JSON or JSON5 token from the stream. This identifies the type of token, where it is in the stream/buffer, and parse information about the token.
* `JSONPipe` - the iopipe of JSON or JSON5 tokens that currently have been parsed from the underlying stream. The `window` member provides the current buffer of tokens that have been parsed.
* `JSONPipe.Element` - a `JSONItem` paired with a reference to the containing `JSONPipe`. This provides the convenience of extracting the data without needing to provide the pipe manually.
* `JSONTokenizer` - This is a type which is a stream of JSON or JSON5 tokens with a position as to the last one fetched. This type is useful for deserializers or other mechanisms to keep track of where in the stream you are. This also provides some high-level features such as skipping or jumping to specific items.
* `ParseConfig` - A configuration structure that determines how parsing should be done.

Using the `JSONPipe` you can process data however you want, just like an iopipe provides a way to process raw data however you want. If you want to maintain a definite position in the stream, or use the serialization functions, then you likely want to use the `JSONTokenizer`.

The `ParseConfig` is passed at compile-time to the `JSONPipe` or `JSONTokenizer` to configure how parsing should work. It contains flags for the following options during parse:

* `replaceEscapes` - The underlying pipe contains mutable data, and the first type parsing out string tokens, any escapes should be replaced with the equivalent actual characters or unicode escape sequences. If this is not specified, then the data is left with escapes.
* `JSON5` - enable JSON5 parsing. This version of the parser is not as performant, as JSON5 allows a lot more flexibility in format, and therefore requires more handling. JSON is a simple format which can be parsed only by processing ASCII data, whereas JSON5 allows many more options for spaces and non-string data.
* `includeSpaces` - tokens will include items which contain the insignificant spacing between significant tokens. This allows perfect editing of a file without disturbing the spacing. If this option is false, spaces are not included in the stream of tokens.
* `includeComments` - For JSON5 only. Include tokens containing the comments in the stream. If this option is false, then comments are not included in the stream of tokens. Comment delimiters are included.

All character widths (`char`, `wchar`, and `dchar`) are allowed, making this parser the most flexible it can be.

Each `JSONItem` or `JSONPipe.Element` provides the means to slice the data from the original stream. Care should be taken to not keep this slice around if the underlying stream is a buffer that is being overwritten. Of course, if your underlying pipe is a string, then you can keep the slices as long as you need.

Simple tokens just provide the positions of the slice. JSON strings and numbers also provide a `JSONParseHint` describing what is contained in the token. The hints are as follows:

* `InPlace` - The slice contains a string or simple token whose slice represents the actual D character data. If `config.replaceEscapes` is true, then this is the only hint you should get for string data.
* `Escapes` - The slice contains a string that contains escapes 
* `Int` - The slice contains an integer.
* `Float` - The slice contains a floating point value.
* `Exp` - The slice contains a floating point value which contains an exponent piece (e.g. `+e10`)
* `Hex` - The slice contains a hexadecimal integer. The slice will start with `0x` or `0X`, and contain hexadecimal digits afterwards with upper or lower case letters. This will only appear when parsing JSON5 streams.
* `Infinity` - The slice contains positive or negative infinity. This will only appear when parsing JSON5 streams.
* `NaN` - The slice contains positive or negative NaN. This will only appear when parsing JSON5 streams.

## Serializer and Deserializer

Please see the wiki for more details on serialization and deserialization. (TODO: move that here)

## DOM

If instead of parsing to a strongly-typed object, you wish to deserialize to or serialize from a DOM-style type (Document Object Model), the `iopipe.json.dom` module gives ways to build, manipulate, and serialize data to or from a JSON iopipe stream.

The `JSONValue` type is a templated struct based on the slice type of the underlying stream. This allows it to contain slices of the stream if needed.

There isn't much to the `JSONValue` type as it is a raw tagged union type.

*NOTE: the `JSONValue` type in jsoniopipe is severely underpowered, especially when compared to other JSON DOM types from D libraries, and likely will go through some large breaking changes in the near future. I advise not depending on this type at this time.*

## JSON5 support

JSON5 deserialization has full support in jsoniopipe. Serialization does not currently have support, but it will be added soon.

JSON5 is a backwards-compatible format specification which follows the ECMA-5 script specification for data. A quick list of differences:

* Comments are allowed
* Hex numbers
* No need to quote member names
* Single-quote strings are allowed
* Trailing commas are allowed

These convenient features make JSON5 a preferable format to JSON for human-maintained content such as configuration files. However, due to the more complex parsing requirements, JSON5 is not recommended for sending data over communication channels, where humans are not hand-maintaining the content.

## Issues

Any issues or questions, please file on github, or I can be found on the D language discord.
