# JSON parsing/serialization library for iopipe

This library allows parsing/serializing JSON using the [https://github.com/schveiguy/iopipe](iopipe) library. At the moment, the library itself is quite functional, but the documentation is sorely lacking (for now).

Look in the parser module for raw data parsing. Everything else is built on top of this.

The dom module contains `JSONValue`, which is a JSON specific object for containing a JSON tree's data. It also has functions to convert a JSON stream to `JSONValue` items.

The serialize module contains functions to serialize and deserialize custom types to and from a text iopipe in JSON format.

Look for the documentation to be filled out in the near future, and some more functionality to be added as needed.
