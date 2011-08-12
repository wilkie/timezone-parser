# Timezone-Parser

This project parses the timezone database and generates code in many different
languages that makes direct use of the timezone data. It is clear that there
is no need for the timezone data structures generated for the tzcode
distribution. This is an unnecessary complication.

## Generated Languages:
* D

## Usage
This project is written for the MIT/GNU Scheme, although effort has been made
to support lighter-weight R5 schemes.

Just run the parse.scm file, and it will generate code for all languages.

Later, specific arguments will allow the generation of other languages.
