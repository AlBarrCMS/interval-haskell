This example uses JsonCpp (https://github.com/open-source-parsers/jsoncpp) for
C++ JSON parsing.

JsonCpp works by generating one cpp file and two header files that you should
include in your project. To set up:
git clone https://github.com/open-source-parsers/jsoncpp
cd jsoncpp
git checkout 1.7.2  # Or the latest 1.y.z release
python amalgamate.py
mv dist/* YOUR_PROJECT_PATH

This directory includes the generated files: jsoncpp.cpp, json/json.h, and
json/json-forwards.h.

Run `make` and then `./example` to compile and run an example program. This
example is based on the documentation at
https://github.com/open-source-parsers/jsoncpp/wiki.
