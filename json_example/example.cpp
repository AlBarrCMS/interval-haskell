#include "json/json.h"
#include <iostream>
#include <fstream>
#include <iterator>
#include <algorithm>

int main(int argc, char ** argv) {
    std::string input_filename = "example_input.json";
    std::string output_filename = "example_output.json";

    std::cout << "Parsing " << input_filename <<
        " and printing some of its contents" << std::endl;

    Json::Value root;   // 'root' will contain the root value after parsing.
    // Read from a file:
    std::ifstream example_input(input_filename, std::ifstream::binary);
    example_input >> root;
    example_input.close();
    // Alternatively, you could read from stdin:
    // std::cin >> root;

    // // You can also read into a particular sub-value.
    // std::cin >> root["subtree"];

    // Get the value of the member of root named 'encoding',
    // and return 'UTF-8' if there is no such member.
    std::string encoding = root.get("my-encoding", "UTF-32").asString();
    std::cout << "my-encoding=" << encoding << std::endl;

    // Get the value of the member of root named 'plug-ins'; return a 'null' value if
    // there is no such member.
    const Json::Value plugins = root["my-plug-ins"];
    // Iterate over the sequence elements.
    for (unsigned int index = 0; index < plugins.size(); ++index) {
        std::cout << "my-plugin0=" << plugins[index].asString() << std::endl;
    }

    int indent_length = root["indent"].get("length", 3).asInt();
    std::cout << "indent_length=" << indent_length << std::endl;
    bool indent_use_space = root["indent"].get("use_space", true).asBool();
    std::cout << "indent_use_space=" << indent_use_space << std::endl;

    std::cout << std::endl << "Modifying some fields and adding new ones"
        << std::endl;

    // Modify some fields
    // Since Json::Value has an implicit constructor for all value types, it is not
    // necessary to explicitly construct the Json::Value object.
    root["encoding"] = "NewEncoding";
    root["indent"]["length"] = 4;
    root["indent"]["use_space"] = false;

    // Make an array [1, 2, 3]
    // From http://stackoverflow.com/a/14211157
    Json::Value vec(Json::arrayValue);
    vec.append(Json::Value(1));
    vec.append(Json::Value(2));
    vec.append(Json::Value(3));

    // Add new fields
    root["competitors"]["home"]["name"] = "Liverpool";
    root["competitors"]["away"]["code"] = 89223;  // We will overwrite this
    root["competitors"]["away"]["name"] = "Aston Villa";
    root["competitors"]["away"]["code"] = vec;

    std::cout << "Writing to " << output_filename << std::endl;
    // Write the new JSON structure to a file
    std::ofstream example_output(output_filename, std::ofstream::binary);
    example_output << root << std::endl;
    example_output.close();
}
