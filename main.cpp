#include <fstream>
#include <iostream>


#include "scanner.h"


int main()
{
    std::ifstream file("test.ar", std::ios::in | std::ios::binary);
    std::string testString((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    //std::string testString = "test \"\" for struct fun -->";
    //std::string testString = "fun test(int a1, float b2) -> int { return a1 + a2 }";

    std::cout << testString << "\n";
    Scanner scanner(std::move(testString));

    Token t = scanner.next();
    while (t.type != TokenType::EndOfFile) {
        //std::cout << int(t.type) << "\n";
        std::cout << t.chars << " " << int(t.type) << "\n";
        t = scanner.next();
    }

}
