#include <fstream>
#include <iostream>


#include "scanner.h"
#include "parser.h"
#include "AST.h"

template <typename T>
class unique_ref final {
private:
    std::unique_ptr<T> pointer;

public:
    unique_ref(T& ref) : pointer{&ref} {}

    T* operator->() {
        return &get();
    }

    T& get() const {
        return *pointer.get();
    }

    const T& cget() const noexcept {
        return *pointer.get();
    }

    void reset(T& newRef) noexcept {
        pointer.reset(&newRef);
    }
};

struct Test {
    int a;
    int b;

    Test(int a, int b) : a{a}, b{b} {}
};

int main(int argc, char **argv)
{
    unique_ref<Test> test{*(new Test{1, 2})};
    test.get();
    test.cget();

    int g = test->a;

    std::string filename;
    if (argc > 1) {
        filename = std::string(argv[1]);
    } else {
        filename = "test.ar";
    }
    std::cout << "Using " << filename << "\n";

    std::ifstream file(filename, std::ios::in | std::ios::binary);
    std::string testString((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());

    auto parser = Parser{std::move(testString)};

    try {
        auto d = parser.declaration();
        std::cout << *d;
        std::cout << "\n";
    } catch (ParserException exception) {
        std::cout << "EXCEPTION\n";
        std::cout << int(exception.cause) << "\n";
        std::cout << exception.token.chars << "\n";
    }

    return 0;
    std::cout << testString << "\n";
    Scanner scanner(std::move(testString));

    Token t = scanner.next();
    while (t.type != TokenType::EndOfFile) {
        std::cout << t.chars << " " << int(t.type) << "\n";
        t = scanner.next();
    }
}
