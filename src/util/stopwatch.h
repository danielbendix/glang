#include <chrono>
#include <iostream>

class Stopwatch {
    using time = decltype(std::chrono::high_resolution_clock::now());

    time start = std::chrono::high_resolution_clock::now();
public:
    void lap(const char *description) {
        time end = std::chrono::high_resolution_clock::now();

        auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start);
        std::cout << description << ": " << duration << "\n";
        start = std::chrono::high_resolution_clock::now();
    }
};
