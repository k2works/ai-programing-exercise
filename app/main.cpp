#include <iostream>

#include "fizz_buzz.h"

int main() {
    FizzBuzz fizzbuzz;
    std::cout << "FizzBuzz 1 to 100:" << std::endl;
    auto result = fizzbuzz.generate_list();
    for (const auto& item : result) {
        std::cout << item << std::endl;
    }
    return 0;
}