#include <iostream>

#include "fizz_buzz.h"

int main() {
    std::cout << "FizzBuzz 1 to 100:" << std::endl;
    for (int i = 1; i <= 100; i++) {
        std::cout << FizzBuzz::generate(i) << std::endl;
    }
    return 0;
}