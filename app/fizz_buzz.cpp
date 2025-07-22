#include "fizz_buzz.h"

std::string FizzBuzz::generate(int number) {
    if (number % 3 == 0 && number % 5 == 0) {
        return "FizzBuzz";
    }
    if (number % 3 == 0) {
        return "Fizz";
    }
    if (number % 5 == 0) {
        return "Buzz";
    }
    return std::to_string(number);
}

std::string FizzBuzz::generate(int number, int type) {
    if (type == 1) {
        return std::to_string(number);
    }
    if (type == 2) {
        return std::to_string(number);
    }
    if (type == 3) {
        if (number % 3 == 0 && number % 5 == 0) {
            return "FizzBuzz";
        }
        return std::to_string(number);
    }
    return generate(number);
}