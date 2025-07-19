#include "fizz_buzz_type.h"

std::string FizzBuzzType01::generate(int number) {
    bool is_fizz = number % 3 == 0;
    bool is_buzz = number % 5 == 0;

    if (is_fizz && is_buzz) {
        return "FizzBuzz";
    }
    if (is_fizz) {
        return "Fizz";
    }
    if (is_buzz) {
        return "Buzz";
    }
    return std::to_string(number);
}

std::string FizzBuzzType02::generate(int number) {
    return std::to_string(number);
}

std::string FizzBuzzType03::generate(int number) {
    bool is_fizz = number % 3 == 0;
    bool is_buzz = number % 5 == 0;

    if (is_fizz && is_buzz) {
        return "FizzBuzz";
    }
    if (is_fizz) {
        return "Fizz";
    }
    if (is_buzz) {
        return "Buzz";
    }
    return "";
}