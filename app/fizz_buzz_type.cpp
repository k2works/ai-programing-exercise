#include "fizz_buzz_type.h"

std::string FizzBuzzType01::generate(int number) {
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

std::string FizzBuzzType02::generate(int number) {
    return std::to_string(number);
}

std::string FizzBuzzType03::generate(int number) {
    if (number % 3 == 0 && number % 5 == 0) {
        return "FizzBuzz";
    }
    return std::to_string(number);
}