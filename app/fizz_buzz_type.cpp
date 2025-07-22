#include "fizz_buzz_type.h"

// Base class implementation
bool FizzBuzzType::is_fizz(int number) const {
    return number % 3 == 0;
}

bool FizzBuzzType::is_buzz(int number) const {
    return number % 5 == 0;
}

std::string FizzBuzzType::fizz_buzz_string(int number) const {
    if (is_fizz(number) && is_buzz(number)) {
        return "FizzBuzz";
    }
    if (is_fizz(number)) {
        return "Fizz";
    }
    if (is_buzz(number)) {
        return "Buzz";
    }
    return std::to_string(number);
}

// Type01 implementation
std::string FizzBuzzType01::generate(int number) {
    return fizz_buzz_string(number);
}

std::string FizzBuzzType02::generate(int number) {
    return std::to_string(number);
}

std::string FizzBuzzType03::generate(int number) {
    if (is_fizz(number) && is_buzz(number)) {
        return "FizzBuzz";
    }
    return std::to_string(number);
}