#include "fizz_buzz.h"

std::string FizzBuzz::generate(int number, int type) {
    switch (type) {
        case 1: // 通常のFizzBuzz
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
        case 2: // 数字のみ
            return std::to_string(number);
        case 3: // FizzBuzzの場合のみ
            if (number % 3 == 0 && number % 5 == 0) {
                return "FizzBuzz";
            }
            if (number % 3 == 0) {
                return "Fizz";
            }
            if (number % 5 == 0) {
                return "Buzz";
            }
            return "";
        default:
            return std::to_string(number);
    }
}