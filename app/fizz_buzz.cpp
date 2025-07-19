#include "fizz_buzz.h"

std::string FizzBuzz::generate(int number) {
    bool is_fizz = number % 3 == 0;
    bool is_buzz = number % 5 == 0;
    
    switch (type_) {
        case 1: // 通常のFizzBuzz
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
        case 2: // 数字のみ
            return std::to_string(number);
        case 3: // FizzBuzzの場合のみ
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
        default:
            throw std::runtime_error("該当するタイプは存在しません");
    }
}

std::vector<std::string> FizzBuzz::generate_list() {
    list_.clear();
    for (int i = 1; i <= MAX_NUMBER; i++) {
        list_.push_back(generate(i));
    }
    return list_;
}