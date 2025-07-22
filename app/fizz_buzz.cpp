#include "fizz_buzz.h"
#include <stdexcept>

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
    auto fb_type = create(type);
    return fb_type->generate(number);
}

FizzBuzz::FizzBuzz(int type) : type_(create(type)) {}

std::string FizzBuzz::generate_instance(int number) {
    return type_->generate(number);
}

std::unique_ptr<FizzBuzzType> FizzBuzz::create(int type) {
    switch (type) {
        case 1:
            return std::make_unique<FizzBuzzType01>();
        case 2:
            return std::make_unique<FizzBuzzType02>();
        case 3:
            return std::make_unique<FizzBuzzType03>();
        default:
            throw std::invalid_argument("該当するタイプは存在しません");
    }
}