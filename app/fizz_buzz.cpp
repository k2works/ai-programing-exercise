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

FizzBuzz::FizzBuzz(const FizzBuzzValue& type) : type_(type.create_type()) {}

std::string FizzBuzz::generate_instance(int number) {
    return type_->generate(number);
}

const std::vector<std::string>& FizzBuzz::list() const {
    return list_;
}

FizzBuzzList FizzBuzz::list_as_collection() const {
    return FizzBuzzList(list_);
}

void FizzBuzz::generate_list() {
    list_.clear();
    for (int i = 1; i <= MAX_NUMBER; ++i) {
        list_.push_back(generate_instance(i));
    }
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