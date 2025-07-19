#include "fizz_buzz.h"

std::string FizzBuzz::generate(int number) {
    switch (type_) {
        case 1:
            return createType01()->generate(number);
        case 2:
            return createType02()->generate(number);
        case 3:
            return createType03()->generate(number);
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