#include "fizz_buzz.h"

FizzBuzz::FizzBuzz(int type) {
    switch (type) {
        case 1:
            type_ = createType01();
            break;
        case 2:
            type_ = createType02();
            break;
        case 3:
            type_ = createType03();
            break;
        default:
            throw std::runtime_error("該当するタイプは存在しません");
    }
}

int FizzBuzz::type() const {
    if (dynamic_cast<FizzBuzzType01*>(type_.get())) return 1;
    if (dynamic_cast<FizzBuzzType02*>(type_.get())) return 2;
    if (dynamic_cast<FizzBuzzType03*>(type_.get())) return 3;
    return 0;
}

std::string FizzBuzz::generate(int number) {
    return type_->generate(number);
}

std::vector<std::string> FizzBuzz::generate_list() {
    list_.clear();
    for (int i = 1; i <= MAX_NUMBER; i++) {
        list_.push_back(generate(i));
    }
    return list_;
}