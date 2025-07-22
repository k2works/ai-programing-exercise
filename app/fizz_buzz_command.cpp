#include "fizz_buzz_command.h"

// Base command implementation
void FizzBuzzCommand::assert_positive(int number) const {
    if (number <= 0) {
        throw AssertionFailedError("値は正の値のみ許可されています: " + std::to_string(number));
    }
}

// Value command implementation
FizzBuzzValueCommand::FizzBuzzValueCommand(std::unique_ptr<FizzBuzzType> type) 
    : type_(std::move(type)) {}

std::string FizzBuzzValueCommand::execute(int number) {
    assert_positive(number);
    return type_->generate(number);
}

// List command implementation
FizzBuzzListCommand::FizzBuzzListCommand(std::unique_ptr<FizzBuzzType> type)
    : type_(std::move(type)) {}

std::string FizzBuzzListCommand::execute(int number) {
    assert_positive(number);
    return type_->generate(number);
}

FizzBuzzList FizzBuzzListCommand::execute_list(int max_number) {
    assert_positive(max_number);
    
    std::vector<std::string> result;
    for (int i = 1; i <= max_number; ++i) {
        result.push_back(type_->generate(i));
    }
    
    return FizzBuzzList(result);
}