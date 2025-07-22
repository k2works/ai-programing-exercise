#include "fizz_buzz_value.h"
#include "fizz_buzz_type.h"
#include <stdexcept>

FizzBuzzValue::FizzBuzzValue(int type) : type_(type) {
    validate(type);
}

FizzBuzzValue::FizzBuzzValue(const FizzBuzzValue& other) : type_(other.type_) {}

FizzBuzzValue& FizzBuzzValue::operator=(const FizzBuzzValue& other) {
    if (this != &other) {
        type_ = other.type_;
    }
    return *this;
}

std::unique_ptr<FizzBuzzType> FizzBuzzValue::create_type() const {
    switch (type_) {
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

int FizzBuzzValue::value() const {
    return type_;
}

bool FizzBuzzValue::operator==(const FizzBuzzValue& other) const {
    return type_ == other.type_;
}

bool FizzBuzzValue::operator!=(const FizzBuzzValue& other) const {
    return !(*this == other);
}

void FizzBuzzValue::validate(int type) const {
    if (type < 1 || type > 3) {
        throw std::invalid_argument("該当するタイプは存在しません");
    }
}