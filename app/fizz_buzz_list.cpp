#include "fizz_buzz_list.h"
#include <sstream>

FizzBuzzList::FizzBuzzList(const std::vector<std::string>& list) : value_(list) {}

FizzBuzzList::FizzBuzzList(const FizzBuzzList& other) : value_(other.value_) {}

FizzBuzzList& FizzBuzzList::operator=(const FizzBuzzList& other) {
    if (this != &other) {
        value_ = other.value_;
    }
    return *this;
}

const std::vector<std::string>& FizzBuzzList::value() const {
    return value_;
}

std::string FizzBuzzList::to_string() const {
    std::ostringstream oss;
    oss << "[";
    for (size_t i = 0; i < value_.size(); ++i) {
        if (i > 0) oss << ", ";
        oss << "\"" << value_[i] << "\"";
    }
    oss << "]";
    return oss.str();
}

FizzBuzzList FizzBuzzList::add(const std::vector<std::string>& other) const {
    std::vector<std::string> new_list = value_;
    new_list.insert(new_list.end(), other.begin(), other.end());
    return FizzBuzzList(new_list);
}

size_t FizzBuzzList::size() const {
    return value_.size();
}

const std::string& FizzBuzzList::operator[](size_t index) const {
    return value_[index];
}

bool FizzBuzzList::empty() const {
    return value_.empty();
}