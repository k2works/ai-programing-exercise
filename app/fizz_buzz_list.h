#pragma once
#include <vector>
#include <string>

class FizzBuzzList {
public:
    explicit FizzBuzzList(const std::vector<std::string>& list);
    FizzBuzzList(const FizzBuzzList& other);
    FizzBuzzList& operator=(const FizzBuzzList& other);
    ~FizzBuzzList() = default;
    
    const std::vector<std::string>& value() const;
    std::string to_string() const;
    FizzBuzzList add(const std::vector<std::string>& other) const;
    size_t size() const;
    const std::string& operator[](size_t index) const;
    bool empty() const;
    
private:
    std::vector<std::string> value_;
};