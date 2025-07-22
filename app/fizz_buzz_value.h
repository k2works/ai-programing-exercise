#pragma once
#include <memory>
#include <stdexcept>

// Forward declaration
class FizzBuzzType;

class FizzBuzzValue {
public:
    explicit FizzBuzzValue(int type);
    FizzBuzzValue(const FizzBuzzValue& other);
    FizzBuzzValue& operator=(const FizzBuzzValue& other);
    ~FizzBuzzValue() = default;
    
    std::unique_ptr<FizzBuzzType> create_type() const;
    int value() const;
    bool operator==(const FizzBuzzValue& other) const;
    bool operator!=(const FizzBuzzValue& other) const;
    
private:
    int type_;
    void validate(int type) const;
};