#pragma once
#include <string>
#include <memory>

class FizzBuzzType {
public:
    virtual ~FizzBuzzType() = default;
    virtual std::string generate(int number) = 0;
    
protected:
    bool is_fizz(int number) const;
    bool is_buzz(int number) const;
    std::string fizz_buzz_string(int number) const;
};

class FizzBuzzType01 : public FizzBuzzType {
public:
    std::string generate(int number) override;
};

class FizzBuzzType02 : public FizzBuzzType {
public:
    std::string generate(int number) override;
};

class FizzBuzzType03 : public FizzBuzzType {
public:
    std::string generate(int number) override;
};