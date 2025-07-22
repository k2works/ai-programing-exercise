#pragma once
#include <string>
#include <memory>

class FizzBuzzType {
public:
    virtual ~FizzBuzzType() = default;
    virtual std::string generate(int number) = 0;
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