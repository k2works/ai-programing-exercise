#pragma once
#include <memory>
#include <stdexcept>
#include "fizz_buzz_type.h"
#include "fizz_buzz_value.h"
#include "fizz_buzz_list.h"

// Custom exception for assertion failures
class AssertionFailedError : public std::runtime_error {
public:
    explicit AssertionFailedError(const std::string& message) 
        : std::runtime_error(message) {}
};

// Command interface
class FizzBuzzCommand {
public:
    virtual ~FizzBuzzCommand() = default;
    virtual std::string execute(int number) = 0;
    
protected:
    void assert_positive(int number) const;
};

// Value command implementation
class FizzBuzzValueCommand : public FizzBuzzCommand {
public:
    explicit FizzBuzzValueCommand(std::unique_ptr<FizzBuzzType> type);
    std::string execute(int number) override;
    
private:
    std::unique_ptr<FizzBuzzType> type_;
};

// List command implementation  
class FizzBuzzListCommand : public FizzBuzzCommand {
public:
    explicit FizzBuzzListCommand(std::unique_ptr<FizzBuzzType> type);
    std::string execute(int number) override;
    FizzBuzzList execute_list(int max_number);
    
private:
    std::unique_ptr<FizzBuzzType> type_;
};