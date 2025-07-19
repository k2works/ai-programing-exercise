#pragma once
#include <string>
#include <stdexcept>
#include <vector>
#include <memory>
#include "fizz_buzz_type.h"

class FizzBuzz {
   private:
    std::vector<std::string> list_;
    int type_;
    
   public:
    static const int MAX_NUMBER = 100;
    
    FizzBuzz(int type = 1) : type_(type) {}
    
    std::vector<std::string> list() const { return list_; }
    int type() const { return type_; }
    
    static std::unique_ptr<FizzBuzzType01> createType01() { return std::make_unique<FizzBuzzType01>(); }
    static std::unique_ptr<FizzBuzzType02> createType02() { return std::make_unique<FizzBuzzType02>(); }
    static std::unique_ptr<FizzBuzzType03> createType03() { return std::make_unique<FizzBuzzType03>(); }
    
    std::string generate(int number);
    std::vector<std::string> generate_list();
};