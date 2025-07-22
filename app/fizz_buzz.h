#pragma once
#include <string>
#include <memory>
#include "fizz_buzz_type.h"

class FizzBuzz {
   public:
    static std::string generate(int number);
    static std::string generate(int number, int type);
    
    FizzBuzz(int type);
    std::string generate_instance(int number);
    
   private:
    std::unique_ptr<FizzBuzzType> type_;
    static std::unique_ptr<FizzBuzzType> create(int type);
};