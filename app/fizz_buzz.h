#pragma once
#include <string>
#include <memory>
#include <vector>
#include "fizz_buzz_type.h"

class FizzBuzz {
   public:
    static const int MAX_NUMBER = 100;
    
    static std::string generate(int number);
    static std::string generate(int number, int type);
    
    FizzBuzz(int type = 1);
    std::string generate_instance(int number);
    const std::vector<std::string>& list() const;
    void generate_list();
    
   private:
    std::unique_ptr<FizzBuzzType> type_;
    std::vector<std::string> list_;
    static std::unique_ptr<FizzBuzzType> create(int type);
};