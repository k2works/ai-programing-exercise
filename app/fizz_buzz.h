#pragma once
#include <string>
#include <memory>
#include <vector>
#include "fizz_buzz_type.h"
#include "fizz_buzz_value.h"
#include "fizz_buzz_list.h"

class FizzBuzz {
   public:
    static const int MAX_NUMBER = 100;
    
    static std::string generate(int number);
    static std::string generate(int number, int type);
    
    FizzBuzz(int type = 1);
    FizzBuzz(const FizzBuzzValue& type);
    std::string generate_instance(int number);
    const std::vector<std::string>& list() const;
    FizzBuzzList list_as_collection() const;
    void generate_list();
    
   private:
    std::unique_ptr<FizzBuzzType> type_;
    std::vector<std::string> list_;
    static std::unique_ptr<FizzBuzzType> create(int type);
};