#pragma once
#include <string>
#include <stdexcept>
#include <vector>

class FizzBuzz {
   private:
    std::vector<std::string> list_;
    int type_;
    
   public:
    static const int MAX_NUMBER = 100;
    
    FizzBuzz(int type = 1) : type_(type) {}
    
    std::vector<std::string> list() const { return list_; }
    std::string generate(int number);
    std::vector<std::string> generate_list();
};