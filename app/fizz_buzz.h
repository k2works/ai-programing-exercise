#pragma once
#include <string>
#include <stdexcept>
#include <vector>

class FizzBuzz {
   private:
    std::vector<std::string> list_;
    
   public:
    static const int MAX_NUMBER = 100;
    
    std::vector<std::string> list() const { return list_; }
    std::string generate(int number, int type = 1);
    std::vector<std::string> generate_list();
};