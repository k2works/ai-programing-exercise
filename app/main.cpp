#include <iostream>
#include <string>
#include <cassert>

class TestRunner {
public:
    static void assert_equal(const std::string& expected, const std::string& actual, const std::string& test_name) {
        if (expected == actual) {
            std::cout << "✓ " << test_name << std::endl;
        } else {
            std::cout << "✗ " << test_name << ": expected '" << expected << "', got '" << actual << "'" << std::endl;
            exit(1);
        }
    }
};

class FizzBuzz {
public:
    static std::string generate(int n) {
        if (n % 3 == 0 && n % 5 == 0) {
            return "FizzBuzz";
        }
        if (n % 3 == 0) {
            return "Fizz";
        }
        if (n % 5 == 0) {
            return "Buzz";
        }
        return std::to_string(n);
    }
};

void test_number_to_string() {
    TestRunner::assert_equal("1", FizzBuzz::generate(1), "test_number_to_string");
}

void test_fizz() {
    TestRunner::assert_equal("Fizz", FizzBuzz::generate(3), "test_fizz");
}

void test_buzz() {
    TestRunner::assert_equal("Buzz", FizzBuzz::generate(5), "test_buzz");
}

void test_fizz_buzz() {
    TestRunner::assert_equal("FizzBuzz", FizzBuzz::generate(15), "test_fizz_buzz");
}

void run_tests() {
    std::cout << "Running tests..." << std::endl;
    test_number_to_string();
    test_fizz();
    test_buzz();
    test_fizz_buzz();
    std::cout << "All tests passed!" << std::endl;
}

void print_fizz_buzz_1_to_100() {
    std::cout << "\nFizzBuzz 1 to 100:" << std::endl;
    for (int i = 1; i <= 100; i++) {
        std::cout << FizzBuzz::generate(i) << std::endl;
    }
}

int main() {
    run_tests();
    print_fizz_buzz_1_to_100();
    return 0;
}