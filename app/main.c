#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void assert_equal(const char* expected, const char* actual, const char* test_name) {
    if (strcmp(expected, actual) == 0) {
        printf("✓ %s\n", test_name);
    } else {
        printf("✗ %s: expected '%s', got '%s'\n", test_name, expected, actual);
        exit(1);
    }
}

char* fizz_buzz(int n) {
    static char result[16];
    if (n % 3 == 0 && n % 5 == 0) {
        strcpy(result, "FizzBuzz");
        return result;
    }
    if (n % 3 == 0) {
        strcpy(result, "Fizz");
        return result;
    }
    if (n % 5 == 0) {
        strcpy(result, "Buzz");
        return result;
    }
    sprintf(result, "%d", n);
    return result;
}

void test_number_to_string() {
    assert_equal("1", fizz_buzz(1), "test_number_to_string");
}

void test_fizz() {
    assert_equal("Fizz", fizz_buzz(3), "test_fizz");
}

void test_buzz() {
    assert_equal("Buzz", fizz_buzz(5), "test_buzz");
}

void test_fizz_buzz() {
    assert_equal("FizzBuzz", fizz_buzz(15), "test_fizz_buzz");
}

void run_tests() {
    printf("Running tests...\n");
    test_number_to_string();
    test_fizz();
    test_buzz();
    test_fizz_buzz();
    printf("All tests passed!\n");
}

void print_fizz_buzz_1_to_100() {
    printf("\nFizzBuzz 1 to 100:\n");
    for (int i = 1; i <= 100; i++) {
        printf("%s\n", fizz_buzz(i));
    }
}

int main() {
    run_tests();
    print_fizz_buzz_1_to_100();
    return 0;
}