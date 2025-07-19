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

const char* greeting() {
    return "hello world";
}

void test_greeting() {
    assert_equal("hello world", greeting(), "test_greeting");
}

int main() {
    printf("Running tests...\n");
    test_greeting();
    printf("All tests passed!\n");
    return 0;
}