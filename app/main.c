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
    sprintf(result, "%d", n);
    return result;
}

void test_number_to_string() {
    assert_equal("1", fizz_buzz(1), "test_number_to_string");
}

int main() {
    printf("Running tests...\n");
    test_number_to_string();
    printf("All tests passed!\n");
    return 0;
}