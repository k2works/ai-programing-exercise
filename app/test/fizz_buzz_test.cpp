#include "../fizz_buzz.h"

#include <gtest/gtest.h>

class FizzBuzzTest : public ::testing::Test {
   protected:
    void SetUp() override {
        // Setup code if needed
    }

    void TearDown() override {
        // Cleanup code if needed
    }
};

// Test for basic number conversion
TEST_F(FizzBuzzTest, test_1を渡したら文字列1を返す) { EXPECT_EQ("1", FizzBuzz::generate(1)); }

TEST_F(FizzBuzzTest, test_3を渡したら文字列Fizzを返す) { EXPECT_EQ("Fizz", FizzBuzz::generate(3)); }

TEST_F(FizzBuzzTest, test_5を渡したら文字列Buzzを返す) { EXPECT_EQ("Buzz", FizzBuzz::generate(5)); }

TEST_F(FizzBuzzTest, test_15を渡したら文字列FizzBuzzを返す) { EXPECT_EQ("FizzBuzz", FizzBuzz::generate(15)); }

// Test for type support
class FizzBuzzTypeTest : public ::testing::Test {
   protected:
    void SetUp() override {}
    void TearDown() override {}
};

TEST_F(FizzBuzzTypeTest, test_タイプ1で1を渡したら文字列1を返す) { 
    EXPECT_EQ("1", FizzBuzz::generate(1, 1)); 
}

TEST_F(FizzBuzzTypeTest, test_タイプ2で1を渡したら文字列1を返す) { 
    EXPECT_EQ("1", FizzBuzz::generate(1, 2)); 
}

TEST_F(FizzBuzzTypeTest, test_タイプ2で3を渡したら文字列3を返す) { 
    EXPECT_EQ("3", FizzBuzz::generate(3, 2)); 
}

TEST_F(FizzBuzzTypeTest, test_タイプ2で5を渡したら文字列5を返す) { 
    EXPECT_EQ("5", FizzBuzz::generate(5, 2)); 
}

TEST_F(FizzBuzzTypeTest, test_タイプ2で15を渡したら文字列15を返す) { 
    EXPECT_EQ("15", FizzBuzz::generate(15, 2)); 
}

TEST_F(FizzBuzzTypeTest, test_タイプ3で1を渡したら文字列1を返す) { 
    EXPECT_EQ("1", FizzBuzz::generate(1, 3)); 
}

TEST_F(FizzBuzzTypeTest, test_タイプ3で3を渡したら文字列3を返す) { 
    EXPECT_EQ("3", FizzBuzz::generate(3, 3)); 
}

TEST_F(FizzBuzzTypeTest, test_タイプ3で5を渡したら文字列5を返す) { 
    EXPECT_EQ("5", FizzBuzz::generate(5, 3)); 
}

TEST_F(FizzBuzzTypeTest, test_タイプ3で15を渡したら文字列FizzBuzzを返す) { 
    EXPECT_EQ("FizzBuzz", FizzBuzz::generate(15, 3)); 
}