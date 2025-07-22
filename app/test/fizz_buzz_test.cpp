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

// Test for object-oriented version
class FizzBuzzObjectTest : public ::testing::Test {
   protected:
    void SetUp() override {}
    void TearDown() override {}
};

TEST_F(FizzBuzzObjectTest, test_タイプ1インスタンスで1を渡したら文字列1を返す) {
    FizzBuzz fizzbuzz(1);
    EXPECT_EQ("1", fizzbuzz.generate_instance(1));
}

TEST_F(FizzBuzzObjectTest, test_タイプ1インスタンスで3を渡したら文字列Fizzを返す) {
    FizzBuzz fizzbuzz(1);
    EXPECT_EQ("Fizz", fizzbuzz.generate_instance(3));
}

TEST_F(FizzBuzzObjectTest, test_タイプ1インスタンスで5を渡したら文字列Buzzを返す) {
    FizzBuzz fizzbuzz(1);
    EXPECT_EQ("Buzz", fizzbuzz.generate_instance(5));
}

TEST_F(FizzBuzzObjectTest, test_タイプ1インスタンスで15を渡したら文字列FizzBuzzを返す) {
    FizzBuzz fizzbuzz(1);
    EXPECT_EQ("FizzBuzz", fizzbuzz.generate_instance(15));
}

// Test for encapsulation
class FizzBuzzEncapsulationTest : public ::testing::Test {
   protected:
    void SetUp() override {}
    void TearDown() override {}
};

TEST_F(FizzBuzzEncapsulationTest, test_listが空の配列を返す) {
    FizzBuzz fizzbuzz;
    EXPECT_TRUE(fizzbuzz.list().empty());
}

TEST_F(FizzBuzzEncapsulationTest, test_generate_listで配列を生成して取得できる) {
    FizzBuzz fizzbuzz;
    fizzbuzz.generate_list();
    auto result = fizzbuzz.list();
    
    EXPECT_EQ(100, result.size());
    EXPECT_EQ("1", result[0]);
    EXPECT_EQ("2", result[1]);
    EXPECT_EQ("Fizz", result[2]);
    EXPECT_EQ("4", result[3]);
    EXPECT_EQ("Buzz", result[4]);
    EXPECT_EQ("FizzBuzz", result[14]);
}