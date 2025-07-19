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

// 数を文字列にして返す - タイプ1の場合
class FizzBuzzType1Test : public ::testing::Test {
   protected:
    void SetUp() override {
        // Setup code if needed
    }
};

// 三の倍数の場合
TEST_F(FizzBuzzType1Test, test_3を渡したら文字列Fizzを返す) { EXPECT_EQ("Fizz", FizzBuzz::generate(3)); }

// 五の倍数の場合
TEST_F(FizzBuzzType1Test, test_5を渡したら文字列Buzzを返す) { EXPECT_EQ("Buzz", FizzBuzz::generate(5)); }

// 三と五の倍数の場合
TEST_F(FizzBuzzType1Test, test_15を渡したら文字列FizzBuzzを返す) { EXPECT_EQ("FizzBuzz", FizzBuzz::generate(15)); }

// その他の場合
TEST_F(FizzBuzzType1Test, test_1を渡したら文字列1を返す) { EXPECT_EQ("1", FizzBuzz::generate(1)); }

// タイプ2の場合（数字のみ）
class FizzBuzzType2Test : public ::testing::Test {
   protected:
    void SetUp() override {
        // Setup code if needed
    }
};

// 三の倍数の場合
TEST_F(FizzBuzzType2Test, test_3を渡したら文字列3を返す) { EXPECT_EQ("3", FizzBuzz::generate(3, 2)); }

// 五の倍数の場合
TEST_F(FizzBuzzType2Test, test_5を渡したら文字列5を返す) { EXPECT_EQ("5", FizzBuzz::generate(5, 2)); }

// 三と五の倍数の場合
TEST_F(FizzBuzzType2Test, test_15を渡したら文字列15を返す) { EXPECT_EQ("15", FizzBuzz::generate(15, 2)); }

// その他の場合
TEST_F(FizzBuzzType2Test, test_1を渡したら文字列1を返す) { EXPECT_EQ("1", FizzBuzz::generate(1, 2)); }

// タイプ3の場合（FizzBuzzの場合のみプリント、それ以外は空文字列）
class FizzBuzzType3Test : public ::testing::Test {
   protected:
    void SetUp() override {
        // Setup code if needed
    }
};

// 三の倍数の場合
TEST_F(FizzBuzzType3Test, test_3を渡したら文字列Fizzを返す) { EXPECT_EQ("Fizz", FizzBuzz::generate(3, 3)); }

// 五の倍数の場合
TEST_F(FizzBuzzType3Test, test_5を渡したら文字列Buzzを返す) { EXPECT_EQ("Buzz", FizzBuzz::generate(5, 3)); }

// 三と五の倍数の場合
TEST_F(FizzBuzzType3Test, test_15を渡したら文字列FizzBuzzを返す) { EXPECT_EQ("FizzBuzz", FizzBuzz::generate(15, 3)); }

// その他の場合
TEST_F(FizzBuzzType3Test, test_1を渡したら空文字列を返す) { EXPECT_EQ("", FizzBuzz::generate(1, 3)); }

TEST_F(FizzBuzzType3Test, test_2を渡したら空文字列を返す) { EXPECT_EQ("", FizzBuzz::generate(2, 3)); }

// それ以外のタイプの場合
class FizzBuzzOtherTypeTest : public ::testing::Test {
   protected:
    void SetUp() override {
        // Setup code if needed
    }
};

TEST_F(FizzBuzzOtherTypeTest, test_例外を返す) {
    EXPECT_THROW({
        try {
            FizzBuzz::generate(1, 4);
        } catch (const std::runtime_error& e) {
            EXPECT_STREQ("該当するタイプは存在しません", e.what());
            throw;
        }
    }, std::runtime_error);
}