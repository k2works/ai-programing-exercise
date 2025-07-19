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
    FizzBuzz fizzbuzz{1};
    void SetUp() override {
        // Setup code if needed
    }
};

// 三の倍数の場合
TEST_F(FizzBuzzType1Test, test_3を渡したら文字列Fizzを返す) { EXPECT_EQ("Fizz", fizzbuzz.generate(3)); }

// 五の倍数の場合
TEST_F(FizzBuzzType1Test, test_5を渡したら文字列Buzzを返す) { EXPECT_EQ("Buzz", fizzbuzz.generate(5)); }

// 三と五の倍数の場合
TEST_F(FizzBuzzType1Test, test_15を渡したら文字列FizzBuzzを返す) { EXPECT_EQ("FizzBuzz", fizzbuzz.generate(15)); }

// その他の場合
TEST_F(FizzBuzzType1Test, test_1を渡したら文字列1を返す) { EXPECT_EQ("1", fizzbuzz.generate(1)); }

// 1から100までのFizzBuzzの配列を返す
class FizzBuzzListTest : public ::testing::Test {
   protected:
    FizzBuzz fizzbuzz;
    std::vector<std::string> result;
    void SetUp() override {
        fizzbuzz.generate_list();
        result = fizzbuzz.list();
    }
};

TEST_F(FizzBuzzListTest, test_配列の初めは文字列の1を返す) { EXPECT_EQ("1", result[0]); }

TEST_F(FizzBuzzListTest, test_配列の最後は文字列のBuzzを返す) { EXPECT_EQ("Buzz", result[99]); }

TEST_F(FizzBuzzListTest, test_配列の2番目は文字列のFizzを返す) { EXPECT_EQ("Fizz", result[2]); }

TEST_F(FizzBuzzListTest, test_配列の4番目は文字列のBuzzを返す) { EXPECT_EQ("Buzz", result[4]); }

TEST_F(FizzBuzzListTest, test_配列の14番目は文字列のFizzBuzzを返す) { EXPECT_EQ("FizzBuzz", result[14]); }

// タイプ2の場合（数字のみ）
class FizzBuzzType2Test : public ::testing::Test {
   protected:
    FizzBuzz fizzbuzz{2};
    void SetUp() override {
        // Setup code if needed
    }
};

// 三の倍数の場合
TEST_F(FizzBuzzType2Test, test_3を渡したら文字列3を返す) { EXPECT_EQ("3", fizzbuzz.generate(3)); }

// 五の倍数の場合
TEST_F(FizzBuzzType2Test, test_5を渡したら文字列5を返す) { EXPECT_EQ("5", fizzbuzz.generate(5)); }

// 三と五の倍数の場合
TEST_F(FizzBuzzType2Test, test_15を渡したら文字列15を返す) { EXPECT_EQ("15", fizzbuzz.generate(15)); }

// その他の場合
TEST_F(FizzBuzzType2Test, test_1を渡したら文字列1を返す) { EXPECT_EQ("1", fizzbuzz.generate(1)); }

// タイプ3の場合（FizzBuzzの場合のみプリント、それ以外は空文字列）
class FizzBuzzType3Test : public ::testing::Test {
   protected:
    FizzBuzz fizzbuzz{3};
    void SetUp() override {
        // Setup code if needed
    }
};

// 三の倍数の場合
TEST_F(FizzBuzzType3Test, test_3を渡したら文字列Fizzを返す) { EXPECT_EQ("Fizz", fizzbuzz.generate(3)); }

// 五の倍数の場合
TEST_F(FizzBuzzType3Test, test_5を渡したら文字列Buzzを返す) { EXPECT_EQ("Buzz", fizzbuzz.generate(5)); }

// 三と五の倍数の場合
TEST_F(FizzBuzzType3Test, test_15を渡したら文字列FizzBuzzを返す) { EXPECT_EQ("FizzBuzz", fizzbuzz.generate(15)); }

// その他の場合
TEST_F(FizzBuzzType3Test, test_1を渡したら空文字列を返す) { EXPECT_EQ("", fizzbuzz.generate(1)); }

TEST_F(FizzBuzzType3Test, test_2を渡したら空文字列を返す) { EXPECT_EQ("", fizzbuzz.generate(2)); }

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
            FizzBuzz fizzbuzz{4};
        } catch (const std::runtime_error& e) {
            EXPECT_STREQ("該当するタイプは存在しません", e.what());
            throw;
        }
    }, std::runtime_error);
}