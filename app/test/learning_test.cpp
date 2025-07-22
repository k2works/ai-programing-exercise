#include "../fizz_buzz.h"
#include "../src/domain/model/fizz_buzz_value.h"
#include "../src/domain/model/fizz_buzz_list.h"
#include "../src/application/fizz_buzz_command.h"

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

// Test for value object
class FizzBuzzValueTest : public ::testing::Test {
   protected:
    void SetUp() override {}
    void TearDown() override {}
};

TEST_F(FizzBuzzValueTest, test_値オブジェクトでタイプ1を作成) {
    FizzBuzzValue type(1);
    EXPECT_EQ(1, type.value());
}

TEST_F(FizzBuzzValueTest, test_値オブジェクトでタイプ2を作成) {
    FizzBuzzValue type(2);
    EXPECT_EQ(2, type.value());
}

TEST_F(FizzBuzzValueTest, test_値オブジェクトでタイプ3を作成) {
    FizzBuzzValue type(3);
    EXPECT_EQ(3, type.value());
}

TEST_F(FizzBuzzValueTest, test_無効なタイプで例外が発生) {
    EXPECT_THROW(FizzBuzzValue(0), std::invalid_argument);
    EXPECT_THROW(FizzBuzzValue(4), std::invalid_argument);
}

TEST_F(FizzBuzzValueTest, test_値オブジェクトでFizzBuzzインスタンスを作成) {
    FizzBuzzValue type(1);
    FizzBuzz fizzbuzz(type);
    EXPECT_EQ("1", fizzbuzz.generate_instance(1));
    EXPECT_EQ("Fizz", fizzbuzz.generate_instance(3));
}

TEST_F(FizzBuzzValueTest, test_値オブジェクトの等価性) {
    FizzBuzzValue type1(1);
    FizzBuzzValue type2(1);
    FizzBuzzValue type3(2);
    
    EXPECT_TRUE(type1 == type2);
    EXPECT_FALSE(type1 == type3);
    EXPECT_TRUE(type1 != type3);
}

// Test for first-class collection
class FizzBuzzListTest : public ::testing::Test {
   protected:
    void SetUp() override {}
    void TearDown() override {}
};

TEST_F(FizzBuzzListTest, test_空のリストを作成) {
    std::vector<std::string> empty_list;
    FizzBuzzList fizz_buzz_list(empty_list);
    
    EXPECT_TRUE(fizz_buzz_list.empty());
    EXPECT_EQ(0, fizz_buzz_list.size());
}

TEST_F(FizzBuzzListTest, test_要素を持つリストを作成) {
    std::vector<std::string> list = {"1", "2", "Fizz"};
    FizzBuzzList fizz_buzz_list(list);
    
    EXPECT_FALSE(fizz_buzz_list.empty());
    EXPECT_EQ(3, fizz_buzz_list.size());
    EXPECT_EQ("1", fizz_buzz_list[0]);
    EXPECT_EQ("2", fizz_buzz_list[1]);
    EXPECT_EQ("Fizz", fizz_buzz_list[2]);
}

TEST_F(FizzBuzzListTest, test_リストの文字列表現) {
    std::vector<std::string> list = {"1", "2", "Fizz"};
    FizzBuzzList fizz_buzz_list(list);
    
    EXPECT_EQ("[\"1\", \"2\", \"Fizz\"]", fizz_buzz_list.to_string());
}

TEST_F(FizzBuzzListTest, test_リストに要素を追加) {
    std::vector<std::string> list1 = {"1", "2"};
    std::vector<std::string> list2 = {"Fizz", "4"};
    FizzBuzzList fizz_buzz_list(list1);
    
    FizzBuzzList new_list = fizz_buzz_list.add(list2);
    
    EXPECT_EQ(4, new_list.size());
    EXPECT_EQ("1", new_list[0]);
    EXPECT_EQ("2", new_list[1]);
    EXPECT_EQ("Fizz", new_list[2]);
    EXPECT_EQ("4", new_list[3]);
    
    // 元のリストは変更されない
    EXPECT_EQ(2, fizz_buzz_list.size());
}

TEST_F(FizzBuzzListTest, test_FizzBuzzからファーストクラスコレクションを取得) {
    FizzBuzz fizzbuzz;
    fizzbuzz.generate_list();
    
    FizzBuzzList collection = fizzbuzz.list_as_collection();
    
    EXPECT_EQ(100, collection.size());
    EXPECT_EQ("1", collection[0]);
    EXPECT_EQ("Fizz", collection[2]);
    EXPECT_EQ("FizzBuzz", collection[14]);
}

// Test for exception handling and commands
class FizzBuzzCommandTest : public ::testing::Test {
   protected:
    void SetUp() override {}
    void TearDown() override {}
};

TEST_F(FizzBuzzCommandTest, test_ValueCommandで正の値を処理) {
    auto type = std::make_unique<FizzBuzzType01>();
    FizzBuzzValueCommand command(std::move(type));
    
    EXPECT_EQ("1", command.execute(1));
    EXPECT_EQ("Fizz", command.execute(3));
    EXPECT_EQ("FizzBuzz", command.execute(15));
}

TEST_F(FizzBuzzCommandTest, test_ValueCommandで0を渡すと例外が発生) {
    auto type = std::make_unique<FizzBuzzType01>();
    FizzBuzzValueCommand command(std::move(type));
    
    EXPECT_THROW(command.execute(0), AssertionFailedError);
}

TEST_F(FizzBuzzCommandTest, test_ValueCommandで負の値を渡すと例外が発生) {
    auto type = std::make_unique<FizzBuzzType01>();
    FizzBuzzValueCommand command(std::move(type));
    
    EXPECT_THROW(command.execute(-1), AssertionFailedError);
}

TEST_F(FizzBuzzCommandTest, test_ListCommandでリストを生成) {
    auto type = std::make_unique<FizzBuzzType01>();
    FizzBuzzListCommand command(std::move(type));
    
    FizzBuzzList result = command.execute_list(5);
    
    EXPECT_EQ(5, result.size());
    EXPECT_EQ("1", result[0]);
    EXPECT_EQ("2", result[1]);
    EXPECT_EQ("Fizz", result[2]);
    EXPECT_EQ("4", result[3]);
    EXPECT_EQ("Buzz", result[4]);
}

TEST_F(FizzBuzzCommandTest, test_ListCommandで0を渡すと例外が発生) {
    auto type = std::make_unique<FizzBuzzType01>();
    FizzBuzzListCommand command(std::move(type));
    
    EXPECT_THROW(command.execute_list(0), AssertionFailedError);
}

TEST_F(FizzBuzzCommandTest, test_ListCommandで負の値を渡すと例外が発生) {
    auto type = std::make_unique<FizzBuzzType01>();
    FizzBuzzListCommand command(std::move(type));
    
    EXPECT_THROW(command.execute_list(-1), AssertionFailedError);
}

TEST_F(FizzBuzzCommandTest, test_例外メッセージの内容) {
    auto type = std::make_unique<FizzBuzzType01>();
    FizzBuzzValueCommand command(std::move(type));
    
    try {
        command.execute(-5);
        FAIL() << "Expected AssertionFailedError";
    } catch (const AssertionFailedError& e) {
        std::string message = e.what();
        EXPECT_TRUE(message.find("値は正の値のみ許可されています") != std::string::npos);
        EXPECT_TRUE(message.find("-5") != std::string::npos);
    }
}