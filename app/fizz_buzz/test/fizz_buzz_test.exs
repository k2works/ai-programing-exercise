defmodule FizzBuzzTest do
  use ExUnit.Case

  describe "タイプごとに出力を切り替えることができる - タイプ1の場合" do
    test "1を渡したら文字列1を返す" do
      assert FizzBuzz.generate(1, 1) == "1"
    end

    test "2を渡したら文字列2を返す" do
      assert FizzBuzz.generate(2, 1) == "2"
    end

    test "3を渡したら文字列Fizzを返す" do
      assert FizzBuzz.generate(3, 1) == "Fizz"
    end

    test "5を渡したら文字列Buzzを返す" do
      assert FizzBuzz.generate(5, 1) == "Buzz"
    end

    test "15を渡したら文字列FizzBuzzを返す" do
      assert FizzBuzz.generate(15, 1) == "FizzBuzz"
    end

    test "1から100までの数を返す" do
      result = FizzBuzz.list(100)
      assert length(result) == 100
      assert Enum.at(result, 0) == "1"
      assert Enum.at(result, 1) == "2"
      assert Enum.at(result, 99) == "Buzz"
    end

    test "1から100までのFizzBuzzの配列を返す" do
      result = FizzBuzz.list(100)
      # 3
      assert Enum.at(result, 2) == "Fizz"
      # 5
      assert Enum.at(result, 4) == "Buzz"
      # 15
      assert Enum.at(result, 14) == "FizzBuzz"
    end
  end

  describe "タイプごとに出力を切り替えることができる - タイプ2の場合" do
    test "1を渡したら文字列1を返す" do
      assert FizzBuzz.generate(1, 2) == "1"
    end
  end

  describe "タイプごとに出力を切り替えることができる - タイプ3の場合" do
    test "1を渡したら文字列1を返す" do
      assert FizzBuzz.generate(1, 3) == "1"
    end
  end

  describe "タイプごとに出力を切り替えることができる - それ以外のタイプの場合" do
    test "存在しないタイプを渡したらエラーになる" do
      assert_raise CaseClauseError, fn ->
        FizzBuzz.generate(1, 4)
      end
    end
  end
end
