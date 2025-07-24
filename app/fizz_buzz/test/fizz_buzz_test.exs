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

    test "5を渡したら文字列Fizzを返す" do
      assert FizzBuzz.generate(5, 2) == "Fizz"
    end

    test "7を渡したら文字列Buzzを返す" do
      assert FizzBuzz.generate(7, 2) == "Buzz"
    end

    test "35を渡したら文字列FizzBuzzを返す" do
      assert FizzBuzz.generate(35, 2) == "FizzBuzz"
    end
  end

  describe "タイプごとに出力を切り替えることができる - タイプ3の場合" do
    test "1を渡したら文字列1を返す" do
      assert FizzBuzz.generate(1, 3) == "1"
    end

    test "3を渡したら文字列Buzzを返す" do
      assert FizzBuzz.generate(3, 3) == "Buzz"
    end

    test "5を渡したら文字列Fizzを返す" do
      assert FizzBuzz.generate(5, 3) == "Fizz"
    end

    test "15を渡したら文字列FizzBuzzを返す" do
      assert FizzBuzz.generate(15, 3) == "FizzBuzz"
    end
  end

  describe "タイプごとに出力を切り替えることができる - それ以外のタイプの場合" do
    test "存在しないタイプを渡したらエラーになる" do
      assert_raise CaseClauseError, fn ->
        FizzBuzz.generate(1, 4)
      end
    end
  end

  describe "構造体を使ったインスタンス生成" do
    test "タイプ1のインスタンスを作成して1を渡したら文字列1を返す" do
      fizzbuzz = FizzBuzz.new(1)
      assert FizzBuzz.generate_with_instance(fizzbuzz, 1) == "1"
    end

    test "タイプ1のインスタンスを作成して3を渡したら文字列Fizzを返す" do
      fizzbuzz = FizzBuzz.new(1)
      assert FizzBuzz.generate_with_instance(fizzbuzz, 3) == "Fizz"
    end

    test "タイプ2のインスタンスを作成して5を渡したら文字列Fizzを返す" do
      fizzbuzz = FizzBuzz.new(2)
      assert FizzBuzz.generate_with_instance(fizzbuzz, 5) == "Fizz"
    end

    test "タイプ3のインスタンスを作成して3を渡したら文字列Buzzを返す" do
      fizzbuzz = FizzBuzz.new(3)
      assert FizzBuzz.generate_with_instance(fizzbuzz, 3) == "Buzz"
    end

    test "構造体のtypeフィールドは読み取り専用" do
      fizzbuzz = FizzBuzz.new(1)
      assert %FizzBuzzType1{} = FizzBuzz.get_type(fizzbuzz)
      # setterは存在しない（不変データ）
    end

    test "無効なタイプでインスタンス作成するとエラー" do
      assert_raise FunctionClauseError, fn ->
        FizzBuzz.new(-1)
      end
    end
  end
end
