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
      result = FizzBuzz.create_list(100)
      assert FizzBuzzList.size(result) == 100
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 0)) == "1"
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 1)) == "2"
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 99)) == "Buzz"
    end

    test "1から100までのFizzBuzzの配列を返す" do
      result = FizzBuzz.create_list(100)
      # 3
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 2)) == "Fizz"
      # 5
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 4)) == "Buzz"
      # 15
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 14)) == "FizzBuzz"
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
      result = FizzBuzz.execute(fizzbuzz, 1)
      assert FizzBuzzValue.to_string(result) == "1"
      assert FizzBuzzValue.get_number(result) == 1
    end

    test "タイプ1のインスタンスを作成して3を渡したら文字列Fizzを返す" do
      fizzbuzz = FizzBuzz.new(1)
      result = FizzBuzz.execute(fizzbuzz, 3)
      assert FizzBuzzValue.to_string(result) == "Fizz"
      assert FizzBuzzValue.get_number(result) == 3
    end

    test "タイプ2のインスタンスを作成して5を渡したら文字列Fizzを返す" do
      fizzbuzz = FizzBuzz.new(2)
      result = FizzBuzz.execute(fizzbuzz, 5)
      assert FizzBuzzValue.to_string(result) == "Fizz"
      assert FizzBuzzValue.get_number(result) == 5
    end

    test "タイプ3のインスタンスを作成して3を渡したら文字列Buzzを返す" do
      fizzbuzz = FizzBuzz.new(3)
      result = FizzBuzz.execute(fizzbuzz, 3)
      assert FizzBuzzValue.to_string(result) == "Buzz"
      assert FizzBuzzValue.get_number(result) == 3
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

  describe "関数型アプローチによる改善" do
    test "Stream を使った遅延評価による FizzBuzz 生成" do
      result = FizzBuzz.functional_generate(1..10, 1)
      assert FizzBuzzList.size(result) == 10
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 2)) == "Fizz"
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 4)) == "Buzz"
    end

    test "並列処理による FizzBuzz 生成" do
      result = FizzBuzz.parallel_generate(1..10, 1)
      assert FizzBuzzList.size(result) == 10
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 2)) == "Fizz"
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 4)) == "Buzz"
    end
  end
end
