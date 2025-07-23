defmodule FizzBuzzTest do
  use ExUnit.Case

  describe "FizzBuzz" do
    test "1を渡したら文字列1を返す" do
      assert FizzBuzz.generate(1) == "1"
    end

    test "2を渡したら文字列2を返す" do
      assert FizzBuzz.generate(2) == "2"
    end

    test "3を渡したら文字列Fizzを返す" do
      assert FizzBuzz.generate(3) == "Fizz"
    end

    test "5を渡したら文字列Buzzを返す" do
      assert FizzBuzz.generate(5) == "Buzz"
    end
  end
end
