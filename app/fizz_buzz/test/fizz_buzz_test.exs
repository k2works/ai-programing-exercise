defmodule FizzBuzzTest do
  use ExUnit.Case

  test "1を渡したら文字列1を返す" do
    assert FizzBuzz.generate(1) == "1"
  end

  test "2を渡したら文字列2を返す" do
    assert FizzBuzz.generate(2) == "2"
  end
end
