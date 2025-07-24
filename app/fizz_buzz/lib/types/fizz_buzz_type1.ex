# FizzBuzzタイプ1の実装
defmodule FizzBuzzType1 do
  use FizzBuzzTypeBase

  defimpl FizzBuzzType do
    def generate(_type, number) do
      FizzBuzzTypeBase.basic_fizzbuzz(number, 3, 5, "Fizz", "Buzz")
    end
  end
end