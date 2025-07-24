# FizzBuzzタイプ3の実装（3と5で逆のルール）
defmodule FizzBuzzType3 do
  use FizzBuzzTypeBase

  defimpl FizzBuzzType do
    def generate(_type, number) do
      FizzBuzzTypeBase.basic_fizzbuzz(number, 5, 3, "Fizz", "Buzz")
    end
  end
end