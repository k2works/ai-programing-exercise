# FizzBuzzタイプ2の実装（5と7で異なるルール）
defmodule FizzBuzzType2 do
  use FizzBuzzTypeBase

  defimpl FizzBuzzType do
    def generate(_type, number) do
      FizzBuzzTypeBase.basic_fizzbuzz(number, 5, 7, "Fizz", "Buzz")
    end
  end
end