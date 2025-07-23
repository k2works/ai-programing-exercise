defmodule FizzBuzz do
  def generate(number) do
    cond do
      rem(number, 15) == 0 -> "FizzBuzz"
      rem(number, 3) == 0 -> "Fizz"
      rem(number, 5) == 0 -> "Buzz"
      true -> to_string(number)
    end
  end

  def list(max) do
    1..max
    |> Enum.map(&generate/1)
  end
end
