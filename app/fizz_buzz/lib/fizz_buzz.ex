defmodule FizzBuzz do
  @moduledoc """
  FizzBuzz問題を解くためのモジュールです。

  数値を受け取り、以下のルールに従って文字列を返します：
  - 3で割り切れる場合: "Fizz"
  - 5で割り切れる場合: "Buzz"  
  - 3と5の両方で割り切れる場合: "FizzBuzz"
  - それ以外: 数値の文字列表現
  """
  def generate(number) when rem(number, 15) == 0, do: "FizzBuzz"
  def generate(number) when rem(number, 3) == 0, do: "Fizz"
  def generate(number) when rem(number, 5) == 0, do: "Buzz"
  def generate(number), do: to_string(number)

  def generate(number, type) do
    case type do
      1 -> 
        cond do
          rem(number, 15) == 0 -> "FizzBuzz"
          rem(number, 3) == 0 -> "Fizz"
          rem(number, 5) == 0 -> "Buzz"
          true -> to_string(number)
        end
      2 -> 
        cond do
          rem(number, 15) == 0 -> "FizzBuzz"
          rem(number, 3) == 0 -> "Fizz"
          rem(number, 5) == 0 -> "Buzz"
          true -> to_string(number)
        end
      3 -> 
        cond do
          rem(number, 15) == 0 -> "FizzBuzz"
          rem(number, 3) == 0 -> "Fizz"
          rem(number, 5) == 0 -> "Buzz"
          true -> to_string(number)
        end
    end
  end

  def list(max) do
    1..max
    |> Enum.map(&generate/1)
  end

  def print(max \\ 100) do
    list(max)
    |> Enum.each(&IO.puts/1)
  end
end
