# FizzBuzzタイプのプロトコル定義
defprotocol FizzBuzzType do
  @doc "数値をFizzBuzz形式に変換する"
  def generate(type, number)
end

# FizzBuzzタイプ1の実装
defmodule FizzBuzzType1 do
  defstruct []

  defimpl FizzBuzzType do
    def generate(_type, number) do
      cond do
        rem(number, 15) == 0 -> "FizzBuzz"
        rem(number, 3) == 0 -> "Fizz"
        rem(number, 5) == 0 -> "Buzz"
        true -> to_string(number)
      end
    end
  end
end

# FizzBuzzタイプ2の実装（5と7で異なるルール）
defmodule FizzBuzzType2 do
  defstruct []

  defimpl FizzBuzzType do
    def generate(_type, number) do
      cond do
        rem(number, 35) == 0 -> "FizzBuzz"
        rem(number, 5) == 0 -> "Fizz"
        rem(number, 7) == 0 -> "Buzz"
        true -> to_string(number)
      end
    end
  end
end

# FizzBuzzタイプ3の実装（3と5で逆のルール）
defmodule FizzBuzzType3 do
  defstruct []

  defimpl FizzBuzzType do
    def generate(_type, number) do
      cond do
        rem(number, 15) == 0 -> "BuzzFizz"
        rem(number, 5) == 0 -> "Fizz"
        rem(number, 3) == 0 -> "Buzz"
        true -> to_string(number)
      end
    end
  end
end

defmodule FizzBuzz do
  @moduledoc """
  FizzBuzz問題を解くためのモジュールです。

  数値を受け取り、以下のルールに従って文字列を返します：
  - 3で割り切れる場合: "Fizz"
  - 5で割り切れる場合: "Buzz"  
  - 3と5の両方で割り切れる場合: "FizzBuzz"
  - それ以外: 数値の文字列表現
  """

  @enforce_keys [:type]
  defstruct [:type]

  def new(1), do: %FizzBuzz{type: %FizzBuzzType1{}}
  def new(2), do: %FizzBuzz{type: %FizzBuzzType2{}}
  def new(3), do: %FizzBuzz{type: %FizzBuzzType3{}}

  def get_type(%FizzBuzz{type: type}), do: type

  def generate(number) when rem(number, 15) == 0, do: "FizzBuzz"
  def generate(number) when rem(number, 3) == 0, do: "Fizz"
  def generate(number) when rem(number, 5) == 0, do: "Buzz"
  def generate(number), do: to_string(number)

  def generate(number, type) do
    case type do
      1 -> FizzBuzzType.generate(%FizzBuzzType1{}, number)
      2 -> FizzBuzzType.generate(%FizzBuzzType2{}, number)
      3 -> FizzBuzzType.generate(%FizzBuzzType3{}, number)
    end
  end

  def generate_with_instance(%FizzBuzz{type: type}, number) do
    FizzBuzzType.generate(type, number)
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
