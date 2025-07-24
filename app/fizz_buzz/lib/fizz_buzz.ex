# FizzBuzzタイプのプロトコル定義
defprotocol FizzBuzzType do
  @doc "数値をFizzBuzz形式に変換する"
  def generate(type, number)
end

# FizzBuzzタイプの基底ビヘイビア
defmodule FizzBuzzTypeBase do
  @moduledoc """
  FizzBuzzタイプの共通機能を提供する基底モジュール
  """

  @doc """
  基本的なFizzBuzz変換ロジック
  """
  def basic_fizzbuzz(number, fizz_divisor, buzz_divisor, fizz_text, buzz_text) do
    fizzbuzz_divisor = fizz_divisor * buzz_divisor
    cond do
      rem(number, fizzbuzz_divisor) == 0 -> fizz_text <> buzz_text
      rem(number, fizz_divisor) == 0 -> fizz_text
      rem(number, buzz_divisor) == 0 -> buzz_text
      true -> to_string(number)
    end
  end

  defmacro __using__(_opts) do
    quote do
      defstruct []
    end
  end
end

# FizzBuzzタイプ1の実装
defmodule FizzBuzzType1 do
  use FizzBuzzTypeBase

  defimpl FizzBuzzType do
    def generate(_type, number) do
      FizzBuzzTypeBase.basic_fizzbuzz(number, 3, 5, "Fizz", "Buzz")
    end
  end
end

# FizzBuzzタイプ2の実装（5と7で異なるルール）
defmodule FizzBuzzType2 do
  use FizzBuzzTypeBase

  defimpl FizzBuzzType do
    def generate(_type, number) do
      FizzBuzzTypeBase.basic_fizzbuzz(number, 5, 7, "Fizz", "Buzz")
    end
  end
end

# FizzBuzzタイプ3の実装（3と5で逆のルール）
defmodule FizzBuzzType3 do
  use FizzBuzzTypeBase

  defimpl FizzBuzzType do
    def generate(_type, number) do
      FizzBuzzTypeBase.basic_fizzbuzz(number, 5, 3, "Fizz", "Buzz")
    end
  end
end

# FizzBuzzの値オブジェクト
defmodule FizzBuzzValue do
  @enforce_keys [:number, :value]
  defstruct [:number, :value]

  def new(number, value) do
    %FizzBuzzValue{number: number, value: value}
  end

  def get_number(%FizzBuzzValue{number: number}), do: number
  def get_value(%FizzBuzzValue{value: value}), do: value
  def to_string(%FizzBuzzValue{value: value}), do: value
end

# FizzBuzzのリストオブジェクト
defmodule FizzBuzzList do
  @enforce_keys [:values]
  defstruct [:values]

  def new(values) when is_list(values) do
    %FizzBuzzList{values: values}
  end

  def get_values(%FizzBuzzList{values: values}), do: values
  def size(%FizzBuzzList{values: values}), do: length(values)
  def at(%FizzBuzzList{values: values}, index), do: Enum.at(values, index)
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

  def execute(%FizzBuzz{type: type}, number) do
    value = FizzBuzzType.generate(type, number)
    FizzBuzzValue.new(number, value)
  end

  def create_list(max) do
    values = 1..max
    |> Enum.map(&generate/1)
    |> Enum.with_index(1)
    |> Enum.map(fn {value, number} -> FizzBuzzValue.new(number, value) end)
    
    FizzBuzzList.new(values)
  end

  def display(max \\ 100) do
    create_list(max)
    |> FizzBuzzList.get_values()
    |> Enum.each(fn value -> IO.puts(FizzBuzzValue.to_string(value)) end)
  end

  # 関数型アプローチによる改善版
  def functional_generate(range, type \\ 1) do
    range
    |> Stream.map(&create_fizzbuzz_value(&1, type))
    |> Enum.to_list()
    |> FizzBuzzList.new()
  end

  defp create_fizzbuzz_value(number, type) do
    value = generate(number, type)
    FizzBuzzValue.new(number, value)
  end

  def parallel_generate(range, type \\ 1) do
    range
    |> Task.async_stream(&create_fizzbuzz_value(&1, type), ordered: true)
    |> Enum.map(fn {:ok, result} -> result end)
    |> FizzBuzzList.new()
  end
end
