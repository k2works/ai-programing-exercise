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