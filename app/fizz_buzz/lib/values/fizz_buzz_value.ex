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