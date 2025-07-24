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