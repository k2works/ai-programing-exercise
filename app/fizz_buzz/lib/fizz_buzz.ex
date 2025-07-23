defmodule FizzBuzz do
  def generate(number) do
    if rem(number, 3) == 0 do
      "Fizz"
    else
      to_string(number)
    end
  end
end
