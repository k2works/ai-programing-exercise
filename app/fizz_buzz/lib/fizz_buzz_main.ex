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

  def new(1), do: {:ok, %FizzBuzz{type: %FizzBuzzType1{}}}
  def new(2), do: {:ok, %FizzBuzz{type: %FizzBuzzType2{}}}
  def new(3), do: {:ok, %FizzBuzz{type: %FizzBuzzType3{}}}
  def new(type), do: {:error, {:invalid_type, type}}

  def get_type(%FizzBuzz{type: type}), do: type

  def generate(number) when rem(number, 15) == 0, do: "FizzBuzz"
  def generate(number) when rem(number, 3) == 0, do: "Fizz"
  def generate(number) when rem(number, 5) == 0, do: "Buzz"
  def generate(number), do: to_string(number)

  def generate(number, type) when is_integer(number) and number > 0 do
    with {:ok, fizzbuzz} <- new(type) do
      {:ok, FizzBuzzType.generate(fizzbuzz.type, number)}
    end
  end

  def generate(number, _type) when not is_integer(number) or number <= 0 do
    {:error, {:invalid_number, number}}
  end

  def execute(%FizzBuzz{type: type}, number) when is_integer(number) and number > 0 do
    value = FizzBuzzType.generate(type, number)
    {:ok, FizzBuzzValue.new(number, value)}
  end

  def execute(%FizzBuzz{}, number) when not is_integer(number) or number <= 0 do
    {:error, {:invalid_number, number}}
  end

  def create_list(max) when is_integer(max) and max > 0 do
    result = 1..max
    |> Stream.map(&create_value_safe/1)
    |> Enum.to_list()
    |> collect_results()

    case result do
      {:ok, values} -> {:ok, FizzBuzzList.new(values)}
      error -> error
    end
  end

  def create_list(max) when not is_integer(max) or max <= 0 do
    {:error, {:invalid_max, max}}
  end

  defp create_value_safe(number) do
    case generate(number) do
      value when is_binary(value) -> {:ok, FizzBuzzValue.new(number, value)}
      error -> error
    end
  end

  defp collect_results(results) do
    results
    |> Enum.reduce_while({:ok, []}, fn
      {:ok, value}, {:ok, acc} -> {:cont, {:ok, [value | acc]}}
      {:error, _} = error, _ -> {:halt, error}
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      error -> error
    end
  end

  def display(max \\ 100) do
    with {:ok, fizzbuzz_list} <- create_list(max) do
      fizzbuzz_list
      |> FizzBuzzList.get_values()
      |> Enum.each(&print_value/1)
      {:ok, :displayed}
    end
  end

  defp print_value(value) do
    value
    |> FizzBuzzValue.to_string()
    |> IO.puts()
  end

  # 関数型アプローチによる改善版（Streamベース）
  def functional_generate(range, type \\ 1) do
    result = range
    |> Stream.map(&create_fizzbuzz_value_safe(&1, type))
    |> Enum.to_list()
    |> collect_results()

    case result do
      {:ok, values} -> {:ok, FizzBuzzList.new(values)}
      error -> error
    end
  end

  # 高階関数を使った柔軟な生成器
  def generate_with_transformer(range, type \\ 1, transformer \\ &Function.identity/1) do
    with {:ok, fizzbuzz_list} <- functional_generate(range, type) do
      transformed_values = fizzbuzz_list
      |> FizzBuzzList.get_values()
      |> Enum.map(transformer)
      
      {:ok, transformed_values}
    end
  end

  defp create_fizzbuzz_value_safe(number, type) do
    with {:ok, value} <- generate(number, type) do
      {:ok, FizzBuzzValue.new(number, value)}
    end
  end

  def parallel_generate(range, type \\ 1, opts \\ []) do
    default_opts = [ordered: true, max_concurrency: System.schedulers_online()]
    final_opts = Keyword.merge(default_opts, opts)

    result = range
    |> Task.async_stream(&create_fizzbuzz_value_safe(&1, type), final_opts)
    |> Stream.map(&extract_result/1)
    |> Enum.to_list()
    |> collect_results()

    case result do
      {:ok, values} -> {:ok, FizzBuzzList.new(values)}
      error -> error
    end
  end

  # モナド的なチェーン処理
  def process_chain(input, operations) when is_list(operations) do
    Enum.reduce_while(operations, {:ok, input}, fn operation, {:ok, value} ->
      case operation.(value) do
        {:ok, new_value} -> {:cont, {:ok, new_value}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  # 関数合成のためのユーティリティ
  def compose(f, g) do
    fn x -> f.(g.(x)) end
  end

  def pipe_functions(functions) when is_list(functions) do
    fn input ->
      Enum.reduce(functions, input, fn f, acc -> f.(acc) end)
    end
  end

  # カリー化された関数生成器
  def curry_generate(type) do
    fn number -> generate(number, type) end
  end

  def curry_execute(fizzbuzz) do
    fn number -> execute(fizzbuzz, number) end
  end

  # Stream を使った遅延評価の拡張
  def lazy_infinite_fizzbuzz(type \\ 1) do
    Stream.iterate(1, &(&1 + 1))
    |> Stream.map(&create_fizzbuzz_value_safe(&1, type))
  end

  def take_while_condition(stream, condition) do
    stream
    |> Stream.take_while(fn
      {:ok, value} -> condition.(value)
      {:error, _} -> false
    end)
  end

  defp extract_result({:ok, {:ok, value}}), do: {:ok, value}
  defp extract_result({:ok, {:error, _} = error}), do: error
  defp extract_result({:exit, reason}), do: {:error, {:task_exit, reason}}
end