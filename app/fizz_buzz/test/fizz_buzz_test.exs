defmodule FizzBuzzTest do
  use ExUnit.Case

  describe "関数型プログラミング - Result型パターン" do
    test "generate/2 は正常な場合 {:ok, value} を返す" do
      assert FizzBuzz.generate(1, 1) == {:ok, "1"}
      assert FizzBuzz.generate(3, 1) == {:ok, "Fizz"}
      assert FizzBuzz.generate(5, 1) == {:ok, "Buzz"}
      assert FizzBuzz.generate(15, 1) == {:ok, "FizzBuzz"}
    end

    test "generate/2 は無効なタイプの場合エラーを返す" do
      assert FizzBuzz.generate(1, 4) == {:error, {:invalid_type, 4}}
    end

    test "generate/2 は無効な数値の場合エラーを返す" do
      assert FizzBuzz.generate(-1, 1) == {:error, {:invalid_number, -1}}
      assert FizzBuzz.generate(0, 1) == {:error, {:invalid_number, 0}}
      assert FizzBuzz.generate("invalid", 1) == {:error, {:invalid_number, "invalid"}}
    end

    test "new/1 は正常な場合 {:ok, fizzbuzz} を返す" do
      assert {:ok, %FizzBuzz{}} = FizzBuzz.new(1)
      assert {:ok, %FizzBuzz{}} = FizzBuzz.new(2)
      assert {:ok, %FizzBuzz{}} = FizzBuzz.new(3)
    end

    test "new/1 は無効なタイプの場合エラーを返す" do
      assert FizzBuzz.new(4) == {:error, {:invalid_type, 4}}
    end

    test "create_list/1 は正常な場合 {:ok, fizzbuzz_list} を返す" do
      assert {:ok, fizzbuzz_list} = FizzBuzz.create_list(10)
      assert FizzBuzzList.size(fizzbuzz_list) == 10
    end

    test "create_list/1 は無効なmaxの場合エラーを返す" do
      assert FizzBuzz.create_list(-1) == {:error, {:invalid_max, -1}}
      assert FizzBuzz.create_list(0) == {:error, {:invalid_max, 0}}
    end
  end

  describe "タイプごとに出力を切り替えることができる - タイプ1の場合" do
    test "1を渡したら文字列1を返す" do
      assert {:ok, "1"} = FizzBuzz.generate(1, 1)
    end

    test "2を渡したら文字列2を返す" do
      assert {:ok, "2"} = FizzBuzz.generate(2, 1)
    end

    test "3を渡したら文字列Fizzを返す" do
      assert {:ok, "Fizz"} = FizzBuzz.generate(3, 1)
    end

    test "5を渡したら文字列Buzzを返す" do
      assert {:ok, "Buzz"} = FizzBuzz.generate(5, 1)
    end

    test "15を渡したら文字列FizzBuzzを返す" do
      assert {:ok, "FizzBuzz"} = FizzBuzz.generate(15, 1)
    end

    test "1から100までの数を返す" do
      assert {:ok, result} = FizzBuzz.create_list(100)
      assert FizzBuzzList.size(result) == 100
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 0)) == "1"
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 1)) == "2"
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 99)) == "Buzz"
    end

    test "1から100までのFizzBuzzの配列を返す" do
      assert {:ok, result} = FizzBuzz.create_list(100)
      # 3
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 2)) == "Fizz"
      # 5
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 4)) == "Buzz"
      # 15
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 14)) == "FizzBuzz"
    end
  end

  describe "タイプごとに出力を切り替えることができる - タイプ2の場合" do
    test "1を渡したら文字列1を返す" do
      assert {:ok, "1"} = FizzBuzz.generate(1, 2)
    end

    test "5を渡したら文字列Fizzを返す" do
      assert {:ok, "Fizz"} = FizzBuzz.generate(5, 2)
    end

    test "7を渡したら文字列Buzzを返す" do
      assert {:ok, "Buzz"} = FizzBuzz.generate(7, 2)
    end

    test "35を渡したら文字列FizzBuzzを返す" do
      assert {:ok, "FizzBuzz"} = FizzBuzz.generate(35, 2)
    end
  end

  describe "タイプごとに出力を切り替えることができる - タイプ3の場合" do
    test "1を渡したら文字列1を返す" do
      assert {:ok, "1"} = FizzBuzz.generate(1, 3)
    end

    test "3を渡したら文字列Buzzを返す" do
      assert {:ok, "Buzz"} = FizzBuzz.generate(3, 3)
    end

    test "5を渡したら文字列Fizzを返す" do
      assert {:ok, "Fizz"} = FizzBuzz.generate(5, 3)
    end

    test "15を渡したら文字列FizzBuzzを返す" do
      assert {:ok, "FizzBuzz"} = FizzBuzz.generate(15, 3)
    end
  end

  describe "タイプごとに出力を切り替えることができる - それ以外のタイプの場合" do
    test "存在しないタイプを渡したらエラーを返す" do
      assert FizzBuzz.generate(1, 4) == {:error, {:invalid_type, 4}}
    end
  end

  describe "構造体を使ったインスタンス生成" do
    test "タイプ1のインスタンスを作成して1を渡したら文字列1を返す" do
      assert {:ok, fizzbuzz} = FizzBuzz.new(1)
      assert {:ok, result} = FizzBuzz.execute(fizzbuzz, 1)
      assert FizzBuzzValue.to_string(result) == "1"
      assert FizzBuzzValue.get_number(result) == 1
    end

    test "タイプ1のインスタンスを作成して3を渡したら文字列Fizzを返す" do
      assert {:ok, fizzbuzz} = FizzBuzz.new(1)
      assert {:ok, result} = FizzBuzz.execute(fizzbuzz, 3)
      assert FizzBuzzValue.to_string(result) == "Fizz"
      assert FizzBuzzValue.get_number(result) == 3
    end

    test "タイプ2のインスタンスを作成して5を渡したら文字列Fizzを返す" do
      assert {:ok, fizzbuzz} = FizzBuzz.new(2)
      assert {:ok, result} = FizzBuzz.execute(fizzbuzz, 5)
      assert FizzBuzzValue.to_string(result) == "Fizz"
      assert FizzBuzzValue.get_number(result) == 5
    end

    test "タイプ3のインスタンスを作成して3を渡したら文字列Buzzを返す" do
      assert {:ok, fizzbuzz} = FizzBuzz.new(3)
      assert {:ok, result} = FizzBuzz.execute(fizzbuzz, 3)
      assert FizzBuzzValue.to_string(result) == "Buzz"
      assert FizzBuzzValue.get_number(result) == 3
    end

    test "構造体のtypeフィールドは読み取り専用" do
      assert {:ok, fizzbuzz} = FizzBuzz.new(1)
      assert %FizzBuzzType1{} = FizzBuzz.get_type(fizzbuzz)
      # setterは存在しない（不変データ）
    end

    test "無効なタイプでインスタンス作成するとエラー" do
      assert FizzBuzz.new(-1) == {:error, {:invalid_type, -1}}
    end

    test "execute は無効な数値の場合エラーを返す" do
      assert {:ok, fizzbuzz} = FizzBuzz.new(1)
      assert FizzBuzz.execute(fizzbuzz, -1) == {:error, {:invalid_number, -1}}
    end
  end

  describe "関数型アプローチによる改善" do
    test "Stream を使った遅延評価による FizzBuzz 生成" do
      assert {:ok, result} = FizzBuzz.functional_generate(1..10, 1)
      assert FizzBuzzList.size(result) == 10
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 2)) == "Fizz"
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 4)) == "Buzz"
    end

    test "並列処理による FizzBuzz 生成" do
      assert {:ok, result} = FizzBuzz.parallel_generate(1..10, 1)
      assert FizzBuzzList.size(result) == 10
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 2)) == "Fizz"
      assert FizzBuzzValue.to_string(FizzBuzzList.at(result, 4)) == "Buzz"
    end

    test "高階関数による変換処理" do
      upcase_transform = fn value -> 
        value 
        |> FizzBuzzValue.to_string() 
        |> String.upcase()
      end
      
      assert {:ok, transformed} = FizzBuzz.generate_with_transformer(1..5, 1, upcase_transform)
      assert transformed == ["1", "2", "FIZZ", "4", "BUZZ"]
    end

    test "カリー化された関数生成器" do
      type1_generator = FizzBuzz.curry_generate(1)
      assert {:ok, "Fizz"} = type1_generator.(3)
      assert {:ok, "Buzz"} = type1_generator.(5)
    end

    test "関数合成" do
      double = fn x -> x * 2 end
      to_string_fn = fn x -> to_string(x) end
      composed = FizzBuzz.compose(to_string_fn, double)
      
      assert composed.(5) == "10"
    end

    test "無限ストリームと条件付き取得" do
      stream = FizzBuzz.lazy_infinite_fizzbuzz(1)
      
      # 最初の10個を取得
      first_10 = stream |> Enum.take(10)
      assert length(first_10) == 10
    end

    test "プロセスチェーン処理" do
      operations = [
        fn x -> {:ok, x + 1} end,
        fn x -> {:ok, x * 2} end,
        fn x -> {:ok, to_string(x)} end
      ]
      
      assert {:ok, "6"} = FizzBuzz.process_chain(2, operations)
      
      # エラーケース
      error_operations = [
        fn x -> {:ok, x + 1} end,
        fn _x -> {:error, :some_error} end,
        fn x -> {:ok, x * 2} end
      ]
      
      assert {:error, :some_error} = FizzBuzz.process_chain(2, error_operations)
    end
  end
end
