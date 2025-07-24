# FizzBuzzタイプのプロトコル定義
defprotocol FizzBuzzType do
  @doc "数値をFizzBuzz形式に変換する"
  def generate(type, number)
end