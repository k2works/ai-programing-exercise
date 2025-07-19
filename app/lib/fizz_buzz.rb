class FizzBuzz
  MAX_NUMBER = 100
  attr_reader :list, :type

  def initialize(type)
    @type = type
  end

  def generate(number)
    is_fizz = number.modulo(3).zero?
    is_buzz = number.modulo(5).zero?

    case @type
    when 1
      return 'FizzBuzz' if is_fizz && is_buzz
      return 'Fizz' if is_fizz
      return 'Buzz' if is_buzz

      number.to_s
    when 2
      number.to_s
    when 3
      return 'FizzBuzz' if is_fizz && is_buzz

      number.to_s
    else
      raise '該当するタイプは存在しません'
    end
  end

  def generate_list
    @list = (1..MAX_NUMBER).map { |n| generate(n) }
  end
end
