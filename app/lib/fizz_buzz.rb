class FizzBuzz
  MAX_NUMBER = 100
  attr_reader :list, :type

  def initialize(type)
    @type = self.class.create(type)
  end

  def self.create(type)
    case type
    when 1
      FizzBuzzType01.new
    when 2
      FizzBuzzType02.new
    when 3
      FizzBuzzType03.new
    else
      raise '該当するタイプは存在しません'
    end
  end

  def generate(number)
    @type.generate(number)
  end

  def generate_list
    @list = (1..MAX_NUMBER).map { |n| generate(n) }
  end
end

class FizzBuzzType01
  def generate(number)
    is_fizz = number.modulo(3).zero?
    is_buzz = number.modulo(5).zero?

    return 'FizzBuzz' if is_fizz && is_buzz
    return 'Fizz' if is_fizz
    return 'Buzz' if is_buzz

    number.to_s
  end
end

class FizzBuzzType02
  def generate(number)
    number.to_s
  end
end

class FizzBuzzType03
  def generate(number)
    is_fizz = number.modulo(3).zero?
    is_buzz = number.modulo(5).zero?

    return 'FizzBuzz' if is_fizz && is_buzz

    number.to_s
  end
end
