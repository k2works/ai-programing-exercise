class FizzBuzzType
  def generate(number)
    raise NotImplementedError
  end
end

class FizzBuzzType01 < FizzBuzzType
  def generate(number)
    is_fizz = number.modulo(3).zero?
    is_buzz = number.modulo(5).zero?

    return 'FizzBuzz' if is_fizz && is_buzz
    return 'Fizz' if is_fizz
    return 'Buzz' if is_buzz

    number.to_s
  end
end

class FizzBuzzType02 < FizzBuzzType
  def generate(number)
    number.to_s
  end
end

class FizzBuzzType03 < FizzBuzzType
  def generate(number)
    is_fizz = number.modulo(3).zero?
    is_buzz = number.modulo(5).zero?

    return 'FizzBuzz' if is_fizz && is_buzz

    number.to_s
  end
end

class FizzBuzzTypeNotDefined < FizzBuzzType
  def generate(_number)
    raise '該当するタイプは存在しません'
  end
end
