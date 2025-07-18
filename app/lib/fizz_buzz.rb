class FizzBuzz
  MAX_NUMBER = 100

  def self.generate(number)
    if number.modulo(3).zero? && number.modulo(5).zero?
      'FizzBuzz'
    elsif number.modulo(3).zero?
      'Fizz'
    elsif number.modulo(5).zero?
      'Buzz'
    else
      number.to_s
    end
  end

  def self.generate_list
    (1..MAX_NUMBER).map { |n| generate(n) }
  end
end
