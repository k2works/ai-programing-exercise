require_relative 'fizz_buzz/type'

class FizzBuzz
  MAX_NUMBER = 100
  attr_reader :list, :type

  def initialize(type)
    @type = type
  end

  def generate(number)
    @type.generate(number)
  end

  def generate_list
    @list = (1..MAX_NUMBER).map { |n| generate(n) }
  end
end
