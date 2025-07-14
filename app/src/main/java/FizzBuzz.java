public class FizzBuzz {
    public String generate(int number) {
        String result = String.valueOf(number);
        if (number % 3 == 0) {
            result = "Fizz";
        }
        return result;
    }
}
