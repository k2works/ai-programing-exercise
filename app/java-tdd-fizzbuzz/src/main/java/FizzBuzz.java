import java.util.ArrayList;
import java.util.List;

public class FizzBuzz {
    
    public String generate(int number) {
        if (number % 3 == 0 && number % 5 == 0) {
            return "FizzBuzz";
        } else if (number % 3 == 0) {
            return "Fizz";
        } else if (number % 5 == 0) {
            return "Buzz";
        }
        return String.valueOf(number);
    }
    
    public List<String> generateList(int count) {
        List<String> result = new ArrayList<>();
        for (int i = 1; i <= count; i++) {
            result.add(generate(i));
        }
        return result;
    }
    
    public void printFizzBuzz(int count) {
        List<String> result = generateList(count);
        for (String s : result) {
            System.out.println(s);
        }
    }
}
