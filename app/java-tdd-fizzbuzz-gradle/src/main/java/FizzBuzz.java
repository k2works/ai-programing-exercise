import java.util.stream.IntStream;

/**
 * FizzBuzz クラス
 * 
 * 1から指定された数までの数字に対して、以下のルールを適用します：
 * - 3で割り切れる数字は "Fizz"
 * - 5で割り切れる数字は "Buzz"
 * - 15で割り切れる数字は "FizzBuzz"
 * - それ以外の数字はそのまま文字列として返す
 */
public class FizzBuzz {
    
    /**
     * 指定された数字をFizzBuzzルールに従って変換します
     * 
     * @param number 変換対象の数字（正の整数）
     * @return FizzBuzzルールに従った文字列
     * @throws IllegalArgumentException 数字が0以下の場合
     */
    public String convert(int number) {
        if (number <= 0) {
            throw new IllegalArgumentException("Input must be positive number");
        }
        
        if (number % 15 == 0) {
            return "FizzBuzz";
        }
        
        if (number % 3 == 0) {
            return "Fizz";
        }
        
        if (number % 5 == 0) {
            return "Buzz";
        }
        
        return String.valueOf(number);
    }
    
    /**
     * 1から指定された数までのFizzBuzzを生成します
     * 
     * @param max 最大値（正の整数）
     * @return FizzBuzzの結果の配列
     * @throws IllegalArgumentException max が0以下の場合
     */
    public String[] generate(int max) {
        if (max <= 0) {
            throw new IllegalArgumentException("Max must be positive number");
        }
        
        return IntStream.rangeClosed(1, max)
                .mapToObj(this::convert)
                .toArray(String[]::new);
    }
    
    /**
     * 1から指定された数までのFizzBuzzを表示します
     * 
     * @param max 最大値（正の整数）
     */
    public void printFizzBuzz(int max) {
        if (max <= 0) {
            throw new IllegalArgumentException("Max must be positive number");
        }
        
        IntStream.rangeClosed(1, max)
                .mapToObj(this::convert)
                .forEach(System.out::println);
    }
    
    /**
     * メインメソッド - FizzBuzzを実行します
     * 
     * @param args コマンドライン引数（最大値を指定、デフォルトは100）
     */
    public static void main(String[] args) {
        FizzBuzz fizzBuzz = new FizzBuzz();
        
        int max = 100; // デフォルト値
        
        if (args.length > 0) {
            try {
                max = Integer.parseInt(args[0]);
            } catch (NumberFormatException e) {
                System.err.println("Invalid number format: " + args[0]);
                System.err.println("Using default value: " + max);
            }
        }
        
        try {
            System.out.println("FizzBuzz (1 to " + max + "):");
            System.out.println("============================");
            fizzBuzz.printFizzBuzz(max);
        } catch (IllegalArgumentException e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
        }
    }
}
