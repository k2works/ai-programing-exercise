/// FizzBuzz実装
/// 3の倍数の場合は"Fizz"、5の倍数の場合は"Buzz"、
/// 15の倍数の場合は"FizzBuzz"を返す
pub struct FizzBuzz {
    max_number: u32,
}

impl FizzBuzz {
    const MAX_NUMBER: u32 = 100;

    pub fn new() -> Self {
        Self {
            max_number: Self::MAX_NUMBER,
        }
    }

    pub fn generate(number: u32) -> String {
        let is_fizz = number % 3 == 0;
        let is_buzz = number % 5 == 0;

        match (is_fizz, is_buzz) {
            (true, true) => "FizzBuzz".to_string(),
            (true, false) => "Fizz".to_string(),
            (false, true) => "Buzz".to_string(),
            (false, false) => number.to_string(),
        }
    }

    pub fn generate_list(&self) -> Vec<String> {
        (1..=self.max_number)
            .map(|n| Self::generate(n))
            .collect()
    }
}

impl Default for FizzBuzz {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fizz_for_3() {
        assert_eq!(FizzBuzz::generate(3), "Fizz");
    }

    #[test]
    fn test_buzz_for_5() {
        assert_eq!(FizzBuzz::generate(5), "Buzz");
    }

    #[test]
    fn test_fizzbuzz_for_15() {
        assert_eq!(FizzBuzz::generate(15), "FizzBuzz");
    }

    #[test]
    fn test_one_for_1() {
        assert_eq!(FizzBuzz::generate(1), "1");
    }

    #[test]
    fn test_two_for_2() {
        assert_eq!(FizzBuzz::generate(2), "2");
    }

    #[test]
    fn test_four_for_4() {
        assert_eq!(FizzBuzz::generate(4), "4");
    }

    #[test]
    fn test_fizz_for_6() {
        assert_eq!(FizzBuzz::generate(6), "Fizz");
    }

    #[test]
    fn test_buzz_for_10() {
        assert_eq!(FizzBuzz::generate(10), "Buzz");
    }

    #[test]
    fn test_fizzbuzz_for_30() {
        assert_eq!(FizzBuzz::generate(30), "FizzBuzz");
    }

    #[test]
    fn test_array_100th_is_buzz() {
        let fizzbuzz = FizzBuzz::new();
        let result = fizzbuzz.generate_list();
        assert_eq!(result[99], "Buzz"); // 0-indexed
    }

    #[test]
    fn test_array_first_is_1() {
        let fizzbuzz = FizzBuzz::new();
        let result = fizzbuzz.generate_list();
        assert_eq!(result[0], "1");
    }

    #[test]
    fn test_array_last_is_buzz() {
        let fizzbuzz = FizzBuzz::new();
        let result = fizzbuzz.generate_list();
        assert_eq!(result[result.len() - 1], "Buzz");
    }
}

// 配列や繰り返し処理を理解するためのテスト
#[cfg(test)]
mod array_tests {
    #[test]
    fn test_iteration() {
        // 繰り返し処理：各要素を2乗する
        let numbers = vec![1, 2, 3];
        let mut output = Vec::new();
        
        for i in numbers {
            output.push(i * i);
        }
        
        assert_eq!(output, vec![1, 4, 9]);
    }

    #[test]
    fn test_filter_select_elements() {
        // 特定の条件を満たす要素だけを配列に入れて返す（select相当）
        let numbers = vec![1, 2, 3, 4, 5, 6];
        let result: Vec<i32> = numbers
            .into_iter()
            .filter(|&x| x % 2 == 0) // 偶数のみ
            .collect();
        
        assert_eq!(result, vec![2, 4, 6]);
    }

    #[test]
    fn test_filter_reject_elements() {
        // 特定の条件を満たさない要素だけを配列に入れて返す（reject相当）
        let numbers = vec![1, 2, 3, 4, 5, 6];
        let result: Vec<i32> = numbers
            .into_iter()
            .filter(|&x| x % 2 != 0) // 奇数のみ
            .collect();
        
        assert_eq!(result, vec![1, 3, 5]);
    }

    #[test]
    fn test_map_transform_elements() {
        // 新しい要素の配列を返す（map相当）
        let words = vec!["apple", "orange", "pineapple", "strawberry"];
        let result: Vec<usize> = words
            .iter()
            .map(|word| word.len())
            .collect();
        
        assert_eq!(result, vec![5, 6, 9, 10]);
    }

    #[test]
    fn test_find_first_element() {
        // 配列の中から条件に一致する要素を取得する（find相当）
        let words = vec!["apple", "orange", "pineapple", "strawberry"];
        let result = words.iter().find(|&&word| word.len() > 7);
        
        assert_eq!(result, Some(&"pineapple"));
    }

    #[test]
    fn test_sort_with_custom_comparator() {
        // 指定した評価式で並び変えた配列を返す
        let numbers = vec!["2", "4", "13", "3", "1", "10"];
        
        // 文字列として並び替え
        let mut result1 = numbers.clone();
        result1.sort();
        assert_eq!(result1, vec!["1", "10", "13", "2", "3", "4"]);
        
        // 数値として並び替え（昇順）
        let mut result2 = numbers.clone();
        result2.sort_by(|a, b| a.parse::<i32>().unwrap().cmp(&b.parse::<i32>().unwrap()));
        assert_eq!(result2, vec!["1", "2", "3", "4", "10", "13"]);
        
        // 数値として並び替え（降順）
        let mut result3 = numbers.clone();
        result3.sort_by(|a, b| b.parse::<i32>().unwrap().cmp(&a.parse::<i32>().unwrap()));
        assert_eq!(result3, vec!["13", "10", "4", "3", "2", "1"]);
    }

    #[test]
    fn test_grep_pattern_matching() {
        // 配列の中から条件に一致する要素を取得する（grep相当）
        let words = vec!["apple", "orange", "pineapple", "strawberry", "apricot"];
        let result: Vec<&str> = words
            .iter()
            .filter(|word| word.starts_with('a'))
            .copied()
            .collect();
        
        assert_eq!(result, vec!["apple", "apricot"]);
    }

    #[test]
    fn test_take_while_condition() {
        // ブロック内の条件式が真である間までの要素を返す（take_while相当）
        let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
        let result: Vec<i32> = numbers
            .iter()
            .take_while(|&&item| item < 6)
            .copied()
            .collect();
        
        assert_eq!(result, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_skip_while_condition() {
        // ブロック内の条件式が真である以降の要素を返す（drop_while相当）
        let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let result: Vec<i32> = numbers
            .iter()
            .skip_while(|&&item| item < 6)
            .copied()
            .collect();
        
        assert_eq!(result, vec![6, 7, 8, 9, 10]);
    }

    #[test]
    fn test_fold_accumulation() {
        // 畳み込み演算を行う（inject相当）
        let numbers = vec![1, 2, 3, 4, 5];
        let result = numbers.iter().fold(0, |total, n| total + n);
        assert_eq!(result, 15);
    }

    #[test]
    fn test_reduce_accumulation() {
        // 畳み込み演算を行う（reduce相当）
        let numbers = vec![1, 2, 3, 4, 5];
        let result = numbers.iter().copied().reduce(|total, n| total + n);
        assert_eq!(result, Some(15));
    }

    #[test]
    fn test_collect_with_transformation() {
        // collectを使った変換処理
        let numbers = vec![1, 2, 3, 4, 5];
        let result: Vec<String> = numbers
            .iter()
            .map(|n| format!("Number: {}", n))
            .collect();
        
        assert_eq!(result, vec![
            "Number: 1", "Number: 2", "Number: 3", "Number: 4", "Number: 5"
        ]);
    }

    #[test]
    fn test_enumerate_with_index() {
        // インデックス付きの繰り返し処理
        let words = vec!["apple", "orange", "pineapple"];
        let result: Vec<String> = words
            .iter()
            .enumerate()
            .map(|(i, word)| format!("{}: {}", i, word))
            .collect();
        
        assert_eq!(result, vec![
            "0: apple", "1: orange", "2: pineapple"
        ]);
    }

    #[test]
    fn test_chain_operations() {
        // 複数の操作を連結
        let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let result: Vec<i32> = numbers
            .iter()
            .filter(|&&x| x % 2 == 0)  // 偶数のみ
            .map(|&x| x * 2)           // 2倍（参照を外してから計算）
            .take(3)                   // 最初の3つ
            .collect();
        
        assert_eq!(result, vec![4, 8, 12]);
    }

    #[test]
    fn test_any_all_predicates() {
        // any（いずれかの要素が条件を満たすか）
        let numbers = vec![1, 2, 3, 4, 5];
        assert!(numbers.iter().any(|&x| x > 3));
        assert!(!numbers.iter().any(|&x| x > 10));
        
        // all（すべての要素が条件を満たすか）
        assert!(numbers.iter().all(|&x| x > 0));
        assert!(!numbers.iter().all(|&x| x > 3));
    }
}
