package main

import (
    "fmt"
    "strconv"
)

func Generate(number int) string {
    if number%3 == 0 && number%5 == 0 {
        return "FizzBuzz"
    }
    if number%3 == 0 {
        return "Fizz"
    }
    if number%5 == 0 {
        return "Buzz"
    }
    return strconv.Itoa(number)
}

func GenerateList(start, end int) []string {
    results := make([]string, 0, end-start+1)
    for i := start; i <= end; i++ {
        results = append(results, Generate(i))
    }
    return results
}

func GenerateByType(number, fizzBuzzType int) string {
    switch fizzBuzzType {
    case 1:
        if number%3 == 0 && number%5 == 0 {
            return "FizzBuzz"
        }
        if number%3 == 0 {
            return "Fizz"
        }
        if number%5 == 0 {
            return "Buzz"
        }
        return strconv.Itoa(number)
    case 2:
        return strconv.Itoa(number)
    case 3:
        if number%3 == 0 && number%5 == 0 {
            return "FizzBuzz"
        }
        return strconv.Itoa(number)
    default:
        panic("未定義のタイプです")
    }
}

func main() {
    fmt.Println("FizzBuzz Game:")
    results := GenerateList(1, 100)
    for _, result := range results {
        fmt.Println(result)
    }
}
