package main

import (
	"fmt"
	"log"
)

func main() {
	fmt.Println("Go TDD Tutorial - Calculator Application")
	fmt.Println("==========================================")

	calculator := NewCalculator()

	// 基本的な計算例を実行
	fmt.Printf("2 + 3 = %d\n", calculator.Add(2, 3))
	fmt.Printf("10 - 4 = %d\n", calculator.Subtract(10, 4))
	fmt.Printf("5 * 6 = %d\n", calculator.Multiply(5, 6))

	// 除算の例（エラーハンドリング付き）
	if result, err := calculator.Divide(15, 3); err != nil {
		log.Printf("エラー: %v", err)
	} else {
		fmt.Printf("15 / 3 = %.2f\n", result)
	}

	// ゼロ除算の例
	if result, err := calculator.Divide(10, 0); err != nil {
		fmt.Printf("エラーが発生しました: %v\n", err)
	} else {
		fmt.Printf("10 / 0 = %.2f\n", result)
	}

	fmt.Println("==========================================")
	fmt.Println("アプリケーションを終了します")
}
