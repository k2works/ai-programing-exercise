package main

import (
	"strconv"
	"fizzbuzz/domain/model"
	"fizzbuzz/domain/type"
)

// FizzBuzz構造体
type FizzBuzz struct {
	list         *model.FizzBuzzList // FizzBuzzListを保持するフィールド
	fizzBuzzType int           // FizzBuzzのタイプを保持するフィールド
	typeImpl     fizzbuzztype.FizzBuzzType  // タイプ実装
}

// NewFizzBuzz コンストラクタ（プリミティブ型を受け取る）
func NewFizzBuzz(fizzBuzzType int) *FizzBuzz {
	base := fizzbuzztype.FizzBuzzTypeBase{}
	typeImpl := base.Create(fizzBuzzType)

	// 未定義タイプの場合は-1を設定
	actualType := fizzBuzzType
	if _, ok := typeImpl.(fizzbuzztype.FizzBuzzTypeNotDefined); ok {
		actualType = -1
	}

	return &FizzBuzz{
		list:         model.NewFizzBuzzList([]model.FizzBuzzValue{}),
		fizzBuzzType: actualType,
		typeImpl:     typeImpl,
	}
}

// NewFizzBuzzWithType コンストラクタ（値オブジェクトを受け取る）
func NewFizzBuzzWithType(typeImpl fizzbuzztype.FizzBuzzType) *FizzBuzz {
	// タイプを逆算（実際の実装では避けるべきですが、ここでは後方互換性のため）
	var fizzBuzzType int
	switch typeImpl.(type) {
	case fizzbuzztype.FizzBuzzType01:
		fizzBuzzType = fizzbuzztype.TYPE_01
	case fizzbuzztype.FizzBuzzType02:
		fizzBuzzType = fizzbuzztype.TYPE_02
	case fizzbuzztype.FizzBuzzType03:
		fizzBuzzType = fizzbuzztype.TYPE_03
	case fizzbuzztype.FizzBuzzTypeNotDefined:
		fizzBuzzType = -1 // 未定義タイプを示すデフォルト値
	default:
		fizzBuzzType = -1 // 未定義タイプを示すデフォルト値
	}

	return &FizzBuzz{
		list:         model.NewFizzBuzzList([]model.FizzBuzzValue{}),
		fizzBuzzType: fizzBuzzType,
		typeImpl:     typeImpl,
	}
}

// Generate 数を文字列に変換してFizzBuzzルールを適用
func (f *FizzBuzz) Generate(number int) model.FizzBuzzValue {
	return f.typeImpl.Generate(number)
}

// Type FizzBuzzタイプを取得
func (f *FizzBuzz) Type() int {
	return f.fizzBuzzType
}

// List FizzBuzz配列を取得
func (f *FizzBuzz) List() []model.FizzBuzzValue {
	return f.list.Value()
}

// GenerateList 範囲指定してFizzBuzzのリストを作成し、インスタンス変数に保存
func (f *FizzBuzz) GenerateList(start, end int) {
	values := make([]model.FizzBuzzValue, 0, end-start+1)
	for i := start; i <= end; i++ {
		values = append(values, f.Generate(i))
	}
	f.list = f.list.Add(values)
}

// BasicFizzBuzzGenerate 基本的なFizzBuzz生成関数（ドキュメントの初期バージョン）
func BasicFizzBuzzGenerate(number, fizzBuzzType int) string {
	isFizz := number%3 == 0
	isBuzz := number%5 == 0

	switch fizzBuzzType {
	case 1:
		if isFizz && isBuzz {
			return "FizzBuzz"
		}
		if isFizz {
			return "Fizz"
		}
		if isBuzz {
			return "Buzz"
		}
		return strconv.Itoa(number)
	case 2:
		return strconv.Itoa(number)
	case 3:
		if isFizz && isBuzz {
			return "FizzBuzz"
		}
		return strconv.Itoa(number)
	default:
		panic("該当するタイプは存在しません")
	}
}


