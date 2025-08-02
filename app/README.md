# FizzBuzz - Prolog版

## 概要

テスト駆動開発（TDD）の手法を用いてPrologでFizzBuzzを実装しました。

## 仕様

1から100までの数をプリントするプログラムを作成。
- 3の倍数のときは数の代わりに「Fizz」をプリント
- 5の倍数のときは「Buzz」をプリント  
- 3と5両方の倍数の場合には「FizzBuzz」をプリント

## ファイル構成

- `main.pl` - FizzBuzzの実装とテスト
- `run_tests.sh` - テスト実行スクリプト
- `run_fizzbuzz.sh` - FizzBuzz実行スクリプト

## 実行方法

### テストの実行
```bash
./run_tests.sh
```

### FizzBuzzの実行（1-100）
```bash
./run_fizzbuzz.sh
```

### 対話モードでの実行
```bash
swipl
?- consult('main.pl').
?- fizzbuzz_print(1, 100).
```

## プレディケート

- `fizzbuzz_generate(Number, Result)` - 単一の数値に対してFizzBuzz変換
- `fizzbuzz_list(Start, End, Result)` - 範囲指定でFizzBuzzリスト生成
- `fizzbuzz_print(Start, End)` - 範囲指定でFizzBuzzをプリント

## テスト駆動開発の実践

1. 数を文字列にして返す（仮実装→三角測量）
2. 3の倍数のときは「Fizz」を返す
3. 5の倍数のときは「Buzz」を返す
4. 3と5両方の倍数の場合には「FizzBuzz」を返す
5. 1から100までの数をプリントする

各ステップでテストファーストの手法を適用し、小さなステップで実装を進めました。
