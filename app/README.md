# FizzBuzz Prolog アプリケーション

テスト駆動開発から始めるProlog入門2で作成したFizzBuzzアプリケーションです。

## 概要

このアプリケーションは、1から指定した数までの数字を出力し、以下のルールに従って文字列を置き換えます：

- 3の倍数の場合は「Fizz」
- 5の倍数の場合は「Buzz」
- 3と5の両方の倍数（15の倍数）の場合は「FizzBuzz」

## ファイル構成

- `fizzbuzz.pl` - FizzBuzzのコアロジック（プロダクトコード）
- `test_fizzbuzz.pl` - テストスイート
- `app.pl` - メインアプリケーション
- `main.pl` - 旧バージョン（入門1で作成）
- `run_tests.sh` - テスト実行スクリプト
- `run_fizzbuzz.sh` - FizzBuzz実行スクリプト

## 使用方法

### テストの実行

```bash
./run_tests.sh
```

または

```bash
swipl -g "consult('test_fizzbuzz.pl'), run_tests, halt."
```

### FizzBuzzの実行

#### デフォルト実行（1-100、タイプ1）

```bash
./run_fizzbuzz.sh
```

#### パラメータ指定実行

```bash
# 1から20まで
./run_fizzbuzz.sh 1 20

# 1から30まで、タイプ2（数字のみ）
./run_fizzbuzz.sh 1 30 2

# 1から30まで、タイプ3（FizzBuzzのみ）
./run_fizzbuzz.sh 1 30 3
```

#### 直接Prologで実行

```bash
# デフォルト
swipl -g "consult('app.pl'), main, halt."

# パラメータ指定
swipl -g "consult('app.pl'), main(['1', '20']), halt."
swipl -g "consult('app.pl'), main(['1', '30', '2']), halt."
```

## 出力タイプ

1. **タイプ1（デフォルト）**: 通常のFizzBuzz出力
2. **タイプ2**: 数字のみを出力
3. **タイプ3**: FizzBuzzのみを出力

## 開発について

このアプリケーションはテスト駆動開発（TDD）の手法を使って開発されました。

### テスト駆動開発のサイクル

1. **Red**: 失敗するテストを書く
2. **Green**: テストを通す最小限のコードを書く
3. **Refactor**: コードを改善する

### モジュール構成

- **fizzbuzz モジュール**: コアロジックを提供
- **test_fizzbuzz**: 包括的なテストスイート
- **app**: メインアプリケーションとコマンドライン処理

## 依存関係

- SWI-Prolog 8.0以上
- plunit（SWI-Prologに含まれるテストフレームワーク）

## ライセンス

このプロジェクトはMITライセンスの下で提供されています。

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
