# FizzBuzz Haskell

テスト駆動開発（TDD）で実装したHaskellによるFizzBuzzプログラムです。

## 概要

このプロジェクトは「テスト駆動開発から始めるRuby入門」の手順に従って、HaskellでFizzBuzzを実装したものです。

### FizzBuzzルール

1. 1から100までの数をプリントする
2. 3の倍数のときは数の代わりに「Fizz」とプリントする
3. 5の倍数のときは「Buzz」とプリントする  
4. 3と5両方の倍数の場合には「FizzBuzz」とプリントする

## 構成

```
fizzbuzz-haskell/
├── app/
│   └── Main.hs          # メインアプリケーション
├── src/
│   └── FizzBuzz.hs      # FizzBuzzロジック
├── test/
│   ├── Spec.hs          # テストエントリーポイント
│   └── FizzBuzzSpec.hs  # FizzBuzzのテストスイート
└── fizzbuzz-haskell.cabal
```

## ビルドと実行

### 必要な環境

- GHC (Glasgow Haskell Compiler)
- Cabal

### テストの実行

```bash
cabal test
```

### アプリケーションの実行

```bash
cabal build
cabal exec fizzbuzz-haskell
```

## TDDによる開発フロー

このプロジェクトは以下のTDDサイクルで開発されました：

1. **レッド（Red）**: 失敗するテストを書く
2. **グリーン（Green）**: 最小限のコードでテストを通す
3. **リファクタリング**: コードを整理・改善する

### 実装手順

1. 数を文字列にして返す（仮実装→三角測量）
2. 3の倍数のときは「Fizz」を返す
3. 5の倍数のときは「Buzz」を返す  
4. 3と5両方の倍数のときは「FizzBuzz」を返す
5. 1から100までをプリントする機能

## テスト

包括的なテストスイートにより、以下がテストされています：

- 数値を文字列に変換
- 3の倍数でFizzを返すこと
- 5の倍数でBuzzを返すこと
- 15の倍数（3と5の倍数）でFizzBuzzを返すこと
- 複数の倍数パターンの検証

## 学習のポイント

- テストファーストによる開発
- 仮実装からの段階的改善
- 三角測量による一般化
- 関数型プログラミングにおけるTDD

### 関数型プログラミングのアプローチ

このFizzBuzz実装では、以下の関数型プログラミングの概念を活用しています：

1. **純粋関数**: 副作用のない関数（`isDivisibleBy`など）
2. **高階関数**: 関数を引数として受け取る関数（`foldMap`）
3. **Monoid**: `First`型を使用した最初の成功値の選択
4. **関数合成**: 小さな関数を組み合わせて複雑な処理を構築
5. **Maybe型**: 明示的な失敗処理とNull安全性
6. **宣言的プログラミング**: 「何をするか」を記述（「どうやるか」ではなく）

### リファクタリングの過程

初期実装（手続き型）:
```haskell
generate n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n
```

最終実装（関数型）:
```haskell
generate n = fromMaybe (show n) $ getFirst $ foldMap (First . ($ n)) rules
```

この実装により：
- ルールの追加が容易（`rules`リストに関数を追加するだけ）
- 各ルールが独立してテスト可能
- 関数の組み合わせによる柔軟性の向上
