# FizzBuzz Go Implementation

テスト駆動開発から始めるRuby入門の内容に従って、Goで実装したFizzBuzzアプリケーションです。

## 概要

このプロジェクトは、テスト駆動開発（TDD）の学習を目的として作成されました。
FizzBuzzという古典的なプログラミング問題を通して、以下の技術を学習・実践しています：

- テスト駆動開発（TDD）
- 静的コード解析
- コードカバレッジ
- 自動化（タスクランナー）

## FizzBuzzルール

1から100までの数をプリントするプログラムで、以下のルールに従います：

- 3の倍数のときは数の代わりに「Fizz」と出力
- 5の倍数のときは「Buzz」と出力  
- 3と5両方の倍数の場合には「FizzBuzz」と出力
- それ以外の場合は数字をそのまま出力

## 必要要件

- Go 1.21以上
- go-task（タスクランナー）

## セットアップ

### go-taskのインストール

```bash
go install github.com/go-task/task/v3/cmd/task@latest
```

または、エイリアスを設定：

```bash
alias task="~/go/bin/task"
```

### 静的解析ツールのインストール

```bash
task lint-install
```

## 使用方法

### 利用可能なタスクの確認

```bash
task --list
```

### アプリケーションの実行

```bash
task run
```

### テストの実行

```bash
task test
```

### コードカバレッジの確認

```bash
task test-coverage
```

カバレッジレポートは `coverage.html` ファイルとして生成されます。

### 静的コード解析

```bash
task lint
```

### コードフォーマット

```bash
task fmt
```

### 全チェックの実行

```bash
task check
```

### 継続的インテグレーション

```bash
task ci
```

### ビルド

```bash
task build
```

### クリーンアップ

```bash
task clean
```

## プロジェクト構造

```
.
├── Taskfile.yml          # タスクランナー設定
├── go.mod                # Goモジュール設定
├── main.go               # メインアプリケーション
├── main_test.go          # テストファイル
├── bin/                  # ビルド成果物
├── coverage.out          # カバレッジデータ
└── coverage.html         # カバレッジレポート
```

## 開発プロセス

このプロジェクトは以下のTDDサイクルに従って開発されました：

1. **Red**: 失敗するテストを書く
2. **Green**: テストを通す最小限のコードを書く
3. **Refactor**: コードをリファクタリングする

### コミットメッセージ規約

[Angularコミットメッセージ規約](https://github.com/angular/angular.js/blob/master/DEVELOPERS.md#type)に従っています：

- `feat`: 新機能
- `fix`: バグ修正
- `docs`: ドキュメント変更のみ
- `style`: コードに影響を与えない変更
- `refactor`: 機能追加でもバグ修正でもないコード変更
- `test`: テストの追加や修正
- `chore`: ビルドプロセスや補助ツールの変更

## テスト

テストはGoの標準テストライブラリを使用して実装されています。

### テストケース

- 1を渡したら文字列"1"を返す
- 2を渡したら文字列"2"を返す
- 3を渡したら文字列"Fizz"を返す
- 5を渡したら文字列"Buzz"を返す
- 15を渡したら文字列"FizzBuzz"を返す
- 1から100までのFizzBuzz配列を返す

## 品質管理

### 静的コード解析

`golangci-lint`を使用してコード品質を維持しています。

### コードカバレッジ

現在のコードカバレッジ: **80%**

### フォーマット

`go fmt`を使用してコードフォーマットを統一しています。

## ライセンス

このプロジェクトは学習目的で作成されました。

## 参考資料

- [テスト駆動開発から始めるRuby入門](https://qiita.com/k2works/items/83741e3e2d2579d748d6)
- [ソフトウェア開発の三種の神器](https://t-wada.hatenablog.jp/entry/clean-code-that-works)
