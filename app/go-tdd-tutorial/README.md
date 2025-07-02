# Go TDD Tutorial - ソフトウェア開発の三種の神器

このプロジェクトは、Goでのテスト駆動開発（TDD）において **ソフトウェア開発の三種の神器** を実践するためのサンプルアプリケーションです。

## ソフトウェア開発の三種の神器

1. **バージョン管理（Git）**
2. **テスティング（Go testing + testify）**
3. **自動化（Makefile + 各種ツール）**

## プロジェクト構成

```
go-tdd-tutorial/
├── main.go              # メインアプリケーション
├── calculator.go        # Calculator構造体の実装
├── calculator_test.go   # テストファイル
├── go.mod              # Go module定義
├── go.sum              # 依存関係のハッシュ
├── Makefile            # タスクランナー（自動化）
├── .golangci.yml       # 静的解析設定
└── README.md           # このファイル
```

## セットアップ

### 前提条件

- Go 1.21以上
- Make（タスクランナー用）
- fswatch（ファイル監視用、オプション）
- golangci-lint（静的解析用、オプション）

### 依存関係のインストール

```bash
# 依存関係のダウンロード
make deps

# または
go mod download
go mod tidy
```

### オプションツールのインストール

```bash
# golangci-lint（静的解析）
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest

# fswatch（macOS）
brew install fswatch

# fswatch（Ubuntu/Debian）
sudo apt-get install fswatch
```

## 使用方法

### 基本コマンド

```bash
# アプリケーションの実行
make run

# テストの実行
make test

# コードフォーマット
make fmt

# 静的解析
make lint

# ビルド
make build

# 品質チェック（フォーマット + 静的解析 + テスト）
make check
```

### 自動化コマンド

```bash
# テストのwatch実行（ファイル変更時に自動実行）
make test-watch

# 品質チェックの自動実行（ファイル変更時に自動実行）
make watch

# テストカバレッジ
make test-coverage
```

### その他

```bash
# クリーンアップ
make clean

# ヘルプ
make help
```

## TDDサイクル

このプロジェクトでは、以下のTDDサイクルを実践します：

1. **Red**: 失敗するテストを書く
2. **Green**: テストをパスする最小限のコードを書く
3. **Refactor**: コードをリファクタリングする

## Calculator機能

基本的な四則演算を提供するCalculator構造体：

- `Add(a, b int) int` - 加算
- `Subtract(a, b int) int` - 減算
- `Multiply(a, b int) int` - 乗算
- `Divide(a, b int) (float64, error)` - 除算（ゼロ除算エラーハンドリング付き）

## テスト実行例

```bash
$ make test
テストを実行中...
go test -v ./...
=== RUN   TestCalculator_Add
=== RUN   TestCalculator_Add/正の数の加算
=== RUN   TestCalculator_Add/負の数の加算
=== RUN   TestCalculator_Add/ゼロの加算
=== RUN   TestCalculator_Add/大きな数の加算
--- PASS: TestCalculator_Add (0.00s)
    --- PASS: TestCalculator_Add/正の数の加算 (0.00s)
    --- PASS: TestCalculator_Add/負の数の加算 (0.00s)
    --- PASS: TestCalculator_Add/ゼロの加算 (0.00s)
    --- PASS: TestCalculator_Add/大きな数の加算 (0.00s)
=== RUN   TestCalculator_Subtract
...
PASS
ok      go-tdd-tutorial 0.002s
```

## 品質管理

### 静的解析

golangci-lintを使用してコードの品質をチェック：

```bash
make lint
```

### コードフォーマット

Go標準のフォーマッターを使用：

```bash
make fmt
```

### テストカバレッジ

```bash
make test-coverage
```

## 開発ワークフロー

1. 新機能やバグ修正のために、まずテストを書く
2. テストを実行して失敗することを確認
3. テストをパスする最小限のコードを実装
4. `make check` で品質チェックを実行
5. 必要に応じてリファクタリング
6. Gitでコミット

### 推奨開発フロー

開発中は以下のコマンドでファイル監視を開始：

```bash
make watch
```

これにより、ファイル変更時に自動でフォーマット、静的解析、テストが実行されます。

## 参考リンク

- [Go公式ドキュメント](https://golang.org/doc/)
- [Go Testing Package](https://golang.org/pkg/testing/)
- [testify - Testing toolkit](https://github.com/stretchr/testify)
- [golangci-lint](https://golangci-lint.run/)
- [ソフトウェア開発の三種の神器](https://t-wada.hatenablog.jp/entry/clean-code-that-works)
