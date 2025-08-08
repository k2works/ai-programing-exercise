# アルゴリズムから始めるHaskell入門

Haskellを使ったアルゴリズム学習のためのプロジェクトです。テスト駆動開発（TDD）を実践しながら、Haskellの基本的な文法とアルゴリズムを学習します。

## 特徴

- **テスト駆動開発**: hspecを使用したテストファーストのアプローチ
- **静的コード解析**: HLintによるコード品質チェック
- **自動フォーマット**: Ormoluによるコード整形
- **継続的インテグレーション**: GitHub Actionsによる自動テスト

## セットアップ

### 必要なツール

- [Stack](https://docs.haskellstack.org/en/stable/README/)

### インストール

```bash
# 依存関係のインストール
make setup

# プロジェクトのビルド
make build
```

## 使用方法

### テストの実行

```bash
make test
```

### 静的コード解析

```bash
make lint
```

### コードフォーマット

```bash
make format
```

### カバレッジ付きテスト

```bash
make coverage
```

### 循環複雑度解析

```bash
# 基本的な複雑度解析
make complexity

# 詳細レポート（リスク分析・推奨事項付き）
make complexity-report
```

### アプリケーションの実行

```bash
stack exec algorithm-haskell-exe
```

## プロジェクト構成

```
├── app/                # 実行可能ファイル
├── src/                # ライブラリソースコード
├── test/               # テストファイル
├── package.yaml        # パッケージ設定
├── stack.yaml          # Stack設定
├── Makefile            # タスクランナー
└── .github/workflows/  # CI/CD設定
```

## 学習トピック

- [ ] FizzBuzz実装（TDD実践）
- [ ] ソート アルゴリズム
- [ ] 探索アルゴリズム
- [ ] データ構造（リスト、ツリー、グラフ）
- [ ] 関数型プログラミングパターン

## ライセンス

BSD3
