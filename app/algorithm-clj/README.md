# Algorithm Clojure

テスト駆動開発（TDD）アプローチを使用したClojureでのアルゴリズム実装プロジェクトです。

## プロジェクト概要

このプロジェクトは、Clojureでアルゴリズムを実装しながら、テスト駆動開発の実践と品質の高いコードを維持するためのツールチェーンを提供します。

## 開発ツール

### 静的コード解析
- **Eastwood**: Clojureコードのリンター（警告やエラーの検出）
- **Kibit**: Clojureのイディオムに従った書き方の提案

### コードフォーマッタ
- **cljfmt**: コードスタイルの統一（現在は一時的に除外中）

### コードカバレッジ
- **Cloverage**: テストカバレッジの測定とレポート生成

### コード品質メトリクス（新規追加）
- **Bikeshed**: 循環複雑度、行長、コードスタイルの測定
- **Yagni**: 未使用関数の検出
- **NVD**: 依存関係の脆弱性検査

### テストツール
- **test.check**: プロパティベーステスト
- **test-refresh**: テストの自動実行

## セットアップ

### 前提条件
- Java 8以上
- Leiningen

### 依存関係のインストール
```bash
lein deps
```

## 使用方法

### 基本的なコマンド

```bash
# テスト実行
lein test

# アプリケーション実行
lein run
```

### 品質チェックコマンド

#### 全体的な品質チェック
```bash
# 静的解析の一括実行
lein check

# 総合品質チェック（静的解析+メトリクス+テスト）
lein quality

# 完全品質チェック（クリーン+全チェック）
lein full-check
```

#### 個別のツール実行

**静的コード解析**
```bash
# Eastwood（リンター）の実行
lein eastwood

# Kibit（イディオム提案）の実行
lein kibit

# 両方を実行
lein lint
```

**コード品質メトリクス（新規追加）**
```bash
# 循環複雑度とコード品質メトリクスの測定
lein bikeshed
# または
lein complexity

# 未使用関数の検出
lein yagni

# 両方を実行
lein metrics
```

**セキュリティチェック（新規追加）**
```bash
# 依存関係の脆弱性検査
lein nvd check
# または
lein security
```

**コードカバレッジ**
```bash
# カバレッジ測定とHTMLレポート生成
lein cloverage
# または
lein coverage

# テスト実行とカバレッジ測定を同時に
lein test-all
```

**テスト自動実行**
```bash
# ファイル変更を監視してテストを自動実行
lein test-refresh
# または
lein autotest
```

**依存関係の更新チェック**
```bash
# 古い依存関係をチェック
lein ancient
# または
lein outdated
```

### Makeタスク（推奨）

#### 品質チェック系
```bash
make check          # 静的コード解析
make complexity     # 循環複雑度測定
make security       # セキュリティチェック
make quality        # 総合品質チェック
make full-check     # 完全品質チェック
```

#### テスト系
```bash
make test           # テスト実行
make coverage       # カバレッジ測定
make test-all       # テスト+カバレッジ
make test-watch     # テスト自動実行
```

#### レポート生成
```bash
make reports        # 全レポート生成（カバレッジ+メトリクス+セキュリティ）
```

#### 開発フロー
```bash
make dev-flow       # 開発時の推奨フロー
make pre-commit     # コミット前のチェック
```

### 生成されるレポート

**カバレッジレポート**
- `target/coverage/index.html`: HTMLレポート

**品質メトリクスレポート（新規追加）**
- コンソール出力: 循環複雑度、長い行、未使用関数の一覧
- Bikeshedによる詳細なコード品質指標

**セキュリティレポート（新規追加）**
- `reports/nvd/`: 依存関係の脆弱性レポート

### 推奨開発フロー

1. **コード変更前**: `make check` で現在の状態を確認
2. **コード実装**: TDDアプローチでテストファーストで開発
3. **コード変更後**: 
   ```bash
   make quality      # 総合品質チェック
   make complexity   # 循環複雑度チェック
   make security     # セキュリティチェック
   ```
4. **継続的な監視**: `make test-watch` でテストを自動実行
5. **コミット前**: `make pre-commit` で最終チェック

## ディレクトリ構造

```
algorithm-clj/
├── project.clj          # プロジェクト設定
├── README.md            # このファイル
├── Makefile             # タスクランナー
├── src/                 # ソースコード
│   └── algorithm_clj/
│       ├── core.clj     # メインエントリーポイント
│       ├── algorithms/  # アルゴリズム実装
│       └── demos/       # デモとサンプル
├── test/                # テストコード
│   └── algorithm_clj/
├── target/              # ビルド成果物
│   └── coverage/        # カバレッジレポート
├── reports/             # 各種レポート（新規追加）
│   └── nvd/            # セキュリティレポート
└── resources/           # リソースファイル
```

## 品質基準

- **テストカバレッジ**: 80%以上を維持
- **循環複雑度**: 関数ごとに10以下を推奨（新規追加）
- **コードスタイル**: Bikeshedの規則に準拠（新規追加）
- **静的解析**: EastwoodとKibitの警告をゼロに維持
- **セキュリティ**: 依存関係の脆弱性をゼロに維持（新規追加）

## メトリクス指標（新規追加）

### 循環複雑度
- **測定対象**: 全関数の制御フローの複雑さ
- **推奨値**: 関数ごとに10以下
- **測定方法**: `make complexity`

### コード品質指標
- **行長**: 100文字以下を推奨
- **未使用関数**: 検出された場合は削除を検討
- **測定方法**: `make metrics`

### セキュリティ指標
- **依存関係の脆弱性**: CVEデータベースとの照合
- **測定方法**: `make security`

## トラブルシューティング

### よくある問題

**依存関係の問題**
```bash
lein clean
lein deps
```

**テスト失敗時の詳細確認**
```bash
lein test :verbose
```

**プラグインのバージョン確認**
```bash
lein ancient :plugins
```

**レポートが生成されない場合**
```bash
make clean
make deps
make reports
```
