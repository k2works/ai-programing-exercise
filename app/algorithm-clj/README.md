# Algorithm Clojure

テスト駆動開発（TDD）アプローチを使用したClojureでのアルゴリズム実装プロジェクトです。

## プロジェクト概要

このプロジェクトは、Clojureでアルゴリズムを実装しながら、テスト駆動開発の実践と品質の高いコードを維持するためのツールチェーンを提供します。

## 開発ツール

### 静的コード解析
- **Eastwood**: Clojureコードのリンター（警告やエラーの検出）
- **Kibit**: Clojureのイディオムに従った書き方の提案

### コードフォーマッタ
- **cljfmt**: コードスタイルの統一

### コードカバレッジ
- **Cloverage**: テストカバレッジの測定とレポート生成

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
# コードフォーマット、静的解析を一括実行
lein check
```

#### 個別のツール実行

**コードフォーマット**
```bash
# フォーマットの確認（修正なし）
lein cljfmt check

# フォーマットの自動修正
lein cljfmt fix
# または
lein fix
```

**静的コード解析**
```bash
# Eastwood（リンター）の実行
lein eastwood

# Kibit（イディオム提案）の実行
lein kibit

# 両方を実行
lein lint
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

### カバレッジレポート

カバレッジレポートは `target/coverage/` ディレクトリに生成されます：
- `target/coverage/index.html`: HTMLレポート
- コンソールにもサマリーが表示されます
- カバレッジが80%未満の場合、失敗として扱われます

### 推奨開発フロー

1. **コード変更前**: `lein check` で現在の状態を確認
2. **コード実装**: TDDアプローチでテストファーストで開発
3. **コード変更後**: 
   ```bash
   lein fix          # フォーマット自動修正
   lein test-all     # テスト実行とカバレッジ測定
   lein lint         # 静的解析
   ```
4. **継続的な監視**: `lein autotest` でテストを自動実行

## ディレクトリ構造

```
algorithm-clj/
├── project.clj          # プロジェクト設定
├── README.md            # このファイル
├── src/                 # ソースコード
│   └── algorithm_clj/
│       ├── core.clj     # メインエントリーポイント
│       ├── algorithms/  # アルゴリズム実装
│       └── demos/       # デモとサンプル
├── test/                # テストコード
│   └── algorithm_clj/
├── target/              # ビルド成果物
│   └── coverage/        # カバレッジレポート
└── resources/           # リソースファイル
```

## 品質基準

- **テストカバレッジ**: 80%以上を維持
- **コードスタイル**: cljfmtの規則に準拠
- **静的解析**: EastwoodとKibitの警告をゼロに維持

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
