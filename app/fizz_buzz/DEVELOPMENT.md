# 開発ガイド

このプロジェクトでは、Elixirにおけるソフトウェア開発の三種の神器を導入しています。

## 使用可能なツール

### 静的コード解析
```bash
mix credo                # コードの静的解析
mix credo --strict      # より厳格な解析
```

### コードフォーマッタ
```bash
mix format              # コードを自動整形
mix format --check-formatted  # フォーマット状態をチェック
```

### コードカバレッジ
```bash
mix test                # テスト実行
mix coveralls           # カバレッジ測定
mix coveralls.html      # HTMLレポート生成
```

### 統合タスク
```bash
mix check               # 基本チェック (format, credo, test)
mix quality             # 全品質チェック (format, credo, test, coverage)
```

### 開発用コマンド
```bash
mix dev                 # 利用可能なコマンド一覧
mix dev.test           # テスト実行
mix dev.format         # コードフォーマット
mix dev.coverage       # カバレッジレポート生成
```

### ファイル監視（要：inotify-tools）
```bash
mix test.watch         # ファイル変更時の自動テスト実行
```

## 開発フロー

1. **コードを書く**
2. **フォーマット**: `mix format`
3. **チェック**: `mix check`
4. **コミット**

## 品質基準

- **フォーマット**: Elixir標準フォーマッタに準拠
- **静的解析**: Credoのルールをクリア
- **テストカバレッジ**: 80%以上を目標
- **全テスト**: パスすること

## 設定ファイル

- `.formatter.exs`: フォーマッタ設定
- `.credo.exs`: Credo静的解析設定
- `mix.exs`: プロジェクト設定とカバレッジ設定