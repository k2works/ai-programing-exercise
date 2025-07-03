# TypeScript TDD Application

テスト駆動開発から始めるTypeScript入門のサンプルアプリケーション

## ソフトウェア開発の三種の神器

このプロジェクトでは以下の三種の神器を実装しています：

1. **バージョン管理**: Git + Conventional Commits
2. **テスティング**: Vitest + コードカバレッジ (c8)
3. **自動化**: Gulp + ファイル監視

## セットアップ

```bash
# 依存関係のインストール
npm install

# 初期セットアップとチェック
npm run setup
```

## 利用可能なコマンド

### 開発
```bash
npm run dev          # 開発サーバー起動
npm run build        # ビルド
npm run preview      # プレビュー
```

### テスト
```bash
npm run test         # テスト実行
npm run test:watch   # テスト監視モード
npm run test:coverage # コードカバレッジ
```

### コード品質
```bash
npm run lint         # 静的解析
npm run lint:fix     # 静的解析（自動修正）
npm run format       # フォーマット
npm run format:check # フォーマットチェック
```

### Gulpタスク
```bash
npm run gulp         # Gulpタスク一覧
npm run watch        # ファイル監視
npm run guard        # Guard（自動監視・修正）
npm run check        # 統合チェック
```

## Guard機能（自動化）

Ruby入門2のGuardに対応する自動化機能です：

```bash
npm run guard
```

このコマンドを実行すると：
- ファイルの変更を監視
- 変更があると自動的にlint（自動修正）→ format → testを実行
- 常に高品質なコードを保持

## プロジェクト構造

```
app/
├── src/                      # ソースコード
│   ├── fizz-buzz.ts         # FizzBuzzクラス
│   ├── fizz-buzz.test.ts    # FizzBuzzテスト
│   ├── array-learning.test.ts # 配列操作学習テスト
│   └── ...
├── coverage/                 # カバレッジレポート
├── gulpfile.js              # Gulpタスク定義
├── vitest.config.ts         # Vitestテスト設定
├── eslint.config.js         # ESLint設定
├── .prettierrc              # Prettier設定
├── .c8rc.json               # c8カバレッジ設定
├── commitlint.config.js     # コミットメッセージ規約
└── package.json             # 依存関係とスクリプト
```

## 実装済み機能

### FizzBuzz
- FizzBuzzクラスの実装
- 包括的なテストスイート
- 配列操作の学習テスト

### 三種の神器
- **バージョン管理**: Conventional Commitsでのコミットメッセージ規約
- **テスティング**: Vitestでのテスト自動化とc8でのカバレッジ計測
- **自動化**: Gulpでのタスク自動化とファイル監視

## 開発ワークフロー

1. **開発開始**:
   ```bash
   npm run guard
   ```

2. **コード編集**: ファイルを編集すると自動でlint、format、testが実行される

3. **コミット**: Conventional Commitsに従ってコミット
   ```bash
   git commit -m "feat: 新機能の追加"
   ```

4. **最終チェック**:
   ```bash
   npm run check
   ```

これにより、Ruby入門2で説明されている「ソフトウェア開発の三種の神器」をTypeScriptプロジェクトで実現できます。
