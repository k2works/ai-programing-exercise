# Novel Game - TypeScript TDD プロジェクト

このプロジェクトは「テスト駆動開発から始めるTypeScript入門」に基づいて作成されたノベルゲーム開発基盤です。

## 特徴

- **テスト駆動開発（TDD）**: すべての機能はテストファーストで開発
- **ソフトウェア開発の三種の神器**: バージョン管理、テスティング、自動化
- **TypeScript**: 型安全性を重視した開発
- **Vite**: 高速な開発サーバーとビルドシステム
- **Vitest**: 高速なユニットテストフレームワーク
- **ESLint + Prettier**: コード品質の自動チェックとフォーマット
- **Gulp**: タスクランナーによる自動化

## セットアップ

```bash
# 依存関係のインストールと初期セットアップ
npm run setup
```

## 開発コマンド

### 基本コマンド

```bash
# 開発サーバー起動
npm run dev

# テスト実行
npm run test

# テスト監視モード
npm run test:watch

# カバレッジ付きテスト
npm run test:coverage

# ビルド
npm run build
```

### 品質管理コマンド

```bash
# ESLint実行
npm run lint

# ESLint自動修正
npm run lint:fix

# Prettier実行
npm run format

# Prettierチェック
npm run format:check

# 全体品質チェック（修正付き）
npm run check
```

### Gulpタスク

```bash
# Guard（ファイル監視+自動実行）
npm run guard

# ファイル監視
npm run watch

# 手動でGulpタスク実行
npx gulp test
npx gulp lint
npx gulp format
```

## 開発フロー

1. **Guard起動**: `npm run guard` でファイル監視開始
2. **TDD実践**: テストファーストでコード開発
3. **自動品質チェック**: ファイル保存時に自動実行される
4. **コミット**: `npm run commit` でステージング+コミット

## プロジェクト構造

```
app/
├── src/                    # ソースコード
│   ├── index.html         # メインHTML
│   ├── main.ts            # エントリーポイント
│   ├── style.css          # スタイル
│   ├── fizz-buzz.ts       # FizzBuzzサンプル
│   ├── fizz-buzz.test.ts  # FizzBuzzテスト
│   └── array-learning.test.ts # 配列学習テスト
├── coverage/              # カバレッジレポート
├── dist/                  # ビルド出力
└── node_modules/          # 依存関係
```

## ソフトウェア開発の三種の神器

### 1. バージョン管理

- Git + Conventional Commits

### 2. テスティング

- Vitest（ユニットテスト）
- カバレッジ測定（@vitest/coverage-v8）

### 3. 自動化

- ESLint（静的解析）
- Prettier（フォーマット）
- Gulp（タスクランナー）
- Guard（ファイル監視）

## 品質管理

- **循環的複雑度**: 7以下に制限
- **TypeScript**: 厳格な型チェック
- **ESLint**: コード品質ルール適用
- **Prettier**: 一貫したコードフォーマット
- **カバレッジ**: テストカバレッジ測定

## 次のステップ

このプロジェクト基盤を使用して、ノベルゲームの機能を段階的に開発していきます：

1. ゲームエンジンの基本設計
2. シナリオシステムの実装
3. UIコンポーネントの開発
4. セーブ・ロード機能
5. 音声・BGM システム

すべての機能は**テスト駆動開発**で実装し、**継続的な品質管理**を行います。
