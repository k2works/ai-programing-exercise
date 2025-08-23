# プロダクト概要

## 概要

TypeScriptとReactで実装されたぷよぷよゲームです。ヘキサゴナルアーキテクチャと関数型プログラミングの原則に基づいて設計されています。

### 目的

- ぷよぷよゲームの基本機能を提供
- テスト駆動開発(TDD)の実践
- ヘキサゴナルアーキテクチャの実装例
- 関数型プログラミングパラダイムの適用

### 前提

| ソフトウェア | バージョン | 備考 |
| :----------- |:--------| :--- |
| Node.js      | 20.x    | JavaScript実行環境 |
| npm          | 10.x    | パッケージマネージャー |
| Git          | 最新     | バージョン管理 |

### 技術スタック

| カテゴリ | 技術 | 用途 |
| :--- | :--- | :--- |
| **フレームワーク** | React 19.1.1 | UIライブラリ |
| **言語** | TypeScript 5.8.3 | 型安全性 |
| **ビルドツール** | Vite 7.1.2 | 高速開発環境 |
| **状態管理** | Zustand 5.0.7 | 軽量状態管理 |
| **テスト** | Vitest 3.2.4 | 単体・統合テスト |
| **E2Eテスト** | Playwright 1.54.2 | エンドツーエンドテスト |
| **コード品質** | ESLint + Prettier | 静的解析・整形 |
| **アーキテクチャ検証** | dependency-cruiser | 依存関係チェック |
| **カバレッジ管理** | Codecov | テストカバレッジ監視 |

## 構成

- [構築](#構築)
- [配置](#配置)
- [運用](#運用)
- [開発](#開発)

## 詳細

### 構築

#### Quick Start

```bash
# 依存関係のインストール
npm install

# 開発サーバーの起動
npm run dev

# テストの実行
npm test

# ビルド
npm run build
```

**[⬆ back to top](#構成)**

### 配置

#### Vercel デプロイ

このプロジェクトはVercelへの自動デプロイが設定されています。

1. mainブランチへのpushで自動デプロイ
2. Pull Requestでプレビューデプロイ

#### ローカルプレビュー

```bash
npm run build
npm run preview
```

**[⬆ back to top](#構成)**

### 運用

#### スクリプト一覧

| コマンド | 説明 |
| :--- | :--- |
| `npm run dev` | 開発サーバー起動 |
| `npm run build` | プロダクションビルド |
| `npm run preview` | ビルドプレビュー |
| `npm test` | テスト実行（ウォッチモード） |
| `npm run test:coverage` | カバレッジ付きテスト実行 |
| `npm run test:ui` | Vitest UIでテスト実行 |
| `npm run test:e2e` | E2Eテスト実行 |
| `npm run test:e2e:ui` | Playwright UIでE2Eテスト実行 |
| `npm run test:e2e:report` | E2Eテストレポート表示 |
| `npm run lint` | ESLintでコード検証 |
| `npm run format` | Prettierでコード整形 |
| `npm run format:check` | フォーマットチェック |

**[⬆ back to top](#構成)**

### 開発

#### アーキテクチャ

ヘキサゴナルアーキテクチャ（ポートとアダプターパターン）を採用：

```
src/
├── domain/           # ドメイン層（ビジネスロジック）
│   ├── models/      # ドメインモデル（Puyo, Game）
│   └── services/    # ドメインサービス
├── application/      # アプリケーション層
│   ├── ports/       # インターフェース定義
│   ├── services/    # ユースケース
│   └── stores/      # 状態管理（Zustand）
├── infrastructure/   # インフラストラクチャ層
│   └── repositories/# データ永続化
└── presentation/     # プレゼンテーション層
    └── components/   # UIコンポーネント
```

#### テスト戦略

- **単体テスト**: Vitestを使用
- **統合テスト**: React Testing Library
- **E2Eテスト**: Playwright（Chrome, Firefox, Safari対応）
- **3A原則**: Arrange, Act, Assert
- **テストファースト**: TDDアプローチ
- **日本語テスト記述**: 可読性の向上

#### 開発フロー

1. featureブランチの作成
2. TDDサイクル（Red-Green-Refactor）
3. コード品質チェック
4. Pull Requestの作成
5. コードレビュー
6. mainブランチへマージ

**[⬆ back to top](#構成)**
