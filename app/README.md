# Bouquet

花束管理システムのモノレポプロジェクト

## プロジェクト構造

```
app/
├── backend/          # Fastify + Prisma バックエンド
├── frontend/         # React + Vite フロントエンド
├── shared/           # 共有型定義とユーティリティ
└── gulpfile.js       # タスクランナー設定
```

## セットアップ

### 前提条件

- Docker と Docker Compose がインストールされていること

### デフォルトユーザー

シードデータで以下のユーザーが作成されます：

- **ユーザーID**: `admin-001`
- **パスワード**: `admin123`
- **ロール**: admin
- **タイプ**: staff

### ローカル開発（Docker）

```bash
# 環境変数ファイルを作成
cp .env.example .env

# Dockerコンテナを起動
docker-compose up -d

# ログを確認
docker-compose logs -f
```

**注意**: 初回起動時は、データベースの初期化に時間がかかる場合があります。

### ローカル開発（Node.js直接）

```bash
# 環境変数ファイルをコピー
cp .env.example .env

# 依存関係のインストール
npm install

# PostgreSQLを起動（Dockerのみ）
docker-compose up -d postgres

# 開発サーバーの起動（バックエンド + フロントエンド）
npm run dev
```

## 開発コマンド

```bash
# リント
npm run lint

# フォーマット
npm run format

# 型チェック
npm run typecheck

# テスト
npm run test

# テストカバレッジ
npm run test:coverage

# ビルド
npm run build
```

## Gulpタスク

```bash
# すべてのチェックを実行
npx gulp

# リント + 自動修正
npx gulp lint:fix

# フォーマットチェック
npx gulp format:check

# 循環参照チェック
npx gulp dep:check

# コミット前チェック
npx gulp pre-commit

# ファイル監視
npx gulp watch
```

## 技術スタック

### バックエンド
- Fastify
- Prisma ORM
- PostgreSQL
- JWT認証
- Zod バリデーション
- OpenAPI/Swagger

### フロントエンド
- React 18
- Vite
- React Router v6
- Material-UI
- Axios

### 開発ツール
- TypeScript
- ESLint
- Prettier
- Vitest
- dependency-cruiser
- Gulp
