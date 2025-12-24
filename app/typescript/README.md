# 生産管理システム (TypeScript/Prisma)

このディレクトリは、TypeScript と Prisma を使用した生産管理システムのデータベース設計および API の実装プロジェクトです。

## セットアップ手順

以下の手順に従って、開発環境を構築してください。

### 1. 前提条件

- Node.js (v22 以上推奨)
- Docker および Docker Compose

### 2. 環境変数の設定

`.env.example` ファイルをコピーして `.env` ファイルを作成し、必要に応じて設定を調整してください。

```bash
cp .env.example .env
```

デフォルト設定では PostgreSQL が使用されるようになっています。

### 3. 依存関係のインストール

```bash
npm install
```

### 4. データベースの起動

Docker Compose を使用して、データベース（PostgreSQL/MySQL）および管理ツールを起動します。

```bash
docker compose up -d
```

起動後、以下のツールにアクセスできます：
- **Adminer (DB 管理ツール)**: http://localhost:8080
- **SchemaSpy (スキーマ可視化)**: http://localhost:8081

### 5. データベースマイグレーションの実行

Prisma を使用してデータベーススキーマを適用します。

```bash
npm run prisma:migrate
```

このコマンドにより、データベースにテーブルが作成され、Prisma Client が生成されます。

> [!CAUTION]
> **マイグレーションエラー (P3006/P1014) が発生する場合**
> 既存のマイグレーションファイルがある状態で、空のデータベースに対して実行すると、「テーブルが存在しない」というエラーが発生することがあります。その場合は、一度既存のマイグレーションファイルを退避させ、以下のコマンドで初期マイグレーションを再生成してください。
>
> ```bash
> npx prisma migrate dev --name init
> ```
> その後、退避させたマイグレーションファイルを `prisma/migrations` に戻し（必要に応じてタイムスタンプを調整）、再度 `npm run prisma:migrate` を実行してください。

## 利用可能なスクリプト

| コマンド | 内容 |
| :--- | :--- |
| `npm run setup` | 依存関係のインストールとチェックを実行 |
| `npm run prisma:migrate` | データベースのマイグレーションを実行 |
| `npm run prisma:generate` | Prisma Client を生成 |
| `npm run prisma:studio` | Prisma Studio を起動 |
| `npm run test` | テストを実行 (Vitest) |
| `npm run dev:api` | 開発用 API サーバーを起動 (tsx watch) |
| `npm run format` | Prettier によるコード整形 |
| `npm run lint` | ESLint による静的解析 |

## ディレクトリ構造

- `prisma/`: Prisma スキーマおよびマイグレーションファイル
- `src/`: ソースコード
- `docker/`: Docker 関連の設定ファイル
- `scripts/`: ユーティリティスクリプト
