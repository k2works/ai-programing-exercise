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

### Swagger UIでの認証

**重要**: コードを変更した後は必ずバックエンドを再起動してください。

1. http://localhost:3000/docs にアクセス
2. POST /api/auth/login でログイン
3. レスポンスから `token` をコピー
4. 画面右上の **Authorize** ボタンをクリック
5. トークンを入力（**Bearerプレフィックスなし**）
   ```
   eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
   ```
6. **Authorize** をクリック
7. 認証が必要なエンドポイントが使用可能に

**トラブルシューティング**:
- 401 Unauthorizedエラーが出る場合は、バックエンドを再起動してください
- 新しいトークンを取得してください

### APIキー認証（バックエンド）

バックエンドAPIは「APIキー」または「JWT」のいずれかで認証可能です（どちらか片方でOK）。

- ヘッダー名: `x-api-key`
- 環境変数: `.env` に `API_KEY` を設定
- 適用範囲: `/api/*` のエンドポイント（`/health` と `/docs`、および `/api/auth/login` は除外）
- Swagger: `ApiKeyAuth`（`apiKey`スキーム）が追加され、UIからAPIキーの入力が可能

#### 1) 設定手順

1. `.env` にAPIキーを設定
   ```env
   API_KEY=your-secure-api-key
   ```
2. バックエンドを再起動
   - Docker: `docker-compose restart backend`
   - ローカル: `npm run dev:backend` を再起動

APIキーを未設定のままにすると、APIキーでは認証できず、JWTのみ有効になります。

#### 2) リクエスト例（cURL）

```bash
# APIキーでの呼び出し
curl -H "x-api-key: your-secure-api-key" \
     http://localhost:3000/api/items

# JWTでの呼び出し（従来通り）
curl -H "Authorization: Bearer <JWT_TOKEN>" \
     http://localhost:3000/api/items
```

#### 3) Swagger UIでの利用

1. 右上の「Authorize」をクリック
2. `bearerAuth`（JWT）または `ApiKeyAuth`（APIキー）のどちらか一方を入力
   - ApiKeyAuth: `x-api-key` に設定した値をそのまま入力
   - bearerAuth: `POST /api/auth/login` の応答 `token` を入力（Bearerプレフィックスなし）
3. Authorizeを押下し、閉じる

#### 4) 動作仕様の要点

- 認証は「APIキー or JWT」。どちらか一方が正しければアクセス可能
- 不正または未指定の場合は `401 Unauthorized` を返却
- 監視ログは最小限（キーの具体値はログ出力しません）

#### 5) セキュリティ上の注意

- `API_KEY` は十分に長く、推測困難な値を使用してください
- `.env` はバージョン管理に含めないでください（本リポジトリは`.env.example`を参照）
- キーの定期的なローテーションを推奨します（新旧キーを並行運用したい場合は、複数キー対応への拡張をご検討ください）

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
