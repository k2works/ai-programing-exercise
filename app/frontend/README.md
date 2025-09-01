# 会議室予約システム - フロントエンド

Next.js を使用した会議室予約システムのフロントエンドアプリケーション。

## 技術スタック

- **Next.js 15.5** - Reactフレームワーク
- **TypeScript** - 型安全な開発
- **Tailwind CSS** - スタイリング
- **React Hook Form** - フォーム管理
- **Zod** - バリデーション
- **Axios** - HTTPクライアント
- **date-fns** - 日付処理

## セットアップ

### 依存関係のインストール
```bash
npm install
```

### 環境変数設定
`.env.local` ファイルに以下を設定:
```
NEXT_PUBLIC_API_URL=http://localhost:8080/api
```

### 開発サーバー起動
```bash
npm run dev
```
http://localhost:3000 でアクセス可能

### ビルド
```bash
npm run build
```

### 本番サーバー起動
```bash
npm run start
```

## 主要機能

### 実装済み機能
- ✅ ログイン画面
- ✅ 予約作成画面
- ✅ 認証管理（JWT）
- ✅ API通信
- ✅ バリデーション

### 未実装機能
- ⏳ 予約一覧画面
- ⏳ 予約編集・削除
- ⏳ リアルタイム更新

## ディレクトリ構造

```
frontend/
├── app/                    # Next.js App Router
│   ├── login/             # ログインページ
│   ├── reservations/      # 予約関連ページ
│   │   └── new/          # 新規予約ページ
│   └── layout.tsx        # 共通レイアウト
├── components/            # React コンポーネント
│   ├── auth-provider.tsx # 認証プロバイダー
│   ├── login-form.tsx    # ログインフォーム
│   └── reservation-form.tsx # 予約フォーム
├── services/              # APIサービス
│   ├── auth-service.ts   # 認証サービス
│   └── reservation-service.ts # 予約サービス
├── lib/                   # ユーティリティ
│   └── api-client.ts     # APIクライアント設定
└── types/                 # 型定義
    └── api.ts            # API型定義
```

## API エンドポイント

- `POST /api/auth/login` - ログイン
- `GET /api/rooms` - 会議室一覧取得
- `GET /api/rooms/{date}` - 予約可能会議室取得
- `POST /api/reservations` - 予約作成
- `GET /api/reservations/{date}` - 予約状況取得

## テストアカウント

開発環境用のテストアカウント:
- ID: `taro` / Pass: `pass123`
- ID: `hanako` / Pass: `pass456`