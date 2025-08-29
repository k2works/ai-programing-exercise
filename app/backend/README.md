# MRS (Meeting Room System) - 会議室予約システム

## 概要

MRSは.NET 9を使用したヘキサゴナルアーキテクチャベースの会議室予約システムです。
テスト駆動開発（TDD）によって構築され、JWT認証、Swagger/OpenAPI統合、SQLiteデータベースを使用しています。

### 目的

- 会議室の効率的な予約管理
- セキュアな認証システム
- RESTful API による外部システム連携
- メンテナブルなアーキテクチャの実現

### 前提

| ソフトウェア | バージョン | 備考 |
| :----------- | :--------- | :--- |
| .NET SDK     | 9.0        | 必須 |
| Visual Studio | 2022      | 推奨 |
| JetBrains Rider | 最新   | 推奨 |

## 構成

- [クイックスタート](#クイックスタート)
- [構築](#構築)
- [配置](#配置)
- [運用](#運用)
- [開発](#開発)

## 詳細

### クイックスタート

```bash
# 1. 依存関係復元とビルド
dotnet restore
dotnet build

# 2. テスト実行（任意）
dotnet test

# 3. アプリケーション起動
dotnet run --project MRS.Api --urls="https://localhost:7148;http://localhost:5148"

# 4. ブラウザで Swagger UI にアクセス
# https://localhost:7148/swagger
```

**[⬆ back to top](#構成)**

### 構築

#### 技術スタック

- **.NET 9** - フレームワーク
- **ASP.NET Core Web API** - API層
- **Dapper** - O/Rマッピング
- **SQLite** - データベース
- **BCrypt.Net** - パスワードハッシュ化
- **JWT Bearer Token** - 認証
- **Swagger/OpenAPI** - API ドキュメンテーション
- **xUnit** - テストフレームワーク

#### 依存関係インストール

```bash
# プロジェクト復元
dotnet restore

# NuGet パッケージ（自動管理）
# - Microsoft.AspNetCore.Authentication.JwtBearer
# - Dapper
# - Microsoft.Data.Sqlite
# - BCrypt.Net-Next
# - Swashbuckle.AspNetCore
```

#### プロジェクト構成

```
MRS.sln
├── MRS.Api/                 # Web API層
│   ├── Controllers/         # コントローラー
│   ├── Middleware/         # ミドルウェア
│   └── Program.cs          # エントリーポイント
├── MRS.Application/        # アプリケーション層
│   ├── DTOs/              # データ転送オブジェクト
│   ├── Ports/             # インターフェース
│   └── Services/          # アプリケーションサービス
├── MRS.Infrastructure/     # インフラストラクチャ層
│   ├── Data/              # データアクセス
│   ├── Repositories/      # リポジトリ実装
│   └── Services/          # 外部サービス
└── MRS.Domain/            # ドメイン層
    ├── Entities/          # エンティティ
    └── ValueObjects/      # 値オブジェクト
```

**[⬆ back to top](#構成)**

### 配置

#### 本番環境構成

```bash
# リリースビルド
dotnet build --configuration Release

# 発行（自己完結型）
dotnet publish --configuration Release --output ./publish

# Docker イメージ作成（将来対応）
# docker build -t mrs-api .
```

#### 環境設定

```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Data Source=mrs.db;Mode=ReadWriteCreate"
  },
  "Jwt": {
    "Key": "[本番環境用の安全なキー]",
    "Issuer": "MRS.Api",
    "Audience": "MRS.Client",
    "ExpirationMinutes": 30
  }
}
```

**[⬆ back to top](#構成)**

### 運用

#### アプリケーション実行

```bash
# 開発環境
dotnet run --project MRS.Api --urls="https://localhost:7148;http://localhost:5148"

# 本番環境
dotnet MRS.Api.dll --urls="https://0.0.0.0:443;http://0.0.0.0:80"
```

#### API エンドポイント

**認証 (Authentication)**
- `POST /api/Auth/login` - ログイン
- `POST /api/Auth/refresh` - トークンリフレッシュ
- `POST /api/Auth/logout` - ログアウト
- `POST /api/Auth/validate` - トークン検証

**会議室 (Rooms)**
- `GET /api/rooms` - 全会議室取得
- `GET /api/rooms/{id}` - 会議室詳細取得
- `GET /api/rooms/available?date=2025-08-29` - 利用可能会議室取得

#### サンプルアカウント

| ユーザーID | パスワード | ロール | 名前 |
|-----------|----------|--------|------|
| admin01   | password123 | Admin  | 管理者 |
| user01    | password123 | Member | 田中太郎 |
| user02    | password123 | Member | 佐藤花子 |

#### 認証テスト手順

1. **Swagger UI アクセス**: https://localhost:7148/swagger
2. **ログインAPI実行**: `POST /api/Auth/login`

**リクエストボディ例:**
```json
{
  "userId": "admin01",
  "password": "password123"
}
```

**成功時のレスポンス:**
```json
{
  "accessToken": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "refreshToken": "refresh_token_string", 
  "expiresAt": "2025-08-29T11:00:00.000Z",
  "userInfo": {
    "userId": "admin01",
    "name": "管理者",
    "role": "Admin"
  }
}
```

#### トラブルシューティング

**ポート競合エラー**
```bash
dotnet run --project MRS.Api --urls="https://localhost:7149;http://localhost:5149"
```

**データベースエラー**
```bash
rm -f mrs.db  # 既存DBファイル削除
dotnet run --project MRS.Api  # 再初期化
```

**SSL証明書エラー**
```bash
dotnet dev-certs https --trust
```

**[⬆ back to top](#構成)**

### 開発

#### テスト駆動開発 (TDD)

```bash
# 全テスト実行
dotnet test

# 特定プロジェクトテスト
dotnet test MRS.Domain.Tests
dotnet test MRS.Application.Tests
dotnet test MRS.Infrastructure.Tests
dotnet test MRS.Api.Tests

# テスト結果詳細
dotnet test --logger "console;verbosity=detailed"
```

#### デバッグ実行

**Visual Studio / Rider:**
1. `MRS.Api` をスタートアッププロジェクトに設定
2. F5 または デバッグ実行

**コマンドライン:**
```bash
dotnet run --project MRS.Api --configuration Debug
```

#### コード品質チェック

```bash
# 静的解析
dotnet build --verbosity normal

# リリースビルド（警告チェック）
dotnet build --configuration Release --verbosity quiet
```

#### データベース管理

**手動初期化:**
```bash
# 1. DBファイル削除
rm -f app/backend/mrs.db

# 2. アプリケーション起動（自動初期化）
dotnet run --project MRS.Api
```

**スキーマ確認:**
- テーブル: Users, Rooms, ReservableRooms
- 自動作成: 初回起動時
- サンプルデータ: 自動挿入

**[⬆ back to top](#構成)**

## 参照

- **アーキテクチャ設計**: `docs/design/アーキテクチャ.md`
- **ドメインモデル**: `docs/design/ドメインモデル.md`
- **API仕様**: Swagger UI (`/swagger`)
- **開発ガイド**: `docs/reference/開発ガイド.md`
- **ADR**: `docs/adr/`

---

**作成者**: Claude Code with TDD  
**更新日**: 2025-08-29  
**バージョン**: 1.0.0