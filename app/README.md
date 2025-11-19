# 会議室予約システム (Meeting Room Reservation System)

Spring Boot 3.3.2 をベースとしたヘキサゴナルアーキテクチャによる会議室予約システムです。

## 技術スタック

- Java 21
- Spring Boot 3.3.2
- Spring Security
- Spring Data JPA
- Thymeleaf
- Flyway
- H2 Database (開発環境)
- PostgreSQL (本番環境)
- Gradle 8.8
- jqwik (プロパティベーステスト)

## アーキテクチャ

ヘキサゴナルアーキテクチャ（ポート&アダプタパターン）を採用しています。

```
mrs/
├── Application.java (メインクラス)
├── WebSecurityConfig.java (セキュリティ設定)
├── application/ (アプリケーション層)
│   ├── domain/ (ドメイン層)
│   │   └── model/
│   │       ├── auth/ (認証ドメイン)
│   │       ├── reservation/ (予約ドメイン)
│   │       └── room/ (会議室ドメイン)
│   ├── port/ (ポート)
│   │   ├── in/ (入力ポート - ユースケース)
│   │   └── out/ (出力ポート - リポジトリ)
│   └── service/ (アプリケーションサービス)
│       ├── auth/
│       ├── reservation/
│       └── room/
├── infrastructure/ (インフラストラクチャ層)
│   ├── in/ (入力アダプタ)
│   │   └── web/
│   └── out/ (出力アダプタ)
│       └── persistence/
└── common/ (共通機能)
    └── validation/
```

## ビルド

```bash
# ビルド
./gradlew build

# テスト実行
./gradlew test

# アプリケーション起動
./gradlew bootRun
```

## 開発環境

### テストユーザー

開発環境では以下のテストユーザーが利用可能です：

| ユーザーID | パスワード | 権限 | 説明 |
|-----------|----------|------|------|
| `admin` | `password` | 管理者 | 全ての予約をキャンセル可能 |
| `user1` | `password` | 一般ユーザー | 自分の予約のみキャンセル可能 |
| `user2` | `password` | 一般ユーザー | 自分の予約のみキャンセル可能 |

### データベース

開発環境では H2 インメモリデータベースを使用します。

- H2 Console: http://localhost:8080/h2-console
- JDBC URL: `jdbc:h2:mem:testdb`
- Username: `sa`
- Password: (空)

### アプリケーション

- URL: http://localhost:8080
- ログインページ: http://localhost:8080/loginForm

## 本番環境

本番環境では PostgreSQL を使用します。

環境変数で設定を上書きできます：

- `DATABASE_URL`: データベース接続 URL
- `DATABASE_USERNAME`: データベースユーザー名
- `DATABASE_PASSWORD`: データベースパスワード

```bash
# 本番環境プロファイルで起動
./gradlew bootRun --args='--spring.profiles.active=prd'
```

## テスト

### 単体テスト

```bash
./gradlew test
```

### プロパティベーステスト

jqwik を使用したプロパティベーステストを実装しています。

```bash
./gradlew test --tests "*Property*"
```

## ドキュメント

詳細な設計ドキュメントは以下を参照してください：

- 要件定義: `../.kiro/specs/meeting-room-reservation-system/requirements.md`
- 設計書: `../.kiro/specs/meeting-room-reservation-system/design.md`
- 実装計画: `../.kiro/specs/meeting-room-reservation-system/tasks.md`
