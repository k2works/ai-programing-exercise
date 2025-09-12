# MRS (Meeting Room System) Backend

## プロジェクト概要

ヘキサゴナルアーキテクチャに基づく会議室予約システムのバックエンドAPI。

### 技術スタック

- **Java**: 21 LTS
- **Spring Boot**: 3.3.2
- **Spring Security**: 6.3.1 (JWT Bearer認証)
- **Spring Data JPA**: 3.3.2
- **データベース**: H2 (dev/test) / PostgreSQL (prod)
- **ビルドツール**: Gradle 8.14

### アーキテクチャ

```
src/main/java/com/k2works/mrs/
├── domain/           # ドメイン層
│   ├── model/       # エンティティ・値オブジェクト
│   ├── service/     # ドメインサービス
│   └── repository/  # リポジトリインターフェース
├── application/      # アプリケーション層
│   ├── usecase/     # ユースケース
│   ├── service/     # アプリケーションサービス
│   ├── command/     # コマンドオブジェクト
│   └── query/       # クエリオブジェクト
├── infrastructure/  # インフラストラクチャ層
│   ├── web/         # Webアダプター（Controller）
│   ├── persistence/ # データベースアダプター
│   ├── external/    # 外部システム連携
│   └── config/      # 設定・Bean定義
└── shared/          # 共通コンポーネント
    ├── exception/   # 例外定義
    ├── validation/  # バリデーション
    └── util/        # ユーティリティ
```

## 開発環境構築

### 前提条件

- Java 21+
- Git

### 初回セットアップ

```bash
# プロジェクトルートに移動
cd app/backend/mrs

# 依存関係解決とテスト実行
./gradlew test

# アプリケーション起動
./gradlew bootRun
```

### 利用可能なタスク

```bash
# 基本タスク
./gradlew build          # ビルド実行
./gradlew test           # テスト実行
./gradlew bootRun        # アプリケーション起動

# 品質チェック
./gradlew checkstyleMain # コーディング規約チェック
./gradlew pmdMain        # バグ検出
./gradlew spotbugsMain   # セキュリティ脆弱性検出
./gradlew qualityCheck   # 全品質チェック実行

# 統合チェック
./gradlew fullCheck      # テスト + 品質チェック
```

## データベース

### 開発環境

- H2 In-Memory Database
- Console: http://localhost:8080/h2-console
  - JDBC URL: `jdbc:h2:mem:mrs_db`
  - Username: `sa`
  - Password: (空)

### マイグレーション

Flyway を使用した自動マイグレーション
- ファイル場所: `src/main/resources/db/migration/`
- 初期スキーマ: `V1__Initial_schema.sql`

## API エンドポイント

### 管理

- Health Check: `GET /actuator/health`
- Metrics: `GET /actuator/metrics`
- Prometheus: `GET /actuator/prometheus`

## 品質管理

### 静的解析ツール

1. **Checkstyle**: コーディング規約チェック
   - 設定: `config/checkstyle/checkstyle.xml`
   - 循環複雑度: 7以下

2. **PMD**: バグパターン検出
   - 設定: `config/pmd/ruleset.xml`
   - 循環複雑度・認知複雑度: 7以下

3. **SpotBugs**: セキュリティ脆弱性検出
   - FindSecBugs プラグイン使用

4. **OWASP Dependency Check**: 依存関係脆弱性チェック
   - 設定: `config/owasp/suppressions.xml`
   - CVSS 7.0+ でビルド失敗

### テスト戦略

- **ユニットテスト**: JUnit 5 + AssertJ + Mockito
- **統合テスト**: Testcontainers (PostgreSQL)
- **アーキテクチャテスト**: ArchUnit (一時無効化中)

## 設定

### プロファイル

- `dev` (デフォルト): 開発環境
- `test`: テスト環境  
- `prod`: 本番環境

### 環境変数

本番環境では以下の環境変数を設定：

```env
DB_URL=jdbc:postgresql://localhost:5432/mrs_db
DB_USERNAME=mrs_user  
DB_PASSWORD=mrs_password
```

## 既知の問題

1. **Checkstyle**: 設定ファイルの互換性問題のため一時無効化
2. **PMD**: Java 21 の一部構文で処理エラーが発生するため一時無効化

## 今後の改善点

1. Checkstyle 設定の修正
2. PMD 設定の Java 21 対応
3. 実際のドメインモデル実装
4. API エンドポイントの実装
5. 統合テストの拡充