# MRS (Meeting Room System) - Backend

## 概要

Spring Boot + Java で実装する会議室予約システムのバックエンド。データアクセスは MyBatis、認証は JWT（Stateless）、マイグレーションは Flyway、API E2E テストは Cucumber を採用。

### 目的

- Iteration 1: 認証（JWT）と会議室一覧（指定日/日付切替）の基盤を提供する。

### 前提

| ソフトウェア | バージョン | 備考 |
| :-- | :-- | :-- |
| Java | 17 | 21でも可（ビルドは8.10.2で検証） |
| Gradle | 8.10.2 | ラッパー同梱（./gradlew） |
| Spring Boot | 3.3.2 | 依存は build.gradle 参照 |
| H2 | 2.2.x | 開発（PostgreSQLモード） |
| PostgreSQL | 14+ | 本番想定 |
| Flyway | 10.x | DBマイグレーション |
| MyBatis | 3.0.3 (SB Starter) | 永続化 |
| Cucumber | 7.18.1 | API E2E |

## 構成

- [構築](#構築)
- [配置](#配置)
- [運用](#運用)
- [開発](#開発)

## 詳細

### Quick Start

```bash
# dev用のJWT秘密鍵（Base64）を環境変数で供給（例）
export JWT_SECRET=Y2hhbmdlLW1lLWNoYW5nZS1tZS1jaGFuZ2UtbWUtY2hhbmdlLW1l

# サーバ起動（H2 + Flyway + MyBatis）
./gradlew bootRun
```

### 構築

```bash
# Gradle Wrapper を生成（同梱済みだが、更新する場合）
gradle wrapper

# 依存取得とビルド（テスト含む）
./gradlew build
```

**[⬆ back to top](#構成)**

### 配置

```bash
# 実行Jarを作成
./gradlew clean bootJar

# 必要な環境変数を指定して起動（例）
export SPRING_PROFILES_ACTIVE=prd
export JWT_SECRET=... # Base64鍵
export SPRING_DATASOURCE_URL=jdbc:postgresql://host:5432/mrs
export SPRING_DATASOURCE_USERNAME=mrs
export SPRING_DATASOURCE_PASSWORD=********

java -jar build/libs/mrs-0.0.1-SNAPSHOT.jar
```

**[⬆ back to top](#構成)**

### 運用

- ヘルスチェック: `GET /actuator/health`
- プロファイル: dev（H2）/ prd（PostgreSQL）
- マイグレーション: Flyway（プロファイル別 `classpath:/db/migration/{dev,prd}`）

**[⬆ back to top](#構成)**

### 開発

```bash
# ユニット/統合/E2E（Cucumber）を含むテスト
./gradlew test

# 品質ゲート（静的解析 + テスト + カバレッジ）
./gradlew qualityCheck jacocoTestReport

# 個別タスク
./gradlew checkstyleMain pmdMain spotbugsMain
```

- サイクロマティック複雑度は7に設定（Checkstyle/PMD）。
- E2EはCucumberのBackgroundで `/api/auth/login` → Bearer 取得。

**[⬆ back to top](#構成)**

## 参照

- 設計/構成: [docs/design/アプリケーション構成.md](../../../docs/design/アプリケーション構成.md)
- アーキテクチャ: [docs/design/アーキテクチャ.md](../../../docs/design/アーキテクチャ.md)
- ドメインモデル: [docs/design/ドメインモデル.md](../../../docs/design/ドメインモデル.md)
- データモデル: [docs/design/データモデル.md](../../../docs/design/データモデル.md)
- ADR: [docs/adr/20250829.md](../../../docs/adr/20250829.md)
- イテレーション計画1: [docs/development/イテレーション計画1.md](../../../docs/development/イテレーション計画1.md)