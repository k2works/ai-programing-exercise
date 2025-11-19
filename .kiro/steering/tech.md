# 技術スタック

## ビルドシステム

- **Node.js**: npm scripts + Gulp タスクランナー
- **Docker**: コンテナベースの開発環境
- **Docker Compose**: マルチコンテナ管理

## 主要技術

### ドキュメント

- **MkDocs**: ドキュメントサイトジェネレーター
- **Material for MkDocs**: テーマ
- **PlantUML / Mermaid**: 図表作成

### 開発環境（Docker コンテナ内）

- **ベース OS**: Ubuntu 22.04
- **言語環境**:
  - Java 21.0.2 (SDKMAN)
  - Scala 3.4.0 (SDKMAN)
  - Kotlin 2.0.0 (SDKMAN)
  - Clojure 1.12.1
  - Node.js 22 (NVM)
  - Ruby 3.4.4 (rbenv)
  - Python 3.12 + uv
  - PHP 8.1 + Composer
  - Haskell 9.4.8 (GHCup)
  - Go 1.22.0
  - Rust stable (rustup)
  - .NET 8.0
  - C/C++ (GCC/Clang)
  - Erlang 26.2.1 + Elixir 1.16.1
  - SWI-Prolog 9.0.4

## よく使うコマンド

### ドキュメント操作

```bash
# MkDocs サーバー起動
npm run docs:serve

# MkDocs サーバー停止
npm run docs:stop

# ドキュメントビルド
npm run docs:build
```

### 作業履歴（ジャーナル）

```bash
# すべてのコミット日付の作業履歴を生成
npm run journal

# 特定日付の作業履歴を生成
npx gulp journal:generate:date --date=YYYY-MM-DD
```

### Docker 操作

```bash
# コンテナ起動
docker-compose up -d

# 開発コンテナに入る
docker build -t ai-programming-dev .
docker run -it -v $(pwd):/srv ai-programming-dev bash

# コンテナ停止
docker-compose down
```

### GitHub Container Registry

```bash
# イメージをプル
docker pull ghcr.io/k2works/ai-programing-exercise/core:0.0.x

# タグを作成してプッシュ（自動ビルド・プッシュがトリガーされる）
git tag 0.0.x
git push origin 0.0.x
```

## パッケージマネージャー

- **npm**: Node.js パッケージ管理
- **Maven/Gradle**: Java ビルドツール
- **Composer**: PHP パッケージ管理
- **uv**: Python パッケージ管理
- **Cargo**: Rust パッケージ管理
