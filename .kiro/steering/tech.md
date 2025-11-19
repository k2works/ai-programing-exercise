# 技術スタック

## 開発環境

### ベース環境

- **OS**: Ubuntu 22.04
- **コンテナ**: Docker / Docker Compose
- **バージョン管理**: Git

### プログラミング言語

- **Java**: 21.0.2 (SDKMAN) + Maven 3.9.4 / Gradle 8.10.2
- **Scala**: 3.4.0 (SDKMAN)
- **Kotlin**: 2.0.0 (SDKMAN)
- **Clojure**: 1.12.1
- **Node.js**: 22 (NVM)
- **Ruby**: 3.4.4 (rbenv)
- **Python**: 3.12 + uv (パッケージマネージャー)
- **PHP**: 8.1 + Composer
- **Haskell**: 9.4.8 (GHCup)
- **Go**: 1.22.0
- **Rust**: stable (rustup)
- **.NET**: 8.0
- **C/C++**: GCC (C11) / G++/Clang (C++20)
- **Erlang**: 26.2.1
- **Elixir**: 1.16.1
- **Prolog**: SWI-Prolog 9.0.4

### ドキュメント環境

- **MkDocs**: Python 3.11 ベース
- **テーマ**: Material for MkDocs
- **図表**: PlantUML / Mermaid

### ビルドツール

- **タスクランナー**: Gulp 5.0.0
- **パッケージマネージャー**: npm

## 共通コマンド

### ドキュメント操作

```bash
# ドキュメントサーバー起動
npm run docs:serve

# ドキュメントサーバー停止
npm run docs:stop

# ドキュメントビルド
npm run docs:build
```

### 作業履歴管理

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
```

### コンテナイメージ管理

```bash
# GitHub Container Registry からイメージを取得
docker pull ghcr.io/k2works/ai-programing-exercise/core:0.0.x

# タグを作成してプッシュ（自動ビルド・プッシュがトリガーされる）
git tag 0.0.x
git push origin 0.0.x
```

## 開発ツール

- **Claude Code Booster**: @k2works/claude-code-booster v0.6.0
- **Gulp**: タスク自動化
- **MkDocs**: ドキュメント生成
