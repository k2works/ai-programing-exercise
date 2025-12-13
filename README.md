# 実践 AIプログラミング

## 概要

AIプログラミングの実践的な学習と開発のためのプロジェクトです。複数のプログラミング言語（Java、Scala、Kotlin、Node.js、Ruby、Python、PHP、Go、Rust、.NET、C、C++、Erlang、Elixir、Prolog）をサポートする開発環境と、MkDocsを使用したドキュメント管理システムを提供します。

### 目的

- AIプログラミングの実践的な学習環境の提供
- 複数言語（Java、Scala、Kotlin、Node.js、Ruby、Python、PHP、Go、Rust、.NET、C、C++、Erlang、Elixir、Prolog）での開発サポート
- ドキュメント駆動開発の促進
- プログラミング学習リソースの集約

### 前提

| ソフトウェア | バージョン   | 備考 |
| :----------- |:--------| :--- |
| Docker       | 最新     | 開発環境の構築に必要 |
| Docker Compose | 最新   | 複数コンテナの管理に必要 |
| Git          | 最新     | バージョン管理に必要 |

## 構成

- [構築](#構築)
- [配置](#配置)
- [運用](#運用)
- [開発](#開発)

## 詳細

### 構築

#### Quick Start

1. リポジトリをクローン
   ```
   git clone https://github.com/k2works/ai-programing-exercise.git
   cd ai-programing-exercise
   ```

2. Dockerコンテナを起動
   ```
   docker-compose up -d
   ```

3. ドキュメントサイトにアクセス
   ```
   http://localhost:8000
   ```

#### 開発環境

プロジェクトは以下の開発環境を提供します：

- **ベースOS**: Ubuntu 22.04
- **言語環境**:
  - Java 21.0.2 (via SDKMAN)
  - Maven 3.9.4 / Gradle 8.10.2
  - Scala 3.4.0 (via SDKMAN)
  - Kotlin 2.0.0 (via SDKMAN)
  - Clojure 1.12.1 (via 公式インストーラー)
  - Node.js 22 (via NVM)
  - Ruby 3.4.4 (via rbenv)
  - Python 3.12 (システムパッケージ)
  - uv (最新バージョン、Pythonパッケージマネージャー)
  - PHP 8.1 (via Ondrej PPA)
  - Composer (PHPパッケージマネージャー)
  - Haskell 9.4.8 (via GHCup)
  - Go 1.22.0 (via 公式バイナリ)
  - Rust stable (via rustup)
  - .NET 8.0 (via Microsoft パッケージリポジトリ)
  - C言語 (C11標準, GCC)
  - C++ (C++20標準, G++, Clang)
  - Erlang 26.2.1 (via Erlang Solutions)
  - Elixir 1.16.1 (via Erlang Solutions)
  - Prolog (SWI-Prolog 9.0.4)
- **ドキュメント環境**:
  - MkDocs (Python 3.11ベース)
  - Material for MkDocsテーマ
  - PlantUML / Mermaidによる図表サポート

#### プロジェクト構造

```
ai-programing-exercise/
├── Dockerfile              # 開発環境のDockerfile
├── README.md               # このファイル
├── docker-compose.yml      # Docker Compose設定
├── docs/                   # ドキュメントディレクトリ
│   ├── Dockerfile          # MkDocs用Dockerfile
│   ├── assets/             # CSS/JSアセット
│   └── index.md            # メインドキュメントページ
└── mkdocs.yml              # MkDocs設定ファイル
```

**[⬆ back to top](#構成)**

### 配置

#### GitHub Pages セットアップ

1. **GitHub リポジトリの Settings を開く**
    - リポジトリページで `Settings` タブをクリック

2. **Pages 設定を開く**
    - 左サイドバーの `Pages` をクリック

3. **Source を設定**
    - `Source` で `Deploy from a branch` を選択
    - `Branch` で `gh-pages` を選択し、フォルダは `/ (root)` を選択
    - `Save` をクリック

4. **初回デプロイ**
    - main ブランチにプッシュすると GitHub Actions が自動実行
    - Actions タブでデプロイ状況を確認

**[⬆ back to top](#構成)**

### 運用

#### ドキュメントの編集

1. ローカル環境でMkDocsサーバーを起動
   ```
   docker-compose up mkdocs
   ```
   または、Gulpタスクを使用:
   ```
   npm run docs:serve
   ```

2. ブラウザで http://localhost:8000 にアクセスして編集結果をプレビュー

3. `docs/`ディレクトリ内のMarkdownファイルを編集

4. 変更をコミットしてプッシュ
   ```
   git add .
   git commit -m "ドキュメントの更新"
   git push
   ```

#### Gulpタスクの使用

プロジェクトには以下のGulpタスクが用意されています：

##### MkDocsタスク

- MkDocsサーバーの起動:
  ```
  npm run docs:serve
  ```
  または
  ```
  npx gulp mkdocs:serve
  ```

- MkDocsサーバーの停止:
  ```
  npm run docs:stop
  ```
  または
  ```
  npx gulp mkdocs:stop
  ```

- MkDocsドキュメントのビルド:
  ```
  npm run docs:build
  ```
  または
  ```
  npx gulp mkdocs:build
  ```

##### 作業履歴（ジャーナル）タスク

- すべてのコミット日付の作業履歴を生成:
  ```
  npm run journal
  ```
  または
  ```
  npx gulp journal:generate
  ```

- 特定の日付の作業履歴を生成:
  ```
  npx gulp journal:generate:date --date=YYYY-MM-DD
  ```
  (例: `npx gulp journal:generate:date --date=2023-04-01`)

生成された作業履歴は `docs/journal/` ディレクトリに保存され、各ファイルには指定された日付のコミット情報が含まれます。

#### GitHub Container Registry

このプロジェクトでは、GitHub Container Registry（GHCR）を使用して開発コンテナイメージを管理しています。

##### 自動ビルド・プッシュ

タグをプッシュすると、GitHub Actions が自動的にコンテナイメージをビルドし、GHCR にプッシュします。

```bash
# タグを作成してプッシュ
git tag 0.0.1
git push origin 0.0.1
```

##### イメージの取得・実行

GHCR からイメージを取得して実行するには：

```bash
# イメージをプル
docker pull ghcr.io/k2works/grokkingfp-excersice:latest

# または特定バージョン
docker pull ghcr.io/k2works/grokkingfp-excersice:0.0.1

# コンテナを実行
docker run -it -v $(pwd):/srv ghcr.io/k2works/grokkingfp-excersice:latest
```

認証が必要な場合は、以下のコマンドでログインします：

```bash
# GitHub Personal Access Token でログイン
echo $GITHUB_TOKEN | docker login ghcr.io -u <username> --password-stdin
```

##### 権限設定

- リポジトリの Settings → Actions → General で `Read and write permissions` を設定
- `GITHUB_TOKEN` に `packages: write` 権限が付与されています

##### Dev Container の使用

VS Code で Dev Container を使用する場合：

1. VS Code で「Dev Containers: Reopen in Container」を実行
2. または「Dev Containers: Rebuild and Reopen in Container」で再ビルド

**[⬆ back to top](#構成)**

### 開発

**[⬆ back to top](#構成)**

## 参照
