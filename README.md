# 実践 AIプログラミング
# 実践 AIプログラミング

## 概要

AIプログラミングの実践的な学習と開発のためのプロジェクトです。複数のプログラミング言語（Java、Scala、Kotlin、Node.js、Ruby、Python、PHP、Go、Rust、.NET）をサポートする開発環境と、MkDocsを使用したドキュメント管理システムを提供します。

### 目的

- AIプログラミングの実践的な学習環境の提供
- 複数言語（Java、Scala、Kotlin、Node.js、Ruby、Python、PHP、Go、Rust、.NET）での開発サポート
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

ドキュメントはGitHub Pagesを通じて公開することができます。リポジトリの設定でGitHub Pagesを有効にし、`/site`ディレクトリを公開ソースとして設定してください。

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

このプロジェクトでは、GitHub Container Registry（GHCR）を使用してコンテナイメージを管理しています。

##### 自動ビルド・プッシュ

タグをプッシュすると、GitHub Actionsが自動的にコンテナイメージをビルドし、GHCRにプッシュします。

```bash
# タグを作成してプッシュ
git tag 0.0.x
git push origin 0.0.x
```

##### イメージの取得・実行

GHCRからイメージを取得して実行するには：

```bash
# イメージをプル
docker pull ghcr.io/k2works/ai-programing-exercise/core:0.0.x

```

認証が必要な場合は、以下のコマンドでログインします：

```bash
# GitHub Personal Access Tokenでログイン
echo $GITHUB_TOKEN | docker login ghcr.io -u <username> --password-stdin
```

##### 権限設定

- リポジトリの設定で「Actions」の権限を適切に設定する必要があります
- `GITHUB_TOKEN`に`packages: write`権限が付与されています

##### イメージの管理

- プライベートリポジトリの場合、イメージもプライベートになります
- パッケージの可視性はリポジトリ設定から変更可能です
- 古いイメージは手動で削除する必要があります

**[⬆ back to top](#構成)**

### 開発

#### 開発環境の利用

1. 開発コンテナを起動
   ```
   docker build -t ai-programming-dev .
   docker run -it -v $(pwd):/srv ai-programming-dev bash
   ```

2. 各言語環境を利用
   - Java: `java -version`
   - Scala: `scala -version`
   - Kotlin: `kotlin -version`
   - Node.js: `node -v`
   - Clojure: `clojure -version`
   - Ruby: `ruby -v`
   - Python: `python3 --version`
   - uv: `uv --version`
   - PHP: `php -v`
   - Composer: `composer --version`
   - Haskell: `ghc --version`
   - Go: `go version`
   - Rust: `rustc --version`
   - .NET: `dotnet --version`

#### ドキュメント作成

- PlantUMLやMermaidを使用して図表を作成可能
- Markdownで記述し、MkDocsでプレビュー

**[⬆ back to top](#構成)**

## 参照

- [MkDocs 公式サイト](https://www.mkdocs.org/)
- [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/)
- [PlantUML](https://plantuml.com/)
- [Mermaid](https://mermaid-js.github.io/mermaid/)
- [SDKMAN](https://sdkman.io/)
- [Scala](https://www.scala-lang.org/)
- [Kotlin](https://kotlinlang.org/)
- [Clojure](https://clojure.org/)
- [rbenv](https://github.com/rbenv/rbenv)
- [NVM](https://github.com/nvm-sh/nvm)
- [uv](https://github.com/astral-sh/uv)
- [Haskell](https://www.haskell.org/)
- [GHCup](https://www.haskell.org/ghcup/)
- [Rust](https://www.rust-lang.org/)
- [rustup](https://rustup.rs/)
## 概要

AIプログラミングの実践的な学習と開発のためのプロジェクトです。複数のプログラミング言語（Java、Node.js、Ruby、Python、PHP、Go、Rust、.NET）をサポートする開発環境と、MkDocsを使用したドキュメント管理システムを提供します。

### 目的

- AIプログラミングの実践的な学習環境の提供
- 複数言語（Java、Node.js、Ruby、Python、PHP、Go、Rust、.NET）での開発サポート
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

ドキュメントはGitHub Pagesを通じて公開することができます。リポジトリの設定でGitHub Pagesを有効にし、`/site`ディレクトリを公開ソースとして設定してください。

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

このプロジェクトでは、GitHub Container Registry（GHCR）を使用してコンテナイメージを管理しています。

##### 自動ビルド・プッシュ

タグをプッシュすると、GitHub Actionsが自動的にコンテナイメージをビルドし、GHCRにプッシュします。

```bash
# タグを作成してプッシュ
git tag 0.0.x
git push origin 0.0.x
```

##### イメージの取得・実行

GHCRからイメージを取得して実行するには：

```bash
# イメージをプル
docker pull ghcr.io/k2works/ai-programing-exercise/core:0.0.x

```

認証が必要な場合は、以下のコマンドでログインします：

```bash
# GitHub Personal Access Tokenでログイン
echo $GITHUB_TOKEN | docker login ghcr.io -u <username> --password-stdin
```

##### 権限設定

- リポジトリの設定で「Actions」の権限を適切に設定する必要があります
- `GITHUB_TOKEN`に`packages: write`権限が付与されています

##### イメージの管理

- プライベートリポジトリの場合、イメージもプライベートになります
- パッケージの可視性はリポジトリ設定から変更可能です
- 古いイメージは手動で削除する必要があります

**[⬆ back to top](#構成)**

### 開発

#### 開発環境の利用

1. 開発コンテナを起動
   ```
   docker build -t ai-programming-dev .
   docker run -it -v $(pwd):/srv ai-programming-dev bash
   ```

2. 各言語環境を利用
   - Java: `java -version`
   - Node.js: `node -v`
   - Clojure: `clojure -version`
   - Ruby: `ruby -v`
   - Python: `python3 --version`
   - uv: `uv --version`
   - PHP: `php -v`
   - Composer: `composer --version`
   - Haskell: `ghc --version`
   - Go: `go version`
   - Rust: `rustc --version`
   - .NET: `dotnet --version`

#### ドキュメント作成

- PlantUMLやMermaidを使用して図表を作成可能
- Markdownで記述し、MkDocsでプレビュー

**[⬆ back to top](#構成)**

## 参照

- [MkDocs 公式サイト](https://www.mkdocs.org/)
- [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/)
- [PlantUML](https://plantuml.com/)
- [Mermaid](https://mermaid-js.github.io/mermaid/)
- [SDKMAN](https://sdkman.io/)
- [Clojure](https://clojure.org/)
- [rbenv](https://github.com/rbenv/rbenv)
- [NVM](https://github.com/nvm-sh/nvm)
- [uv](https://github.com/astral-sh/uv)
- [Haskell](https://www.haskell.org/)
- [GHCup](https://www.haskell.org/ghcup/)
- [Rust](https://www.rust-lang.org/)
- [rustup](https://rustup.rs/)
