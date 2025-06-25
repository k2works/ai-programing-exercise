# 実践 AIプログラミング

## 概要

AIプログラミングの実践的な学習と開発のためのプロジェクトです。複数のプログラミング言語（Java、Node.js、Ruby）をサポートする開発環境と、MkDocsを使用したドキュメント管理システムを提供します。

### 目的

- AIプログラミングの実践的な学習環境の提供
- 複数言語（Java、Node.js、Ruby）での開発サポート
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

#### 開発環境

プロジェクトは以下の開発環境を提供します：

- **ベースOS**: Ubuntu 22.04
- **言語環境**:
  - Java 21.0.2 (via SDKMAN)
  - Maven 3.9.4 / Gradle 8.10.2
  - Node.js 22 (via NVM)
  - Ruby 3.4.4 (via rbenv)
  - Python 3.12 (システムパッケージ)
  - uv (最新バージョン、Pythonパッケージマネージャー)
- **ドキュメント環境**:
  - MkDocs (Python 3.11ベース)
  - Material for MkDocsテーマ
  - PlantUML / Mermaidによる図表サポート

### Quick Start

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

#### プロジェクト構造

```
ai-programing-exercise/
├── Dockerfile              # 開発環境のDockerfile
├── README.md               # このファイル
├── docker-compose.yml      # Docker Compose設定
├── docs/                   # ドキュメントディレクトリ
│   ├── Dockerfile          # MkDocs用Dockerfile
│   ├── assets/             # CSS/JSアセット
│   ├── index.md            # メインドキュメントページ
│   └── wiki/               # Wikiコンテンツ
│       ├── README.md       # Wiki概要
│       ├── テンプレート/    # 各種ドキュメントテンプレート
│       ├── 参照/           # 参照情報
│       ├── 記事/           # 技術記事
│       └── 読書メモ/       # 書籍の読書メモ
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
   - Ruby: `ruby -v`
   - Python: `python3 --version`
   - uv: `uv --version`

#### ドキュメント作成

- `docs/wiki/テンプレート/` ディレクトリにある各種テンプレートを活用
- PlantUMLやMermaidを使用して図表を作成可能
- Markdownで記述し、MkDocsでプレビュー

**[⬆ back to top](#構成)**

## 参照

- [MkDocs 公式サイト](https://www.mkdocs.org/)
- [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/)
- [PlantUML](https://plantuml.com/)
- [Mermaid](https://mermaid-js.github.io/mermaid/)
- [SDKMAN](https://sdkman.io/)
- [rbenv](https://github.com/rbenv/rbenv)
- [NVM](https://github.com/nvm-sh/nvm)
- [uv](https://github.com/astral-sh/uv)
