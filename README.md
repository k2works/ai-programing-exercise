# 実践 AIプログラミング

## 概要

AIプログラミングの実践的な学習と開発のためのプロジェクトです。Claude Code および Gemini CLI を使用した AI 駆動開発環境と、MkDocs を使用したドキュメント管理システムを提供します。

### 目的

- AIプログラミングの実践的な学習環境の提供
- AI 駆動開発（Claude Code、Gemini CLI）のサポート
- ドキュメント駆動開発の促進
- プログラミング学習リソースの集約

### 前提

| ソフトウェア | バージョン   | 備考 |
| :----------- |:--------| :--- |
| Docker       | 最新     | 開発環境の構築に必要 |
| Docker Compose | 最新   | 複数コンテナの管理に必要 |
| Git          | 最新     | バージョン管理に必要 |
| Node.js      | 22+     | npm スクリプト実行に必要 |

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

2. 依存関係をインストール
   ```
   npm install
   ```

3. Dockerコンテナを起動
   ```
   docker-compose up -d
   ```

4. ドキュメントサイトにアクセス
   ```
   http://localhost:8000
   ```

#### 開発環境

プロジェクトは以下の開発環境を提供します：

- **ベースOS**: Ubuntu 22.04
- **言語環境**:
  - Node.js 22 (via NVM)
- **AI開発ツール**:
  - Claude Code (@anthropic-ai/claude-code)
  - Gemini CLI (@google/gemini-cli)
  - Claude Code Booster (@k2works/claude-code-booster)
- **ドキュメント環境**:
  - MkDocs (Python 3.11ベース)
  - Material for MkDocsテーマ
  - PlantUML / Mermaidによる図表サポート

#### プロジェクト構造

```
ai-programing-exercise/
├── .devcontainer/              # Dev Container 設定
├── .github/                    # GitHub Actions 設定
├── apps/                       # アプリケーションコード
├── docs/                       # ドキュメントディレクトリ
│   ├── adr/                    # アーキテクチャ決定記録
│   ├── assets/                 # CSS/JS アセット
│   ├── design/                 # 設計ドキュメント
│   ├── development/            # 開発ガイド
│   ├── operation/              # 運用ドキュメント
│   ├── reference/              # リファレンスガイド
│   │   ├── よいソフトウェアとは.md
│   │   ├── 開発ガイド.md
│   │   ├── 要件定義ガイド.md
│   │   ├── アーキテクチャ設計ガイド.md
│   │   ├── テスト戦略ガイド.md
│   │   └── ... その他のガイド
│   ├── requirements/           # 要件定義
│   ├── template/               # テンプレート
│   ├── Dockerfile              # MkDocs 用 Dockerfile
│   └── index.md                # メインドキュメントページ
├── ops/                        # 運用・インフラ設定
│   ├── docker/                 # Docker 関連設定
│   └── scripts/                # 運用スクリプト
├── CLAUDE.md                   # Claude Code 用プロジェクト設定
├── Dockerfile                  # 開発環境の Dockerfile
├── docker-compose.yml          # Docker Compose 設定
├── gulpfile.js                 # Gulp タスク定義
├── mkdocs.yml                  # MkDocs 設定ファイル
├── package.json                # Node.js 依存関係
└── README.md                   # このファイル
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

#### npm スクリプト

| コマンド | 説明 |
|---------|------|
| `npm run docs:serve` | MkDocs サーバーを起動 |
| `npm run docs:stop` | MkDocs サーバーを停止 |
| `npm run docs:build` | MkDocs ドキュメントをビルド |
| `npm run journal` | 作業履歴（ジャーナル）を生成 |
| `npm run claude:yol` | Claude Code を権限スキップモードで起動 |

#### Gulpタスクの使用

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
docker pull ghcr.io/k2works/ai-programing-exercise:latest

# または特定バージョン
docker pull ghcr.io/k2works/ai-programing-exercise:0.0.1

# コンテナを実行
docker run -it -v $(pwd):/srv ghcr.io/k2works/ai-programing-exercise:latest
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

#### リファレンスガイド

`docs/reference/` ディレクトリには開発に必要なガイドドキュメントが含まれています：

- **よいソフトウェアとは.md** - ソフトウェア品質の定義と原則
- **開発ガイド.md** - 開発ライフサイクルとプロセス
- **要件定義ガイド.md** - RDRA による要件定義
- **アーキテクチャ設計ガイド.md** - システムアーキテクチャ設計
- **テスト戦略ガイド.md** - テスト戦略と実践
- **コーディングとテストガイド.md** - TDD とコーディング規約
- **エクストリームプログラミング.md** - XP プラクティス

**[⬆ back to top](#構成)**

## 参照

- [Claude Code ドキュメント](https://docs.anthropic.com/claude-code)
- [MkDocs ドキュメント](https://www.mkdocs.org/)
- [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/)
- [GitHub Pages](https://pages.github.com/)
- [Claude Code Booster](https://github.com/k2works/claude-code-booster)
