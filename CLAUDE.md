# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

これは、複数のプログラミング言語でのテスト駆動開発（TDD）学習を目的とした多言語AIプログラミング演習環境です。プロジェクトはドキュメント駆動開発を重視し、Dockerコンテナを通じて包括的な多言語開発環境を提供します。

## 開発環境

### Dockerベースの多言語環境
プロジェクトは12種類以上のプログラミング言語を含む完全な開発環境を提供します：

```bash
# 開発環境を開始
docker-compose up -d

# すべての言語を含む開発コンテナにアクセス
docker-compose exec app bash

# ドキュメントサーバーを開始（localhost:8000）
docker-compose up mkdocs
```

**利用可能な言語:**
Java 21.0.2、Scala 3.4.0、Kotlin 2.0.0、Clojure 1.12.1、Node.js 22、Ruby 3.4.4、Python 3.12、PHP 8.1、Haskell 9.4.8、Go 1.22.0、Rust (stable)、.NET 8.0

## 共通コマンド

### ドキュメント管理
```bash
npm run docs:serve          # localhost:8000でMkDocsサーバーを開始
npm run docs:build          # 静的ドキュメントをビルド
npm run docs:stop           # ドキュメントサーバーを停止
npm run journal             # git履歴から開発日誌を生成

# 代替のGulpコマンド
npx gulp mkdocs:serve        # 代替サーバー開始
npx gulp mkdocs:stop         # 代替サーバー停止
npx gulp journal:generate    # すべてのコミット日誌を生成
npx gulp journal:generate:date --date=YYYY-MM-DD  # 特定日付の日誌を生成
```

### 言語固有のビルドコマンド

**Java/Kotlin/Scala（プロジェクトが存在する場合）:**
```bash
# Mavenプロジェクト
mvn test && mvn compile && mvn package

# Gradleプロジェクト
gradle test && gradle build

# 単一テスト実行
gradle test --tests SpecificTestClass
```

**Node.js:**
```bash
npm test && npm run build
yarn test && yarn build
```

**Python:**
```bash
uv run pytest              # uvを使用した現代的なPythonテスト
uv build                   # uvを使用した現代的なPythonビルド
```

**その他の言語:**
```bash
# Ruby
bundle exec rake test && bundle exec rake build

# Go
go test ./... && go build

# Rust
cargo test && cargo build

# .NET
dotnet test && dotnet build
```

## プロジェクトアーキテクチャ

### コンテナアーキテクチャ
- **app**: すべてのツールチェーンを含む多言語開発コンテナ
- **mkdocs**: PlantUMLサポート付きドキュメントサーバー
- **plantuml**: 専用PlantUML図表サーバー（plantuml:8080）

### ドキュメントシステム
プロジェクトはMaterialテーマ付きMkDocsを使用し、以下をサポートします：
- アーキテクチャ可視化のためのPlantUML図表
- プロセスフロー用のMermaid図表
- 自動git履歴日誌生成
- `docs/wiki/`内の広範囲な日本語TDD学習資料

### 主要な開発パターン
- **テスト駆動開発**: すべての実装はTDD実践に従うべきです
- **ドキュメントファースト**: すべての新機能に対して包括的なドキュメントを作成
- **多言語一貫性**: 異なる言語間で一貫したパターンを維持
- **コンテナファースト開発**: すべての開発は標準化されたコンテナ内で行われます

## GitHub Copilot統合

プロジェクトには特定のガイダンスを含む`.github/copilot-instructions.md`があります：
- `docs/wiki/開発プロセス標準.md`からのTDD手法に従う
- アーキテクチャドキュメント用のPlantUML図表を作成
- 外部サービス（Notion、GitHub、Slack、Atlassian、Wiki.js）用のMCP Server統合を使用
- 特定フォーマットで開発日誌を維持

### MCP Serverワークフロー
```bash
# 日誌管理ワークフロー
git log [start]..[end] --oneline                    # git履歴を分析
git diff [start]..[end] -- [file]                   # 特定の変更をレビュー

# 日誌はdocs/journal/YYYYMMDD.mdに保存
# MCP Serverによる外部システムとの自動統合
```

### GitHub Container Registry
```bash
# タグによる自動ビルドトリガー
git tag 0.0.x && git push origin 0.0.x

# 公開イメージの取得
docker pull ghcr.io/k2works/ai-programing-exercise/core:0.0.x
```

## 開発哲学

`docs/wiki/開発プロセス標準.md`に基づいて、プロジェクトは以下を重視します：
- **問題解決指向**: 問題解決指向の開発アプローチ
- **よいソフトウェア**: 明確な原則を持つ「よいソフトウェア」の作成に焦点
- **アジャイル開発**: アジャイル開発手法
- **包括的ドキュメント**: 包括的なドキュメント実践

## 重要なファイルの場所

**設定:**
- `mkdocs.yml`: ドキュメントサイト設定
- `gulpfile.js`: タスク自動化定義
- `docker-compose.yml`: コンテナオーケストレーション
- `package.json`: Node.js依存関係とスクリプト

**ドキュメント:**
- `docs/wiki/`: 広範囲なTDDと開発手法ガイド
- `docs/journal/`: 開発日誌と日次ログ
- `script/`: 自動化用Gulpタスク定義

**テンプレート:**
- `docs/wiki/テンプレート/`: ADR、ユーザーストーリー、設計テンプレートを含むドキュメントテンプレート

## このコードベースでの作業

1. **Docker環境から開始**: 開発には常に`docker-compose up -d`を使用
2. **ドキュメントファーストアプローチ**: 変更があればMkDocsドキュメントを更新
3. **TDD実践に従う**: 既存の例に示されているようにテストファースト開発を実装
4. **日誌を生成**: 開発進捗を記録するために`npm run journal`を使用
5. **言語一貫性を維持**: 新しい言語実装を追加する際は確立されたパターンに従う
6. **PlantUMLを使用**: 複雑な機能にはアーキテクチャ図表を作成

## 新規開発のための注意事項

- これは**学習環境テンプレート**です - 実際のアプリケーションコードは`app/`ディレクトリに追加すべきです
- すべての言語ツールチェーンはDocker環境にプリインストールされています
- `docs/wiki/`内の広範囲な日本語ドキュメントが手法ガイダンスを提供します
- 日誌生成はMCP Serverと統合され、外部サービス同期を行います
- プロジェクト哲学との一貫性を保つためにGitHub Copilot指示に従ってください