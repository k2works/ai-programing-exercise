# プロジェクト構造

## ディレクトリ構成

```
ai-programing-exercise/
├── .devcontainer/          # Dev Container 設定
├── .github/                # GitHub Actions ワークフロー
├── .kiro/                  # Kiro 設定（steering、specs など）
├── app/                    # アプリケーションコード
├── docs/                   # ドキュメントルート
│   ├── adr/                # Architecture Decision Records
│   ├── assets/             # CSS/JS アセット
│   │   ├── css/
│   │   └── js/
│   ├── design/             # 設計ドキュメント
│   ├── development/        # 開発ガイド
│   ├── operation/          # 運用ドキュメント
│   ├── reference/          # リファレンス（開発ガイド、設計ガイドなど）
│   ├── requirements/       # 要件定義
│   ├── template/           # ドキュメントテンプレート
│   ├── wiki/               # Wiki コンテンツ
│   │   ├── WIP/            # 作業中のドキュメント
│   │   ├── テンプレート/    # 日本語テンプレート
│   │   ├── 参照/           # 参照資料
│   │   ├── 技術メモ/       # 技術メモ
│   │   ├── 記事/           # 記事コンテンツ
│   │   └── 読書メモ/       # 読書メモ
│   ├── Dockerfile          # MkDocs 用 Dockerfile
│   └── index.md            # ドキュメントトップページ
├── scripts/                # ビルドスクリプト
│   ├── journal.js          # ジャーナル生成
│   └── mkdocs.js           # MkDocs タスク
├── Dockerfile              # 開発環境 Dockerfile
├── docker-compose.yml      # Docker Compose 設定
├── gulpfile.js             # Gulp タスク定義
├── mkdocs.yml              # MkDocs 設定
├── package.json            # npm パッケージ定義
├── CLAUDE.md               # AI Agent 実行ガイドライン
└── README.md               # プロジェクト README
```

## 重要なディレクトリ

### docs/reference/

開発の指針となるリファレンスドキュメント：

- `よいソフトウェアとは.md`: ソフトウェア品質の定義
- `開発ガイド.md`: 開発プロセスの標準
- `アーキテクチャ設計ガイド.md`: アーキテクチャ設計の指針
- `テスト戦略ガイド.md`: テスト戦略
- `ドメインモデル設計ガイド.md`: ドメインモデリング
- その他各種ガイド

### docs/wiki/

プロジェクトの知識ベース：

- `記事/`: 技術記事（アジャイル、アルゴリズム、アーキテクチャ、開発など）
- `WIP/`: 作業中のドキュメント
- `参照/`: 参照資料
- `技術メモ/`: 技術的なメモ
- `読書メモ/`: 読書記録

### docs/template/

ドキュメント作成用テンプレート：

- `ADR.md`: Architecture Decision Record
- `要件定義.md`: 要件定義テンプレート
- `設計.md`: 設計ドキュメントテンプレート
- `完全形式のユースケース.md`: ユースケーステンプレート
- `インセプションデッキ.md`: プロジェクト開始時のテンプレート

## ファイル命名規則

- ドキュメント: 日本語ファイル名を使用可能
- コード: 英語のスネークケースまたはキャメルケース
- 設定ファイル: 小文字のケバブケース
