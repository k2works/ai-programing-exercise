# プロジェクト構造

## ディレクトリ構成

```
ai-programing-exercise/
├── .claude/                    # Claude AI 設定
│   ├── agents/                 # エージェントロール定義
│   ├── commands/               # カスタムコマンド
│   ├── scripts/                # 自動化スクリプト
│   └── settings.json           # Claude 設定
├── .kiro/                      # Kiro AI 設定
│   └── steering/               # ステアリングルール
├── app/                        # アプリケーションコード
├── docs/                       # ドキュメント
│   ├── adr/                    # Architecture Decision Records
│   ├── design/                 # 設計ドキュメント
│   ├── development/            # 開発ガイド
│   ├── operation/              # 運用ドキュメント
│   ├── reference/              # リファレンスガイド
│   ├── requirements/           # 要件定義
│   ├── template/               # ドキュメントテンプレート
│   └── wiki/                   # Wiki コンテンツ
├── scripts/                    # ビルド・自動化スクリプト
├── Dockerfile                  # 開発環境定義
├── docker-compose.yml          # コンテナ構成
├── gulpfile.js                 # Gulp タスク定義
├── mkdocs.yml                  # MkDocs 設定
└── package.json                # npm パッケージ定義
```

## 重要なディレクトリ

### `/docs`

プロジェクトのすべてのドキュメントを格納。MkDocs でビルドされ、GitHub Pages で公開可能。

- **adr/**: アーキテクチャ決定記録
- **design/**: 設計ドキュメント
- **reference/**: 開発ガイド、ベストプラクティス
- **template/**: 再利用可能なドキュメントテンプレート
- **wiki/**: 技術メモ、記事、読書メモ

### `/docs/reference`

開発の指針となる重要なガイドを格納：

- `よいソフトウェアとは.md`: ソフトウェア品質の定義
- `開発ガイド.md`: 開発プロセスの標準
- `アーキテクチャ設計ガイド.md`: アーキテクチャパターン
- `テスト戦略ガイド.md`: テスト手法
- `コーディングとテストガイド.md`: コーディング規約

### `/.claude`

Claude AI の設定とカスタマイズ：

- **agents/roles/**: 専門ロール定義（architect、backend、frontend など）
- **commands/**: カスタムコマンド（分析、リファクタリング、PR 管理など）
- **scripts/**: 自動化スクリプト

### `/app`

実際のアプリケーションコードを格納する場所。

### `/scripts`

ビルドや自動化のためのスクリプト：

- `journal.js`: 作業履歴生成
- `mkdocs.js`: ドキュメント管理

## ファイル命名規則

- **ドキュメント**: 日本語ファイル名を使用（例: `よいソフトウェアとは.md`）
- **コード**: 英語のケバブケースまたはキャメルケース
- **設定ファイル**: 標準的な命名規則に従う（例: `package.json`、`mkdocs.yml`）

## 除外設定

- Cursor では `.windsurf/` を除外
- Windsurf では `.cursor/` を除外
