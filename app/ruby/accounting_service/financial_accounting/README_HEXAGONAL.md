# 財務会計サービス - ヘキサゴナルアーキテクチャ実装

第9章「境界付けられたコンテキスト（Bounded Context）」の実装サンプルです。

## アーキテクチャ概要

このサービスはヘキサゴナルアーキテクチャ（ポート＆アダプターパターン）に基づいて実装されています。

### ディレクトリ構造

```
app/
├── domain/                          # ドメイン層（ビジネスロジック）
│   ├── models/
│   │   ├── journal.rb              # 仕訳集約ルート
│   │   └── journal_entry.rb        # 仕訳明細エンティティ
│   └── value_objects/
│
├── ports/                           # ポート層（インターフェース定義）
│   ├── in/                          # 入力ポート（ユースケース）
│   │   └── create_journal_use_case.rb
│   └── out/                         # 出力ポート（リポジトリ）
│       └── journal_repository.rb
│
├── application/                     # アプリケーション層（ユースケース実装）
│   └── services/
│       └── create_journal_service.rb
│
└── infrastructure/                  # インフラストラクチャ層（技術詳細）
    └── adapters/
        ├── in/
        │   └── web/                 # REST コントローラ
        └── out/
            └── persistence/         # ActiveRecord 実装
```

## 層の責務

### 1. ドメイン層
- **責務**: ビジネスルールとエンティティ
- **特徴**: フレームワーク非依存、純粋な Ruby オブジェクト
- **実装例**:
  - `Domain::Models::Journal`: 仕訳の集約ルート、貸借一致検証を実装
  - `Domain::Models::JournalEntry`: 仕訳明細、借方・貸方の金額を保持

### 2. ポート層
- **責務**: インターフェース定義
- **特徴**: 抽象的な契約のみを定義
- **実装例**:
  - `Ports::In::CreateJournalUseCase`: 仕訳作成ユースケースのインターフェース
  - `Ports::Out::JournalRepository`: 仕訳リポジトリのインターフェース

### 3. アプリケーション層
- **責務**: ユースケースの実装
- **特徴**: ドメインモデルを組み合わせてビジネスフローを実現
- **実装例**:
  - `Application::Services::CreateJournalService`: 仕訳作成ユースケースの具体的な実装

### 4. インフラストラクチャ層
- **責務**: 技術詳細の実装（REST API、データベース、メッセージング）
- **特徴**: ポートインターフェースの具体的な実装
- **実装例**: （今後実装予定）

## テストの実行

```bash
# ドメイン層のテスト
bundle exec rspec spec/domain/models/journal_spec.rb

# アプリケーション層のテスト
bundle exec rspec spec/application/services/create_journal_service_spec.rb

# すべてのテストを実行
bundle exec rspec
```

## TDD アプローチ

このプロジェクトはテスト駆動開発（TDD）で実装されています：

1. **Red**: 失敗するテストを書く
2. **Green**: テストを通す最小限のコード実装
3. **Refactor**: リファクタリング

### テストの例

```ruby
# spec/domain/models/journal_spec.rb
describe Domain::Models::Journal do
  describe '#balanced?' do
    it '貸借が一致する場合は true を返す' do
      journal = Journal.new(journal_date: Date.today, description: 'test', fiscal_year: 2024)
      journal.add_entry(JournalEntry.new(account_code: '1001', debit_amount: 100_000))
      journal.add_entry(JournalEntry.new(account_code: '4001', credit_amount: 100_000))

      expect(journal.balanced?).to be true
    end
  end
end
```

## 実装済みの機能

- ✅ ドメインモデル（Journal, JournalEntry）
- ✅ ポートインターフェース（入力ポート、出力ポート）
- ✅ ユースケース実装（CreateJournalService）
- ✅ インフラストラクチャ層（ActiveRecord アダプター）
  - JournalRecord, JournalEntryRecord（永続化モデル）
  - JournalRepositoryImpl（リポジトリ実装）
  - 伝票番号の自動生成
  - 会計年度による検索
- ✅ データベースマイグレーション
- ✅ TDD によるテストカバレッジ（ドメイン、アプリケーション、インフラ層）

## データベース設定

```yaml
# config/database.yml
development:
  database: financial_accounting_development
  username: postgres
  password: postgres
  host: localhost
  port: 5432

test:
  database: financial_accounting_test
  username: postgres
  password: postgres
  host: localhost
  port: 5434
```

## 次のステップ

- シードデータの作成
- REST API コントローラの実装
- 管理会計サービスとの連携（サービス間通信）
- Docker Compose による統合テスト環境

## 参考資料

- 記事: `docs/wiki/記事/データベース/実践データベース設計/財務会計/Ruby.md` 第9章
- ヘキサゴナルアーキテクチャ: Ports and Adapters Pattern
- ドメイン駆動設計（DDD）: Bounded Context, Aggregate Root
