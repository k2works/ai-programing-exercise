# 実装サマリー：境界付けられたコンテキスト（Bounded Context）

## 概要

財務会計サービスと管理会計サービス間のイベント駆動アーキテクチャと腐敗防止層（Anti-Corruption Layer）の実装を完了しました。

**実装日**: 2025-11-18
**対象章**: 第 8 章 - 境界付けられたコンテキスト

## 実装内容

### 1. 腐敗防止層（Anti-Corruption Layer: ACL）

外部システム（財務会計サービス）のモデルが管理会計サービスのドメインモデルを汚染しないように、ACL パターンを実装しました。

#### 1.1 外部 DTO の定義

**ファイル**: `src/domain/models/external/FinancialAccountingEvent.ts`

外部システムから受信するイベントの型定義：

```typescript
export interface JournalCreatedEventDto extends FinancialAccountingEvent {
  eventType: 'JournalCreated'
  payload: {
    journalId: string
    fiscalYear: number
    journalDate: Date | string
    totalDebitAmount: number
    totalCreditAmount: number
  }
}
```

#### 1.2 イベント翻訳層

**ファイル**: `src/application/translators/FinancialAccountingEventTranslator.ts`

- 外部イベント → 内部ドメインモデルの変換
- イベントバリデーション
- 貸借一致チェック（許容誤差: 0.01）

主要メソッド：
- `translateToJournalCache()`: イベントを JournalCache に変換
- `validateEvent()`: イベントの妥当性検証

#### 1.3 HTTP アダプター

**ファイル**: `src/infrastructure/adapters/FinancialAccountingAdapter.ts`

財務会計サービスとの同期通信用アダプター：

```typescript
- fetchJournalsByFiscalYear(fiscalYear): Promise<JournalDto[]>
- fetchJournalById(journalId): Promise<JournalDto>
- fetchAccounts(): Promise<AccountDto[]>
- healthCheck(): Promise<boolean>
```

#### 1.4 ファサードサービス

**ファイル**: `src/application/services/FinancialAccountingService.ts`

イベント駆動（非同期）と HTTP（同期）の統一インターフェース：

- イベント経由のデータ受信
- REST API 経由の直接アクセス
- 外部 DTO → 内部モデルの変換

### 2. TestContainers による統合テスト

#### 2.1 テストコンテナセットアップ

**ファイル**: `tests/setup/test-containers.ts`

- PostgreSQL コンテナの起動・停止
- RabbitMQ コンテナの起動・停止
- Prisma マイグレーションの自動実行
- データベースのクリーンアップ

#### 2.2 統合テスト

**単体テスト** (`tests/unit/translators/FinancialAccountingEventTranslator.test.ts`)

- イベント変換ロジックのテスト（10 テスト）
- バリデーション機能のテスト
- エッジケースの検証

**リポジトリ統合テスト** (`tests/integration/repository/JournalCacheRepository.test.ts`)

- 実際の PostgreSQL を使用（5 テスト）
- CRUD 操作の検証
- 会計年度別の検索テスト

**ハンドラー統合テスト** (`tests/integration/handlers/JournalCreatedHandler.test.ts`)

- イベント処理の E2E テスト（5 テスト）
- バリデーションエラーの検証
- サポート外イベントの処理確認

**マルチサービス統合テスト** (`tests/e2e/multi-service.test.ts`)

- RabbitMQ を介したイベント駆動通信のテスト（7 テスト）
- パブリッシャー・サブスクライバーの統合検証
- 実際の RabbitMQ コンテナを使用

### 3. テスト結果

```
Test Files: 4 passed (4)
Tests: 27 passed (27)
Duration: 50.95s

✓ tests/unit/translators/FinancialAccountingEventTranslator.test.ts (10 tests)
✓ tests/integration/repository/JournalCacheRepository.test.ts (5 tests)
✓ tests/integration/handlers/JournalCreatedHandler.test.ts (5 tests)
✓ tests/e2e/multi-service.test.ts (7 tests)
```

全 27 テストがパスし、以下の機能が正常に動作することを確認：

- ✅ イベントの変換と検証
- ✅ データベース永続化
- ✅ RabbitMQ を介したメッセージング
- ✅ エラーハンドリング
- ✅ 会計年度別データ管理
- ✅ 貸借一致検証

## アーキテクチャ概要

### イベント駆動アーキテクチャ

```
[財務会計サービス]
        ↓ (JournalCreated イベント)
   [RabbitMQ Exchange]
        ↓ (Routing: journal.created)
   [RabbitMQ Queue]
        ↓
[管理会計サービス EventSubscriber]
        ↓
[JournalCreatedHandler]
        ↓
[FinancialAccountingEventTranslator (ACL)]
        ↓ (外部 DTO → 内部モデル)
[JournalCacheRepository]
        ↓
   [PostgreSQL]
```

### 腐敗防止層（ACL）の構造

```
外部システム
    ↓
[外部 DTO]
    ↓
[Translator] ← バリデーション
    ↓
[内部ドメインモデル]
    ↓
[リポジトリ]
    ↓
データベース
```

### 通信方式

1. **非同期（イベント駆動）**
   - RabbitMQ Topic Exchange 使用
   - Routing Key: `journal.created`
   - 永続化メッセージ
   - 手動 ACK

2. **同期（HTTP）**
   - REST API 経由
   - ヘルスチェック
   - データ同期用

## ディレクトリ構造

```
management-accounting-service/
├── src/
│   ├── application/
│   │   ├── handlers/
│   │   │   └── journal-created-handler.ts
│   │   ├── services/
│   │   │   └── FinancialAccountingService.ts
│   │   └── translators/
│   │       └── FinancialAccountingEventTranslator.ts
│   ├── domain/
│   │   └── models/
│   │       ├── external/
│   │       │   └── FinancialAccountingEvent.ts
│   │       └── journal-cache.ts
│   └── infrastructure/
│       ├── adapters/
│       │   └── FinancialAccountingAdapter.ts
│       ├── messaging/
│       │   └── EventSubscriber.ts
│       └── persistence/
│           └── PrismaJournalCacheRepository.ts
├── tests/
│   ├── unit/
│   │   └── translators/
│   │       └── FinancialAccountingEventTranslator.test.ts
│   ├── integration/
│   │   ├── handlers/
│   │   │   └── JournalCreatedHandler.test.ts
│   │   └── repository/
│   │       └── JournalCacheRepository.test.ts
│   ├── e2e/
│   │   └── multi-service.test.ts
│   └── setup/
│       └── test-containers.ts
└── docs/
    ├── ACL.md
    └── IMPLEMENTATION_SUMMARY.md (このファイル)
```

## 主要な技術的決定

### 1. ACL パターンの採用理由

- 外部サービスの変更から管理会計サービスを保護
- ドメインモデルの整合性を維持
- バリデーションの集約化

### 2. TestContainers の採用理由

- 実際のデータベース・メッセージングシステムを使用
- モックでは検証できない統合ポイントのテスト
- CI/CD パイプラインへの容易な統合

### 3. イベント検証の実装

貸借一致チェックに浮動小数点数の許容誤差（0.01）を設定：

```typescript
const difference = Math.abs(payload.totalDebitAmount - payload.totalCreditAmount)
if (difference > 0.01) {
  errors.push(`Debit and credit amounts must match (difference: ${difference.toFixed(2)})`)
}
```

### 4. RabbitMQ の設定

- **Exchange タイプ**: Topic
- **Exchange 名**: `financial-accounting-events`
- **Queue 名**: `management-accounting-queue`
- **Routing Key**: `journal.created`
- **メッセージ永続化**: 有効
- **手動 ACK**: 有効（信頼性向上）

## 依存関係

```json
{
  "dependencies": {
    "@prisma/client": "^5.10.0",
    "amqplib": "^0.10.3",
    "axios": "^1.6.0"
  },
  "devDependencies": {
    "@testcontainers/postgresql": "^10.4.0",
    "@testcontainers/rabbitmq": "^10.4.0",
    "testcontainers": "^10.4.0",
    "vitest": "^1.2.0"
  }
}
```

## パフォーマンス

### テスト実行時間

- 単体テスト: ~5ms
- リポジトリ統合テスト: ~35s（コンテナ起動含む）
- ハンドラー統合テスト: ~36s（コンテナ起動含む）
- マルチサービス E2E テスト: ~50s（コンテナ起動含む）

### 最適化ポイント

- TestContainers の並列実行による高速化検討
- キャッシュの活用
- コンテナの再利用

## 次のステップ

### 短期的な改善

1. **エラーリトライ機能の追加**
   - デッドレターキューの設定
   - エクスポネンシャルバックオフ

2. **監視・ログの強化**
   - 構造化ログの導入
   - メトリクスの収集（Prometheus）
   - 分散トレーシング（OpenTelemetry）

3. **パフォーマンス最適化**
   - バッチ処理の実装
   - イベント圧縮
   - コネクションプールの調整

### 長期的な改善

1. **SAGA パターンの導入**
   - 分散トランザクション管理
   - 補償トランザクション

2. **イベントソーシング**
   - イベントストアの導入
   - イベントリプレイ機能

3. **CQRS（Command Query Responsibility Segregation）**
   - 読み取り・書き込みモデルの分離
   - 読み取り専用の最適化されたビュー

## まとめ

境界付けられたコンテキストの実装により、以下を達成しました：

1. ✅ **疎結合なマイクロサービス**: 財務会計と管理会計の独立性を確保
2. ✅ **データ整合性**: ACL によるバリデーションと変換
3. ✅ **信頼性の高いメッセージング**: RabbitMQ の永続化と ACK
4. ✅ **包括的なテスト**: 単体・統合・E2E テストで品質を保証
5. ✅ **保守性の高い設計**: 明確な責務分離とレイヤー構造

DDD 戦略的設計パターンを実践し、変更に強く拡張可能なマイクロサービスアーキテクチャを構築しました。
