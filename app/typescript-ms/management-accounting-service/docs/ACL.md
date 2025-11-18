# 腐敗防止層（Anti-Corruption Layer）

## 概要

腐敗防止層（ACL）は、DDD（ドメイン駆動設計）の戦略的設計パターンの1つで、異なる境界付けられたコンテキスト間の通信を仲介し、一方のコンテキストのモデルが他方のコンテキストを汚染しないようにするレイヤーです。

## 目的

1. **ドメインモデルの独立性**: 財務会計サービスの変更が管理会計サービスに影響を与えない
2. **結合度の低減**: サービス間の依存関係を最小化
3. **バージョン互換性**: 外部システムのAPIが変更されても、内部ドメインモデルは影響を受けない
4. **変換の一元管理**: 外部データ形式と内部データ形式の変換ロジックを1箇所に集約

## アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│ 財務会計サービス（Financial Accounting Service）              │
│  - JournalCreatedEvent を RabbitMQ に発行                    │
│  - REST API による同期アクセスも提供                          │
└─────────────────────────────────────────────────────────────┘
                            │
                            │ イベント／HTTP
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 腐敗防止層（Anti-Corruption Layer）                          │
│                                                               │
│  ┌───────────────────────────────────────────────────────┐  │
│  │ FinancialAccountingEvent.ts                           │  │
│  │  - 外部システムのDTO定義                               │  │
│  └───────────────────────────────────────────────────────┘  │
│                            │                                  │
│                            ▼                                  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │ FinancialAccountingEventTranslator.ts                 │  │
│  │  - 外部イベント → 内部ドメインモデルに変換             │  │
│  │  - バリデーション                                      │  │
│  └───────────────────────────────────────────────────────┘  │
│                            │                                  │
│                            ▼                                  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │ FinancialAccountingAdapter.ts                         │  │
│  │  - HTTP通信の抽象化                                    │  │
│  │  - 同期的なデータ取得                                  │  │
│  └───────────────────────────────────────────────────────┘  │
│                            │                                  │
│                            ▼                                  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │ FinancialAccountingService.ts                         │  │
│  │  - ファサードパターン                                  │  │
│  │  - イベント購読とHTTP APIの統一インターフェース        │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
                            │
                            │ 内部ドメインモデル
                            ▼
┌─────────────────────────────────────────────────────────────┐
│ 管理会計サービス（Management Accounting Service）            │
│  - JournalCache ドメインモデル                               │
│  - ビジネスロジック                                          │
└─────────────────────────────────────────────────────────────┘
```

## コンポーネント

### 1. FinancialAccountingEvent.ts

外部システム（財務会計サービス）のイベント形式を定義するDTOです。

**責務**:
- 財務会計サービスのイベント型定義
- 型ガード関数の提供

**例**:
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

### 2. FinancialAccountingEventTranslator.ts

外部イベントを内部ドメインモデルに変換するトランスレーターです。

**責務**:
- 外部イベント形式 → 内部ドメインモデルへの変換
- イベントデータのバリデーション
- エラーハンドリング

**変換ロジック**:
```typescript
translateToJournalCache(event: FinancialAccountingEvent): JournalCache | null {
  if (!isJournalCreatedEvent(event)) {
    return null
  }

  return {
    journalId: event.payload.journalId,
    fiscalYear: event.payload.fiscalYear,
    journalDate: new Date(event.payload.journalDate),
    totalDebitAmount: event.payload.totalDebitAmount,
    totalCreditAmount: event.payload.totalCreditAmount,
    receivedAt: new Date()
  }
}
```

**バリデーション**:
- 必須フィールドの存在確認
- データ型の検証
- ビジネスルールの検証（貸借一致など）

### 3. FinancialAccountingAdapter.ts

財務会計サービスとのHTTP通信を抽象化するアダプターです。

**責務**:
- HTTP APIクライアントの管理
- 同期的なデータ取得
- エラーハンドリングとリトライ

**提供するメソッド**:
- `fetchJournalsByFiscalYear(fiscalYear)` - 会計年度別の仕訳取得
- `fetchJournalById(journalId)` - 個別の仕訳取得
- `fetchAccounts()` - 勘定科目マスタ取得
- `healthCheck()` - ヘルスチェック

### 4. FinancialAccountingService.ts

財務会計サービスとの統合を一元管理するファサードです。

**責務**:
- アダプターとトランスレーターの統合
- 高レベルなビジネスロジック
- 統一されたインターフェースの提供

**使用例**:
```typescript
const service = new FinancialAccountingService('http://financial-accounting:3001')

// 初期データロード
const caches = await service.fetchJournalCachesByFiscalYear(2024)

// ヘルスチェック
const isAvailable = await service.isAvailable()
```

### 5. JournalCreatedHandler.ts（更新）

腐敗防止層を使用してイベントを処理するハンドラーです。

**変更点**:
- トランスレーターを使用した変換処理の追加
- バリデーションロジックの統合
- エラーハンドリングの強化

**処理フロー**:
```typescript
async handle(event: FinancialAccountingEvent): Promise<void> {
  // 1. バリデーション
  const validation = this.translator.validateEvent(event)
  if (!validation.valid) {
    throw new Error(`Event validation failed: ${validation.errors.join(', ')}`)
  }

  // 2. 変換
  const journalCache = this.translator.translateToJournalCache(event)
  if (!journalCache) {
    return // サポートされていないイベント
  }

  // 3. 保存
  await this.journalCacheRepository.save(journalCache)
}
```

## 利点

### 1. ドメインモデルの独立性

財務会計サービスがイベント形式を変更しても、管理会計サービスのドメインモデルは影響を受けません。変更はトランスレーター層で吸収されます。

### 2. テスト容易性

各コンポーネントが独立しているため、単体テストが容易です：

```typescript
describe('FinancialAccountingEventTranslator', () => {
  it('should translate JournalCreatedEvent to JournalCache', () => {
    const translator = new FinancialAccountingEventTranslator()
    const event = createMockJournalCreatedEvent()
    const result = translator.translateToJournalCache(event)
    expect(result).toBeDefined()
    expect(result?.journalId).toBe('123')
  })
})
```

### 3. バージョン管理

異なるバージョンのイベント形式を同時にサポートできます：

```typescript
translateToJournalCache(event: FinancialAccountingEvent): JournalCache | null {
  if (isJournalCreatedEventV1(event)) {
    return this.fromV1(event)
  } else if (isJournalCreatedEventV2(event)) {
    return this.fromV2(event)
  }
  return null
}
```

### 4. 明確な責務分離

- **DTO**: 外部システムの形式定義
- **Translator**: 変換ロジック
- **Adapter**: 通信の抽象化
- **Service**: 統合の一元管理

## 使用方法

### イベント駆動通信（非同期）

EventSubscriber が受信したイベントは、JournalCreatedHandler を通じて自動的に処理されます。

### HTTP API通信（同期）

初期データロードやキャッシュ再構築が必要な場合：

```typescript
const service = new FinancialAccountingService(
  process.env.FINANCIAL_ACCOUNTING_SERVICE_URL || 'http://localhost:3001'
)

// 会計年度2024の仕訳データを同期的に取得
const journalCaches = await service.fetchJournalCachesByFiscalYear(2024)

// リポジトリに保存
for (const cache of journalCaches) {
  await journalCacheRepository.save(cache)
}
```

## 拡張性

### 新しいイベント型の追加

1. `FinancialAccountingEvent.ts` に新しいDTOを追加
2. `FinancialAccountingEventTranslator.ts` に変換ロジックを追加
3. 新しいイベントハンドラーを作成

### 外部システムの追加

同様のパターンで、他の境界付けられたコンテキストとの統合も実装できます：

- `SalesServiceAdapter.ts`
- `InventoryServiceAdapter.ts`
- etc.

## 参考資料

- [Domain-Driven Design (Eric Evans)](https://www.domainlanguage.com/ddd/)
- [Implementing Domain-Driven Design (Vaughn Vernon)](https://vaughnvernon.com/)
- [マイクロサービスパターン (Chris Richardson)](https://microservices.io/patterns/index.html)
