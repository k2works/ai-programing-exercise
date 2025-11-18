// src/domain/event/DomainEvent.ts

/**
 * ドメインイベント基底クラス
 *
 * ドメイン内で発生した重要なビジネスイベントを表現する
 */
export abstract class DomainEvent {
  private readonly _occurredAt: Date
  private readonly _eventId: string

  constructor() {
    this._occurredAt = new Date()
    this._eventId = this.generateEventId()
  }

  get occurredAt(): Date {
    return new Date(this._occurredAt)
  }

  get eventId(): string {
    return this._eventId
  }

  abstract get eventType(): string

  private generateEventId(): string {
    return `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`
  }
}

/**
 * 仕訳作成イベント
 */
export class JournalCreatedEvent extends DomainEvent {
  constructor(
    public readonly journalId: number,
    public readonly journalDate: Date,
    public readonly description: string,
    public readonly details: unknown[],
    public readonly userId: string,
    public readonly userName: string
  ) {
    super()
  }

  get eventType(): string {
    return 'JournalCreated'
  }
}

/**
 * 勘定科目更新イベント
 */
export class AccountUpdatedEvent extends DomainEvent {
  constructor(
    public readonly accountCode: string,
    public readonly oldValues: Record<string, unknown>,
    public readonly newValues: Record<string, unknown>,
    public readonly userId: string,
    public readonly userName: string
  ) {
    super()
  }

  get eventType(): string {
    return 'AccountUpdated'
  }
}

/**
 * 仕訳削除イベント
 */
export class JournalDeletedEvent extends DomainEvent {
  constructor(
    public readonly journalId: number,
    public readonly journalData: Record<string, unknown>,
    public readonly reason: string,
    public readonly userId: string,
    public readonly userName: string
  ) {
    super()
  }

  get eventType(): string {
    return 'JournalDeleted'
  }
}
