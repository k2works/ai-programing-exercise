package com.example.accounting.domain.event;

import lombok.Getter;

import java.time.LocalDateTime;
import java.util.UUID;

/**
 * ドメインイベント基底クラス
 *
 * すべてのドメインイベントはこのクラスを継承します。
 */
@Getter
public abstract class DomainEvent {
    private final String eventId;
    private final LocalDateTime occurredAt;

    protected DomainEvent() {
        this.eventId = UUID.randomUUID().toString();
        this.occurredAt = LocalDateTime.now();
    }

    /**
     * イベントタイプを取得
     *
     * @return イベントタイプ（クラス名）
     */
    public abstract String getEventType();
}
