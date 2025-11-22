package com.example.accounting.infrastructure.out.persistence;

import com.example.accounting.domain.event.DomainEvent;
import com.example.accounting.domain.event.JournalEntryApprovedEvent;
import com.example.accounting.domain.event.JournalEntryCreatedEvent;
import com.example.accounting.domain.event.JournalEntryDeletedEvent;
import com.example.accounting.domain.repository.ConcurrentModificationException;
import com.example.accounting.domain.repository.EventStoreRepository;
import com.example.accounting.infrastructure.out.persistence.dao.EventStore;
import com.example.accounting.infrastructure.out.persistence.mapper.EventStoreMapper;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

/**
 * イベントストアリポジトリ実装（MyBatis）
 */
@Repository
@RequiredArgsConstructor
public class EventStoreRepositoryImpl implements EventStoreRepository {
    private final EventStoreMapper eventStoreMapper;
    private final ObjectMapper objectMapper;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    @Transactional
    public void save(String aggregateId, List<DomainEvent> events, int expectedVersion) {
        int currentVersion = getCurrentVersion(aggregateId);

        if (currentVersion != expectedVersion) {
            throw new ConcurrentModificationException(
                String.format("Expected version %d but was %d", expectedVersion, currentVersion)
            );
        }

        int sequenceNumber = currentVersion + 1;
        for (DomainEvent event : events) {
            String eventData = toJson(event);

            try {
                EventStore eventStore = EventStore.builder()
                    .aggregateId(aggregateId)
                    .aggregateType("JournalEntry")
                    .eventType(event.getEventType())
                    .eventVersion(event.getEventVersion())
                    .eventData(eventData)
                    .occurredAt(event.getOccurredAt())
                    .userId(event.getUserId())
                    .correlationId(null)
                    .causationId(null)
                    .sequenceNumber(sequenceNumber)
                    .build();

                eventStoreMapper.insert(eventStore);
                sequenceNumber++;

                // イベント発行（Projection のトリガー）
                eventPublisher.publishEvent(event);
            } catch (DuplicateKeyException e) {
                throw new ConcurrentModificationException(
                    "Concurrent modification detected for aggregate: " + aggregateId, e
                );
            }
        }
    }

    @Override
    public List<DomainEvent> getEvents(String aggregateId) {
        List<EventStore> entities = eventStoreMapper.findByAggregateId(aggregateId);
        return entities.stream()
                .map(this::toDomainEvent)
                .toList();
    }

    @Override
    public List<DomainEvent> getEventsUntil(String aggregateId, LocalDateTime pointInTime) {
        List<EventStore> entities = eventStoreMapper.findByAggregateIdUntil(aggregateId, pointInTime);
        return entities.stream()
                .map(this::toDomainEvent)
                .toList();
    }

    @Override
    public int getCurrentVersion(String aggregateId) {
        Integer maxSequence = eventStoreMapper.getLatestSequenceNumber(aggregateId);
        return maxSequence != null ? maxSequence : 0;
    }

    /**
     * JSON シリアライズ
     *
     * @param obj オブジェクト
     * @return JSON文字列
     */
    private String toJson(Object obj) {
        try {
            return objectMapper.writeValueAsString(obj);
        } catch (JsonProcessingException e) {
            throw new RuntimeException("Failed to serialize event to JSON", e);
        }
    }

    /**
     * イベントのデシリアライズ
     *
     * @param entity イベントストアエンティティ
     * @return ドメインイベント
     */
    private DomainEvent toDomainEvent(EventStore entity) {
        try {
            return switch (entity.getEventType()) {
                case "JournalEntryCreatedEvent" ->
                    objectMapper.readValue(entity.getEventData(), JournalEntryCreatedEvent.class);
                case "JournalEntryApprovedEvent" ->
                    objectMapper.readValue(entity.getEventData(), JournalEntryApprovedEvent.class);
                case "JournalEntryDeletedEvent" ->
                    objectMapper.readValue(entity.getEventData(), JournalEntryDeletedEvent.class);
                default ->
                    throw new IllegalArgumentException("Unknown event type: " + entity.getEventType());
            };
        } catch (JsonProcessingException e) {
            throw new RuntimeException("Failed to deserialize event from JSON", e);
        }
    }
}
