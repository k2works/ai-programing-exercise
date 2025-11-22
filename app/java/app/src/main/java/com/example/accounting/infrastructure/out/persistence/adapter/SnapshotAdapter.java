package com.example.accounting.infrastructure.out.persistence.adapter;

import com.example.accounting.domain.aggregate.JournalEntryAggregate;
import com.example.accounting.domain.repository.SnapshotRepository;
import com.example.accounting.infrastructure.out.persistence.dao.AggregateSnapshot;
import com.example.accounting.infrastructure.out.persistence.mapper.SnapshotMapper;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * スナップショットリポジトリ Adapter
 */
@Repository
@RequiredArgsConstructor
public class SnapshotAdapter implements SnapshotRepository {
    private final SnapshotMapper snapshotMapper;
    private final ObjectMapper objectMapper;

    @Override
    public void saveSnapshot(String aggregateId, int version, JournalEntryAggregate aggregate) {
        String snapshotData = toJson(aggregate);
        snapshotMapper.insertSnapshot(aggregateId, "JournalEntry", version, snapshotData);

        // 古いスナップショットを削除（最新 3 個のみ保持）
        snapshotMapper.deleteOldSnapshots(aggregateId, 3);
    }

    @Override
    public Optional<Snapshot> getLatestSnapshot(String aggregateId) {
        AggregateSnapshot entity = snapshotMapper.selectLatestSnapshot(aggregateId);
        if (entity == null) {
            return Optional.empty();
        }

        JournalEntryAggregate aggregate = fromJson(entity.getSnapshotData());
        return Optional.of(new Snapshot(aggregateId, entity.getVersion(), aggregate));
    }

    /**
     * JSON シリアライズ
     *
     * @param aggregate Aggregate
     * @return JSON文字列
     */
    private String toJson(JournalEntryAggregate aggregate) {
        try {
            return objectMapper.writeValueAsString(aggregate);
        } catch (JsonProcessingException e) {
            throw new RuntimeException("Failed to serialize snapshot", e);
        }
    }

    /**
     * JSON デシリアライズ
     *
     * @param json JSON文字列
     * @return Aggregate
     */
    private JournalEntryAggregate fromJson(String json) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            mapper.registerModule(new JavaTimeModule());
            return mapper.readValue(json, JournalEntryAggregate.class);
        } catch (JsonProcessingException e) {
            throw new RuntimeException("Failed to deserialize snapshot", e);
        }
    }
}
