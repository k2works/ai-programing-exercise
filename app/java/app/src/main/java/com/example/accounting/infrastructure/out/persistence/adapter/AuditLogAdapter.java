package com.example.accounting.infrastructure.out.persistence.adapter;

import com.example.accounting.application.port.out.AuditLogRepository;
import com.example.accounting.domain.model.audit.AuditAction;
import com.example.accounting.domain.model.audit.AuditLog;
import com.example.accounting.infrastructure.out.persistence.mapper.AuditLogMapper;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 監査ログ Output Adapter
 * MyBatis Mapperとドメインモデルの橋渡し
 */
@Repository
public class AuditLogAdapter implements AuditLogRepository {

    private final AuditLogMapper auditLogMapper;
    private final ObjectMapper objectMapper;

    public AuditLogAdapter(AuditLogMapper auditLogMapper) {
        this.auditLogMapper = auditLogMapper;
        this.objectMapper = new ObjectMapper();
    }

    @Override
    public AuditLog save(AuditLog auditLog) {
        com.example.accounting.infrastructure.out.persistence.dao.AuditLog entity = toEntity(auditLog);
        auditLogMapper.insert(entity);
        return auditLog.toBuilder().id(entity.getLogId()).build();
    }

    @Override
    public List<AuditLog> findByEntity(String entityType, String entityId) {
        return auditLogMapper.findByEntity(entityType, entityId).stream()
                .map(this::toDomainModel)
                .collect(Collectors.toList());
    }

    @Override
    public List<AuditLog> findByUser(String userId, LocalDateTime startDate, LocalDateTime endDate) {
        return auditLogMapper.findByUser(userId, startDate, endDate).stream()
                .map(this::toDomainModel)
                .collect(Collectors.toList());
    }

    @Override
    public List<AuditLog> findByPeriod(LocalDateTime startDate, LocalDateTime endDate, int limit) {
        return auditLogMapper.findByPeriod(startDate, endDate, limit).stream()
                .map(this::toDomainModel)
                .collect(Collectors.toList());
    }

    @Override
    public List<AuditLog> findAll() {
        return auditLogMapper.findAll().stream()
                .map(this::toDomainModel)
                .collect(Collectors.toList());
    }

    /**
     * EntityからDomain Modelへ変換
     */
    private AuditLog toDomainModel(com.example.accounting.infrastructure.out.persistence.dao.AuditLog entity) {
        if (entity == null) {
            return null;
        }

        return AuditLog.builder()
                .id(entity.getLogId())
                .entityType(entity.getEntityType())
                .entityId(entity.getEntityId())
                .action(AuditAction.valueOf(entity.getAction()))
                .userId(entity.getUserId())
                .userName(entity.getUserName())
                .timestamp(entity.getTimestamp())
                .oldValues(parseJson(entity.getOldValues()))
                .newValues(parseJson(entity.getNewValues()))
                .changes(parseJson(entity.getChanges()))
                .reason(entity.getReason())
                .ipAddress(entity.getIpAddress())
                .userAgent(entity.getUserAgent())
                .build();
    }

    /**
     * Domain ModelからEntityへ変換
     */
    private com.example.accounting.infrastructure.out.persistence.dao.AuditLog toEntity(AuditLog domain) {
        com.example.accounting.infrastructure.out.persistence.dao.AuditLog entity =
                new com.example.accounting.infrastructure.out.persistence.dao.AuditLog();
        entity.setLogId(domain.getId());
        entity.setEntityType(domain.getEntityType());
        entity.setEntityId(domain.getEntityId());
        entity.setAction(domain.getAction().name());
        entity.setUserId(domain.getUserId());
        entity.setUserName(domain.getUserName());
        entity.setTimestamp(domain.getTimestamp());
        entity.setOldValues(toJson(domain.getOldValues()));
        entity.setNewValues(toJson(domain.getNewValues()));
        entity.setChanges(toJson(domain.getChanges()));
        entity.setReason(domain.getReason());
        entity.setIpAddress(domain.getIpAddress());
        entity.setUserAgent(domain.getUserAgent());
        return entity;
    }

    /**
     * MapをJSON文字列に変換
     */
    private String toJson(Map<String, Object> map) {
        if (map == null || map.isEmpty()) {
            return null;
        }
        try {
            return objectMapper.writeValueAsString(map);
        } catch (JsonProcessingException e) {
            throw new RuntimeException("Failed to convert map to JSON", e);
        }
    }

    /**
     * JSON文字列をMapに変換
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> parseJson(String json) {
        if (json == null || json.isEmpty()) {
            return null;
        }
        try {
            return objectMapper.readValue(json, Map.class);
        } catch (JsonProcessingException e) {
            throw new RuntimeException("Failed to parse JSON", e);
        }
    }
}
