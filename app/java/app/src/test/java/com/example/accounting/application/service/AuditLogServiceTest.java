package com.example.accounting.application.service;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.application.port.out.AuditLogRepository;
import com.example.accounting.application.service.finacial.AuditLogService;
import com.example.accounting.domain.model.audit.AuditAction;
import com.example.accounting.domain.model.audit.AuditLog;
import com.example.accounting.infrastructure.out.persistence.adapter.AuditLogAdapter;
import com.example.accounting.infrastructure.out.persistence.mapper.AuditLogMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 監査ログサービスの統合テスト
 */
@DisplayName("監査ログサービス - 統合テスト")
class AuditLogServiceTest extends TestDatabaseConfig {

    private AuditLogService auditLogService;
    private AuditLogMapper mapper;

    @BeforeEach
    void setUpEach() {
        // MyBatis Mapper の取得
        mapper = sqlSessionFactory.openSession(true).getMapper(AuditLogMapper.class);

        // Repository と Service の初期化
        AuditLogRepository repository = new AuditLogAdapter(mapper);
        auditLogService = new AuditLogService(repository);
    }

    @AfterEach
    void cleanup() throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().execute("DELETE FROM \"監査ログ\"");
        }
    }

    @Test
    @DisplayName("CREATE操作の監査ログを記録できる")
    void testRecordCreateLog() {
        // Given
        Map<String, Object> changes = Map.of(
            "journalNo", "J001",
            "journalDate", "2025-01-15",
            "description", "売上計上"
        );

        AuditLog auditLog = AuditLog.create(
            "Journal",
            "J001",
            AuditAction.CREATE,
            "user001",
            "山田太郎",
            changes,
            "192.168.1.100"
        );

        // When
        AuditLog recorded = auditLogService.recordLog(auditLog);

        // Then
        assertThat(recorded.getId()).isNotNull();
        assertThat(recorded.getEntityType()).isEqualTo("Journal");
        assertThat(recorded.getEntityId()).isEqualTo("J001");
        assertThat(recorded.getAction()).isEqualTo(AuditAction.CREATE);
        assertThat(recorded.getUserId()).isEqualTo("user001");
        assertThat(recorded.getChanges()).containsEntry("journalNo", "J001");
    }

    @Test
    @DisplayName("UPDATE操作の監査ログを記録できる")
    void testRecordUpdateLog() {
        // Given
        Map<String, Object> oldValues = Map.of("accountName", "現金");
        Map<String, Object> newValues = Map.of("accountName", "現金及び預金");

        AuditLog auditLog = AuditLog.createForUpdate(
            "Account",
            "1001",
            "user001",
            "山田太郎",
            oldValues,
            newValues,
            "192.168.1.100"
        );

        // When
        AuditLog recorded = auditLogService.recordLog(auditLog);

        // Then
        assertThat(recorded.getId()).isNotNull();
        assertThat(recorded.getAction()).isEqualTo(AuditAction.UPDATE);
        assertThat(recorded.getOldValues()).containsEntry("accountName", "現金");
        assertThat(recorded.getNewValues()).containsEntry("accountName", "現金及び預金");
    }

    @Test
    @DisplayName("DELETE操作の監査ログを記録できる")
    void testRecordDeleteLog() {
        // Given
        Map<String, Object> oldValues = Map.of("journalNo", "J001", "description", "売上計上");

        AuditLog auditLog = AuditLog.createForDelete(
            "Journal",
            "J001",
            "user001",
            "山田太郎",
            oldValues,
            "入力ミスによる削除",
            "192.168.1.100"
        );

        // When
        AuditLog recorded = auditLogService.recordLog(auditLog);

        // Then
        assertThat(recorded.getId()).isNotNull();
        assertThat(recorded.getAction()).isEqualTo(AuditAction.DELETE);
        assertThat(recorded.getReason()).isEqualTo("入力ミスによる削除");
        assertThat(recorded.getOldValues()).containsEntry("journalNo", "J001");
    }

    @Test
    @DisplayName("エンティティの監査ログを取得できる")
    void testGetLogsByEntity() {
        // Given: 同じエンティティに対する複数の操作
        auditLogService.recordLog(AuditLog.create(
            "Journal", "J001", AuditAction.CREATE,
            "user001", "山田太郎", Map.of("description", "作成"), null
        ));

        auditLogService.recordLog(AuditLog.createForUpdate(
            "Journal", "J001",
            "user002", "佐藤花子",
            Map.of("amount", 100000),
            Map.of("amount", 150000),
            null
        ));

        // When
        List<AuditLog> logs = auditLogService.getLogsByEntity("Journal", "J001");

        // Then
        assertThat(logs).hasSize(2);
        assertThat(logs.get(0).getAction()).isEqualTo(AuditAction.UPDATE); // 新しい順
        assertThat(logs.get(1).getAction()).isEqualTo(AuditAction.CREATE);
    }

    @Test
    @DisplayName("ユーザーの監査ログを取得できる")
    void testGetLogsByUser() {
        // Given
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime yesterday = now.minusDays(1);
        LocalDateTime tomorrow = now.plusDays(1);

        auditLogService.recordLog(AuditLog.create(
            "Journal", "J001", AuditAction.CREATE,
            "user001", "山田太郎", Map.of(), null
        ));

        auditLogService.recordLog(AuditLog.create(
            "Account", "1001", AuditAction.CREATE,
            "user001", "山田太郎", Map.of(), null
        ));

        auditLogService.recordLog(AuditLog.create(
            "Journal", "J002", AuditAction.CREATE,
            "user002", "佐藤花子", Map.of(), null
        ));

        // When
        List<AuditLog> logs = auditLogService.getLogsByUser("user001", yesterday, tomorrow);

        // Then
        assertThat(logs).hasSize(2);
        assertThat(logs).allMatch(log -> "user001".equals(log.getUserId()));
    }

    @Test
    @DisplayName("期間で監査ログを取得できる")
    void testGetLogsByPeriod() {
        // Given
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime yesterday = now.minusDays(1);
        LocalDateTime tomorrow = now.plusDays(1);

        for (int i = 0; i < 5; i++) {
            auditLogService.recordLog(AuditLog.create(
                "Journal", "J00" + i, AuditAction.CREATE,
                "user001", "山田太郎", Map.of(), null
            ));
        }

        // When: 3件に制限
        List<AuditLog> logs = auditLogService.getLogsByPeriod(yesterday, tomorrow, 3);

        // Then
        assertThat(logs).hasSize(3);
    }

    @Test
    @DisplayName("全ての監査ログを取得できる")
    void testGetAllLogs() {
        // Given
        auditLogService.recordLog(AuditLog.create(
            "Journal", "J001", AuditAction.CREATE,
            "user001", "山田太郎", Map.of(), null
        ));

        auditLogService.recordLog(AuditLog.create(
            "Account", "1001", AuditAction.CREATE,
            "user002", "佐藤花子", Map.of(), null
        ));

        // When
        List<AuditLog> logs = auditLogService.getAllLogs();

        // Then
        assertThat(logs).hasSize(2);
    }

    @Test
    @DisplayName("JSONBフィールドが正しく保存・取得できる")
    void testJsonbFields() {
        // Given
        Map<String, Object> changes = Map.of(
            "journalNo", "J001",
            "amount", 100000,
            "items", List.of("item1", "item2")
        );

        AuditLog auditLog = AuditLog.create(
            "Journal", "J001", AuditAction.CREATE,
            "user001", "山田太郎", changes, null
        );

        // When
        AuditLog recorded = auditLogService.recordLog(auditLog);
        List<AuditLog> logs = auditLogService.getLogsByEntity("Journal", "J001");

        // Then
        assertThat(logs).hasSize(1);
        AuditLog retrieved = logs.get(0);
        assertThat(retrieved.getChanges()).containsEntry("journalNo", "J001");
        assertThat(retrieved.getChanges()).containsEntry("amount", 100000);
        assertThat(retrieved.getChanges()).containsKey("items");
    }
}
