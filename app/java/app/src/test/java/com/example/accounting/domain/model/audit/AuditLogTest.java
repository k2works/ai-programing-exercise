package com.example.accounting.domain.model.audit;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 監査ログドメインモデルのテスト
 */
@DisplayName("監査ログ - ドメインモデル")
class AuditLogTest {

    @Test
    @DisplayName("監査ログを作成できる")
    void test_監査ログを作成できる() {
        // Given
        Map<String, Object> changes = Map.of(
            "journalDate", "2025-01-15",
            "description", "売上計上",
            "amount", 100000
        );

        // When
        AuditLog auditLog = AuditLog.create(
            "Journal",
            "12345",
            AuditAction.CREATE,
            "user001",
            "山田太郎",
            changes,
            "192.168.1.100"
        );

        // Then
        assertThat(auditLog.getEntityType()).isEqualTo("Journal");
        assertThat(auditLog.getEntityId()).isEqualTo("12345");
        assertThat(auditLog.getAction()).isEqualTo(AuditAction.CREATE);
        assertThat(auditLog.getUserId()).isEqualTo("user001");
        assertThat(auditLog.getTimestamp()).isNotNull();
    }

    @Test
    @DisplayName("更新操作は変更前後の値を記録する")
    void test_更新操作は変更前後の値を記録する() {
        // Given
        Map<String, Object> oldValues = Map.of("accountName", "現金");
        Map<String, Object> newValues = Map.of("accountName", "現金及び預金");

        // When
        AuditLog auditLog = AuditLog.createForUpdate(
            "Account",
            "1001",
            "user001",
            "山田太郎",
            oldValues,
            newValues,
            "192.168.1.100"
        );

        // Then
        assertThat(auditLog.getAction()).isEqualTo(AuditAction.UPDATE);
        assertThat(auditLog.getOldValues()).isEqualTo(oldValues);
        assertThat(auditLog.getNewValues()).isEqualTo(newValues);
    }

    @Test
    @DisplayName("削除操作は理由を記録できる")
    void test_削除操作は理由を記録できる() {
        // When
        AuditLog auditLog = AuditLog.createForDelete(
            "Journal",
            "12345",
            "user001",
            "山田太郎",
            Map.of("description", "売上計上"),
            "入力ミスによる削除",
            "192.168.1.100"
        );

        // Then
        assertThat(auditLog.getAction()).isEqualTo(AuditAction.DELETE);
        assertThat(auditLog.getReason()).isEqualTo("入力ミスによる削除");
    }

    @Test
    @DisplayName("タイムスタンプが自動設定される")
    void test_タイムスタンプが自動設定される() {
        // Given
        LocalDateTime before = LocalDateTime.now();

        // When
        AuditLog auditLog = AuditLog.create(
            "Journal", "1", AuditAction.CREATE,
            "user001", "山田太郎", Map.of(), null
        );

        LocalDateTime after = LocalDateTime.now();

        // Then
        assertThat(auditLog.getTimestamp()).isBetween(before, after);
    }

    @Test
    @DisplayName("サマリー文字列を生成できる")
    void test_サマリー文字列を生成できる() {
        // When
        AuditLog auditLog = AuditLog.create(
            "Journal", "12345", AuditAction.CREATE,
            "user001", "山田太郎", Map.of(), null
        );

        // Then
        assertThat(auditLog.getSummary()).isEqualTo("Journal 12345 を作成");
    }

    @Test
    @DisplayName("CREATE操作のサマリー")
    void test_CREATE操作のサマリー() {
        // When
        AuditLog auditLog = AuditLog.create(
            "Account", "1001", AuditAction.CREATE,
            "user001", "山田太郎", Map.of(), null
        );

        // Then
        assertThat(auditLog.getSummary()).contains("作成");
    }

    @Test
    @DisplayName("UPDATE操作のサマリー")
    void test_UPDATE操作のサマリー() {
        // When
        AuditLog auditLog = AuditLog.createForUpdate(
            "Account", "1001",
            "user001", "山田太郎",
            Map.of(), Map.of(), null
        );

        // Then
        assertThat(auditLog.getSummary()).contains("更新");
    }

    @Test
    @DisplayName("DELETE操作のサマリー")
    void test_DELETE操作のサマリー() {
        // When
        AuditLog auditLog = AuditLog.createForDelete(
            "Journal", "12345",
            "user001", "山田太郎",
            Map.of(), "テスト削除", null
        );

        // Then
        assertThat(auditLog.getSummary()).contains("削除");
    }
}
