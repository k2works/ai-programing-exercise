package com.example.accounting.domain.model.audit;

import lombok.Builder;
import lombok.Value;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * 監査ログドメインモデル
 *
 * 不変オブジェクト（Immutable）として設計し、
 * 一度作成されたログは変更できないことを保証します。
 */
@Value
@Builder(toBuilder = true)
public class AuditLog {
    Long id;
    String entityType;
    String entityId;
    AuditAction action;
    String userId;
    String userName;
    LocalDateTime timestamp;
    Map<String, Object> oldValues;
    Map<String, Object> newValues;
    Map<String, Object> changes;
    String reason;
    String ipAddress;
    String userAgent;

    /**
     * ファクトリメソッド：CREATE操作用
     *
     * @param entityType エンティティ種別（Journal, Accountなど）
     * @param entityId エンティティID
     * @param action 操作種別
     * @param userId ユーザーID
     * @param userName ユーザー名
     * @param changes 変更内容
     * @param ipAddress IPアドレス
     * @return 監査ログ
     */
    public static AuditLog create(
            String entityType,
            String entityId,
            AuditAction action,
            String userId,
            String userName,
            Map<String, Object> changes,
            String ipAddress) {

        return AuditLog.builder()
                .entityType(entityType)
                .entityId(entityId)
                .action(action)
                .userId(userId)
                .userName(userName)
                .timestamp(LocalDateTime.now())
                .changes(changes)
                .ipAddress(ipAddress)
                .build();
    }

    /**
     * ファクトリメソッド：UPDATE操作用
     *
     * @param entityType エンティティ種別
     * @param entityId エンティティID
     * @param userId ユーザーID
     * @param userName ユーザー名
     * @param oldValues 変更前の値
     * @param newValues 変更後の値
     * @param ipAddress IPアドレス
     * @return 監査ログ
     */
    public static AuditLog createForUpdate(
            String entityType,
            String entityId,
            String userId,
            String userName,
            Map<String, Object> oldValues,
            Map<String, Object> newValues,
            String ipAddress) {

        return AuditLog.builder()
                .entityType(entityType)
                .entityId(entityId)
                .action(AuditAction.UPDATE)
                .userId(userId)
                .userName(userName)
                .timestamp(LocalDateTime.now())
                .oldValues(oldValues)
                .newValues(newValues)
                .ipAddress(ipAddress)
                .build();
    }

    /**
     * ファクトリメソッド：DELETE操作用
     *
     * @param entityType エンティティ種別
     * @param entityId エンティティID
     * @param userId ユーザーID
     * @param userName ユーザー名
     * @param oldValues 削除されるデータ
     * @param reason 削除理由
     * @param ipAddress IPアドレス
     * @return 監査ログ
     */
    public static AuditLog createForDelete(
            String entityType,
            String entityId,
            String userId,
            String userName,
            Map<String, Object> oldValues,
            String reason,
            String ipAddress) {

        return AuditLog.builder()
                .entityType(entityType)
                .entityId(entityId)
                .action(AuditAction.DELETE)
                .userId(userId)
                .userName(userName)
                .timestamp(LocalDateTime.now())
                .oldValues(oldValues)
                .reason(reason)
                .ipAddress(ipAddress)
                .build();
    }

    /**
     * サマリー文字列を生成
     *
     * @return サマリー文字列（例: "Journal 12345 を作成"）
     */
    public String getSummary() {
        return String.format("%s %s を%s",
                entityType,
                entityId,
                action.getDisplayName());
    }
}
