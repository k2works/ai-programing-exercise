package com.example.accounting.application.event;

import com.example.accounting.application.service.AuditLogService;
import com.example.accounting.domain.event.AccountCreatedEvent;
import com.example.accounting.domain.event.AccountDeletedEvent;
import com.example.accounting.domain.event.AccountUpdatedEvent;
import com.example.accounting.domain.event.JournalCreatedEvent;
import com.example.accounting.domain.event.JournalDeletedEvent;
import com.example.accounting.domain.event.JournalUpdatedEvent;
import com.example.accounting.domain.model.audit.AuditLog;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * 監査イベントリスナー
 *
 * ドメインイベントを受け取り、自動的に監査ログを記録します。
 */
@Component
public class AuditEventListener {

    private final AuditLogService auditLogService;

    public AuditEventListener(AuditLogService auditLogService) {
        this.auditLogService = auditLogService;
    }

    /**
     * 勘定科目作成イベントをリッスン
     */
    @TransactionalEventListener
    @Async
    public void handleAccountCreated(AccountCreatedEvent event) {
        AuditLog auditLog = AuditLog.create(
            "Account",
            event.getAccountCode(),
            com.example.accounting.domain.model.audit.AuditAction.CREATE,
            event.getUserId(),
            event.getUserName(),
            event.getAccountData(),
            event.getIpAddress()
        );
        auditLogService.recordLog(auditLog);
    }

    /**
     * 勘定科目更新イベントをリッスン
     */
    @TransactionalEventListener
    @Async
    public void handleAccountUpdated(AccountUpdatedEvent event) {
        AuditLog auditLog = AuditLog.createForUpdate(
            "Account",
            event.getAccountCode(),
            event.getUserId(),
            event.getUserName(),
            event.getOldValues(),
            event.getNewValues(),
            event.getIpAddress()
        );
        auditLogService.recordLog(auditLog);
    }

    /**
     * 勘定科目削除イベントをリッスン
     */
    @TransactionalEventListener
    @Async
    public void handleAccountDeleted(AccountDeletedEvent event) {
        AuditLog auditLog = AuditLog.createForDelete(
            "Account",
            event.getAccountCode(),
            event.getUserId(),
            event.getUserName(),
            event.getOldValues(),
            event.getReason(),
            event.getIpAddress()
        );
        auditLogService.recordLog(auditLog);
    }

    /**
     * 仕訳作成イベントをリッスン
     */
    @TransactionalEventListener
    @Async
    public void handleJournalCreated(JournalCreatedEvent event) {
        AuditLog auditLog = AuditLog.create(
            "Journal",
            event.getJournalNo(),
            com.example.accounting.domain.model.audit.AuditAction.CREATE,
            event.getUserId(),
            event.getUserName(),
            event.getJournalData(),
            event.getIpAddress()
        );
        auditLogService.recordLog(auditLog);
    }

    /**
     * 仕訳更新イベントをリッスン
     */
    @TransactionalEventListener
    @Async
    public void handleJournalUpdated(JournalUpdatedEvent event) {
        AuditLog auditLog = AuditLog.createForUpdate(
            "Journal",
            event.getJournalNo(),
            event.getUserId(),
            event.getUserName(),
            event.getOldValues(),
            event.getNewValues(),
            event.getIpAddress()
        );
        auditLogService.recordLog(auditLog);
    }

    /**
     * 仕訳削除イベントをリッスン
     */
    @TransactionalEventListener
    @Async
    public void handleJournalDeleted(JournalDeletedEvent event) {
        AuditLog auditLog = AuditLog.createForDelete(
            "Journal",
            event.getJournalNo(),
            event.getUserId(),
            event.getUserName(),
            event.getOldValues(),
            event.getReason(),
            event.getIpAddress()
        );
        auditLogService.recordLog(auditLog);
    }
}
