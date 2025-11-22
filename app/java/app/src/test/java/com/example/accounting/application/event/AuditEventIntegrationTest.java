package com.example.accounting.application.event;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.application.port.out.AccountRepository;
import com.example.accounting.application.port.out.AuditLogRepository;
import com.example.accounting.application.service.finacial.AccountService;
import com.example.accounting.application.service.finacial.AuditLogService;
import com.example.accounting.domain.model.financial.Account;
import com.example.accounting.domain.model.audit.AuditAction;
import com.example.accounting.domain.model.audit.AuditLog;
import com.example.accounting.infrastructure.in.event.AuditEventListenerAdapter;
import com.example.accounting.infrastructure.out.persistence.adapter.AccountAdapter;
import com.example.accounting.infrastructure.out.persistence.adapter.AuditLogAdapter;
import com.example.accounting.infrastructure.out.persistence.mapper.AccountMapper;
import com.example.accounting.infrastructure.out.persistence.mapper.AuditLogMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.PayloadApplicationEvent;
import org.springframework.context.event.SimpleApplicationEventMulticaster;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 監査イベント統合テスト
 *
 * ドメインイベントが監査ログに自動的に記録されることを確認
 */
@DisplayName("監査イベント - 統合テスト")
class AuditEventIntegrationTest extends TestDatabaseConfig {

    private AccountService accountService;
    private AuditLogService auditLogService;
    private AccountMapper accountMapper;
    private AuditLogMapper auditLogMapper;
    private SimpleApplicationEventMulticaster eventPublisher;

    @BeforeEach
    void setUpEach() {
        // MyBatis Mapper の取得
        accountMapper = sqlSessionFactory.openSession(true).getMapper(AccountMapper.class);
        auditLogMapper = sqlSessionFactory.openSession(true).getMapper(AuditLogMapper.class);

        // Repository の初期化
        AccountRepository accountRepository = new AccountAdapter(accountMapper);
        AuditLogRepository auditLogRepository = new AuditLogAdapter(auditLogMapper);

        // Service の初期化
        auditLogService = new AuditLogService(auditLogRepository);

        // Event Publisher の設定（同期実行）
        eventPublisher = new SimpleApplicationEventMulticaster();
        AuditEventListenerAdapter listener = new AuditEventListenerAdapter(auditLogService);
        eventPublisher.addApplicationListener(event -> {
            if (event instanceof PayloadApplicationEvent) {
                Object payload = ((PayloadApplicationEvent<?>) event).getPayload();
                if (payload instanceof com.example.accounting.domain.event.AccountCreatedEvent) {
                    listener.handleAccountCreated((com.example.accounting.domain.event.AccountCreatedEvent) payload);
                } else if (payload instanceof com.example.accounting.domain.event.AccountUpdatedEvent) {
                    listener.handleAccountUpdated((com.example.accounting.domain.event.AccountUpdatedEvent) payload);
                } else if (payload instanceof com.example.accounting.domain.event.AccountDeletedEvent) {
                    listener.handleAccountDeleted((com.example.accounting.domain.event.AccountDeletedEvent) payload);
                }
            }
        });

        // テスト用の EventPublisher ラッパー
        ApplicationEventPublisher publisher = new ApplicationEventPublisher() {
            @Override
            public void publishEvent(Object event) {
                eventPublisher.multicastEvent(new PayloadApplicationEvent<>(this, event));
            }
        };

        accountService = new AccountService(accountRepository, publisher);
    }

    @AfterEach
    void cleanup() throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().execute("DELETE FROM \"監査ログ\"");
            conn.createStatement().execute("DELETE FROM \"勘定科目マスタ\"");
        }
    }

    @Test
    @DisplayName("勘定科目作成時に監査ログが自動記録される")
    void testAccountCreationGeneratesAuditLog() throws InterruptedException {
        // Given
        Account account = createAccount("1010", "現金", "資産", "B", "借", "1");

        // When
        accountService.createAccount(account);

        // 非同期処理を待つ（同期実行なので即座に完了）
        Thread.sleep(100);

        // Then: 監査ログが記録されている
        List<AuditLog> logs = auditLogService.getLogsByEntity("Account", "1010");
        assertThat(logs).hasSize(1);

        AuditLog log = logs.get(0);
        assertThat(log.getAction()).isEqualTo(AuditAction.CREATE);
        assertThat(log.getEntityType()).isEqualTo("Account");
        assertThat(log.getEntityId()).isEqualTo("1010");
        assertThat(log.getUserId()).isEqualTo("system");
        assertThat(log.getChanges()).containsEntry("accountCode", "1010");
        assertThat(log.getChanges()).containsEntry("accountName", "現金");
    }

    @Test
    @DisplayName("勘定科目更新時に監査ログが自動記録される")
    void testAccountUpdateGeneratesAuditLog() throws InterruptedException {
        // Given: 既存の勘定科目
        Account account = createAccount("1010", "現金", "資産", "B", "借", "1");
        accountService.createAccount(account);
        Thread.sleep(100);

        // When: 科目名を更新
        Account updatedAccount = createAccount("1010", "現金及び預金", "資産", "B", "借", "1");
        accountService.updateAccount("1010", updatedAccount);
        Thread.sleep(100);

        // Then: UPDATE の監査ログが記録されている
        List<AuditLog> logs = auditLogService.getLogsByEntity("Account", "1010");
        assertThat(logs).hasSize(2); // CREATE + UPDATE

        AuditLog updateLog = logs.get(0); // 新しい順
        assertThat(updateLog.getAction()).isEqualTo(AuditAction.UPDATE);
        assertThat(updateLog.getOldValues()).containsEntry("accountName", "現金");
        assertThat(updateLog.getNewValues()).containsEntry("accountName", "現金及び預金");
    }

    @Test
    @DisplayName("勘定科目削除時に監査ログが自動記録される")
    void testAccountDeletionGeneratesAuditLog() throws InterruptedException {
        // Given: 既存の勘定科目
        Account account = createAccount("1010", "現金", "資産", "B", "借", "1");
        accountService.createAccount(account);
        Thread.sleep(100);

        // When: 勘定科目を削除
        accountService.deleteAccount("1010");
        Thread.sleep(100);

        // Then: DELETE の監査ログが記録されている
        List<AuditLog> logs = auditLogService.getLogsByEntity("Account", "1010");
        assertThat(logs).hasSize(2); // CREATE + DELETE

        AuditLog deleteLog = logs.get(0); // 新しい順
        assertThat(deleteLog.getAction()).isEqualTo(AuditAction.DELETE);
        assertThat(deleteLog.getOldValues()).containsEntry("accountCode", "1010");
        assertThat(deleteLog.getReason()).isEqualTo("削除");
    }

    @Test
    @DisplayName("複数の操作が順番に監査ログに記録される")
    void testMultipleOperationsGenerateAuditLogs() throws InterruptedException {
        // Given & When: CREATE -> UPDATE -> DELETE
        Account account = createAccount("1010", "現金", "資産", "B", "借", "1");
        accountService.createAccount(account);
        Thread.sleep(100);

        Account updated = createAccount("1010", "現金及び預金", "資産", "B", "借", "1");
        accountService.updateAccount("1010", updated);
        Thread.sleep(100);

        accountService.deleteAccount("1010");
        Thread.sleep(100);

        // Then: 3件の監査ログが記録されている
        List<AuditLog> logs = auditLogService.getLogsByEntity("Account", "1010");
        assertThat(logs).hasSize(3);

        // 新しい順: DELETE, UPDATE, CREATE
        assertThat(logs.get(0).getAction()).isEqualTo(AuditAction.DELETE);
        assertThat(logs.get(1).getAction()).isEqualTo(AuditAction.UPDATE);
        assertThat(logs.get(2).getAction()).isEqualTo(AuditAction.CREATE);
    }

    /**
     * テストデータ作成ヘルパー
     */
    private Account createAccount(String code, String name, String accountType,
                                   String bsplType, String debitCreditType, String elementType) {
        Account account = new Account();
        account.setAccountCode(code);
        account.setAccountName(name);
        account.setAccountAbbr(name);
        account.setAccountKana(name);
        account.setBsplType(bsplType);
        account.setDebitCreditType(debitCreditType);
        account.setElementType(elementType);
        account.setAggregationType("1");
        account.setDisplayOrder(100);
        return account;
    }
}
