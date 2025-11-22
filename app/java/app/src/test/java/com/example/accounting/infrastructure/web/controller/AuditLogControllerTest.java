package com.example.accounting.infrastructure.web.controller;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.application.port.out.AccountRepository;
import com.example.accounting.application.port.out.AuditLogRepository;
import com.example.accounting.application.service.AccountService;
import com.example.accounting.application.service.AuditLogService;
import com.example.accounting.domain.model.Account;
import com.example.accounting.domain.model.audit.AuditLog;
import com.example.accounting.infrastructure.out.persistence.adapter.AccountAdapter;
import com.example.accounting.infrastructure.out.persistence.adapter.AuditLogAdapter;
import com.example.accounting.infrastructure.out.persistence.mapper.AccountMapper;
import com.example.accounting.infrastructure.out.persistence.mapper.AuditLogMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import static org.hamcrest.Matchers.hasSize;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * 監査ログコントローラー統合テスト
 */
@DisplayName("監査ログAPI - 統合テスト")
class AuditLogControllerTest extends TestDatabaseConfig {

    private MockMvc mockMvc;
    private AuditLogService auditLogService;
    private AccountService accountService;
    private AccountMapper accountMapper;
    private AuditLogMapper auditLogMapper;

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
        accountService = new AccountService(accountRepository, event -> { });

        // Controller の初期化
        AuditLogController controller = new AuditLogController(auditLogService);
        mockMvc = MockMvcBuilders.standaloneSetup(controller).build();
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
    @DisplayName("エンティティの変更履歴を取得できる")
    void testGetEntityHistory() throws Exception {
        // Given: 勘定科目を作成して監査ログを記録
        Account account = createAccount("1010", "現金", "資産", "B", "借", "1");
        accountService.createAccount(account);

        // 監査ログを直接記録（イベント発行なしでテスト）
        AuditLog log = AuditLog.create(
            "Account", "1010",
            com.example.accounting.domain.model.audit.AuditAction.CREATE,
            "test-user", "テストユーザー",
            java.util.Map.of("accountCode", "1010", "accountName", "現金"),
            "127.0.0.1"
        );
        auditLogService.recordLog(log);

        // When/Then: API 呼び出し
        mockMvc.perform(get("/api/v1/audit-logs/entity/Account/1010")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$", hasSize(1)))
                .andExpect(jsonPath("$[0].entityType").value("Account"))
                .andExpect(jsonPath("$[0].entityId").value("1010"))
                .andExpect(jsonPath("$[0].action").value("CREATE"))
                .andExpect(jsonPath("$[0].userId").value("test-user"));
    }

    @Test
    @DisplayName("ユーザーの操作履歴を取得できる")
    void testGetUserActivity() throws Exception {
        // Given: 監査ログを記録
        AuditLog log1 = AuditLog.create(
            "Account", "1010",
            com.example.accounting.domain.model.audit.AuditAction.CREATE,
            "user1", "ユーザー1",
            java.util.Map.of("accountCode", "1010"),
            "127.0.0.1"
        );
        auditLogService.recordLog(log1);

        AuditLog log2 = AuditLog.create(
            "Account", "1020",
            com.example.accounting.domain.model.audit.AuditAction.CREATE,
            "user1", "ユーザー1",
            java.util.Map.of("accountCode", "1020"),
            "127.0.0.1"
        );
        auditLogService.recordLog(log2);

        // When/Then: API 呼び出し
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime start = now.minusHours(1);
        LocalDateTime end = now.plusHours(1);

        DateTimeFormatter formatter = DateTimeFormatter.ISO_DATE_TIME;

        mockMvc.perform(get("/api/v1/audit-logs/user/user1")
                .param("startDate", start.format(formatter))
                .param("endDate", end.format(formatter))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$", hasSize(2)))
                .andExpect(jsonPath("$[0].userId").value("user1"))
                .andExpect(jsonPath("$[1].userId").value("user1"));
    }

    @Test
    @DisplayName("期間別の監査ログを取得できる")
    void testGetAuditLogsByPeriod() throws Exception {
        // Given: 複数の監査ログを記録
        for (int i = 1; i <= 3; i++) {
            AuditLog log = AuditLog.create(
                "Account", "101" + i,
                com.example.accounting.domain.model.audit.AuditAction.CREATE,
                "user" + i, "ユーザー" + i,
                java.util.Map.of("accountCode", "101" + i),
                "127.0.0.1"
            );
            auditLogService.recordLog(log);
        }

        // When/Then: API 呼び出し
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime start = now.minusHours(1);
        LocalDateTime end = now.plusHours(1);

        DateTimeFormatter formatter = DateTimeFormatter.ISO_DATE_TIME;

        mockMvc.perform(get("/api/v1/audit-logs/period")
                .param("startDate", start.format(formatter))
                .param("endDate", end.format(formatter))
                .param("limit", "10")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$", hasSize(3)));
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
