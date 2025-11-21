package com.example.accounting.application.service;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.application.port.out.DailyBalanceRepository;
import com.example.accounting.domain.model.DailyBalance;
import com.example.accounting.infrastructure.persistence.mapper.DailyAccountBalanceMapper;
import com.example.accounting.infrastructure.persistence.repository.DailyBalanceRepositoryImpl;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.InputStream;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.Properties;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 残高管理サービスのテスト
 */
@DisplayName("残高管理サービス - 統合テスト")
class BalanceServiceTest extends TestDatabaseConfig {

    private BalanceService balanceService;
    private DailyAccountBalanceMapper mapper;

    @BeforeAll
    static void setUp() throws Exception {
        // データベースセットアップ
        Flyway flyway = Flyway.configure()
                .dataSource(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword())
                .locations("classpath:db/migration")
                .load();
        flyway.migrate();

        // MyBatis セットアップ
        Properties properties = new Properties();
        properties.setProperty("driver", "org.postgresql.Driver");
        properties.setProperty("url", postgres.getJdbcUrl());
        properties.setProperty("username", postgres.getUsername());
        properties.setProperty("password", postgres.getPassword());

        InputStream inputStream = Resources.getResourceAsStream("mybatis-config.xml");
        sqlSessionFactory = new SqlSessionFactoryBuilder().build(inputStream, properties);

        // テスト用勘定科目を登録
        insertTestAccounts();
    }

    /**
     * テスト用勘定科目を登録
     */
    private static void insertTestAccounts() {
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {
            conn.createStatement().executeUpdate("""
                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
                VALUES
                ('1020', '普通預金', '資産'::account_type, false, true, 0),
                ('1030', '当座預金', '資産'::account_type, false, true, 0),
                ('2010', '買掛金', '負債'::account_type, false, true, 0),
                ('4010', '売上高', '収益'::account_type, false, true, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING
                """);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUpEach() {
        // MyBatis Mapper の取得
        mapper = sqlSessionFactory.openSession(true).getMapper(DailyAccountBalanceMapper.class);

        // Repository と Service の初期化
        DailyBalanceRepository repository = new DailyBalanceRepositoryImpl(mapper);
        balanceService = new BalanceService(repository);
    }

    @AfterEach
    void cleanup() throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                postgres.getJdbcUrl(),
                postgres.getUsername(),
                postgres.getPassword())) {
            conn.createStatement().execute("DELETE FROM \"日次勘定科目残高\"");
        }
    }

    @Test
    @DisplayName("新規の日次残高を登録できる")
    void testInsertNewDailyBalance() {
        // Given: 日次残高ドメインモデル
        LocalDate entryDate = LocalDate.of(2025, 1, 15);
        String accountCode = "1020";  // 普通預金

        DailyBalance balance = new DailyBalance();
        balance.setEntryDate(entryDate);
        balance.setAccountCode(accountCode);
        balance.setSubAccountCode("");
        balance.setDepartmentCode("");
        balance.setProjectCode("");
        balance.setSettlement(false);
        balance.setDebitAmount(new BigDecimal("100000.00"));
        balance.setCreditAmount(BigDecimal.ZERO);

        // When: 日次残高を更新（初回登録）
        balanceService.updateDailyBalance(balance);

        // Then: データが正しく登録されている
        Optional<DailyBalance> result = balanceService.getBalance(
                entryDate, accountCode, "", "", "", false);

        assertThat(result).isPresent();
        assertThat(result.get().getDebitAmount()).isEqualByComparingTo(new BigDecimal("100000.00"));
        assertThat(result.get().getCreditAmount()).isEqualByComparingTo(BigDecimal.ZERO);
    }

    @Test
    @DisplayName("既存の日次残高を加算更新できる（UPSERT）")
    void testUpsertDailyBalance() {
        // Given: 既存の日次残高が存在
        LocalDate entryDate = LocalDate.of(2025, 1, 20);
        String accountCode = "1020";

        DailyBalance initialBalance = new DailyBalance();
        initialBalance.setEntryDate(entryDate);
        initialBalance.setAccountCode(accountCode);
        initialBalance.setSubAccountCode("");
        initialBalance.setDepartmentCode("");
        initialBalance.setProjectCode("");
        initialBalance.setSettlement(false);
        initialBalance.setDebitAmount(new BigDecimal("50000.00"));
        initialBalance.setCreditAmount(BigDecimal.ZERO);

        balanceService.updateDailyBalance(initialBalance);

        // When: 同じキーで追加の残高を更新
        DailyBalance additionalBalance = new DailyBalance();
        additionalBalance.setEntryDate(entryDate);
        additionalBalance.setAccountCode(accountCode);
        additionalBalance.setSubAccountCode("");
        additionalBalance.setDepartmentCode("");
        additionalBalance.setProjectCode("");
        additionalBalance.setSettlement(false);
        additionalBalance.setDebitAmount(new BigDecimal("30000.00"));
        additionalBalance.setCreditAmount(BigDecimal.ZERO);

        balanceService.updateDailyBalance(additionalBalance);

        // Then: 金額が加算されている
        Optional<DailyBalance> result = balanceService.getBalance(
                entryDate, accountCode, "", "", "", false);

        assertThat(result).isPresent();
        assertThat(result.get().getDebitAmount()).isEqualByComparingTo(new BigDecimal("80000.00"));
    }

    @Test
    @DisplayName("補助科目・部門・プロジェクトコード別に残高を管理できる")
    void testBalanceBySubAccountAndDepartment() {
        // Given: 同じ勘定科目で異なる補助科目の残高
        LocalDate entryDate = LocalDate.of(2025, 2, 1);
        String accountCode = "1020";

        // 補助科目A
        DailyBalance balanceA = new DailyBalance();
        balanceA.setEntryDate(entryDate);
        balanceA.setAccountCode(accountCode);
        balanceA.setSubAccountCode("SUB01");
        balanceA.setDepartmentCode("D01");
        balanceA.setProjectCode("");
        balanceA.setSettlement(false);
        balanceA.setDebitAmount(new BigDecimal("10000.00"));
        balanceA.setCreditAmount(BigDecimal.ZERO);

        // 補助科目B
        DailyBalance balanceB = new DailyBalance();
        balanceB.setEntryDate(entryDate);
        balanceB.setAccountCode(accountCode);
        balanceB.setSubAccountCode("SUB02");
        balanceB.setDepartmentCode("D02");
        balanceB.setProjectCode("");
        balanceB.setSettlement(false);
        balanceB.setDebitAmount(new BigDecimal("20000.00"));
        balanceB.setCreditAmount(BigDecimal.ZERO);

        // When: 両方を登録
        balanceService.updateDailyBalance(balanceA);
        balanceService.updateDailyBalance(balanceB);

        // Then: それぞれ独立して管理されている
        Optional<DailyBalance> resultA = balanceService.getBalance(
                entryDate, accountCode, "SUB01", "D01", "", false);
        Optional<DailyBalance> resultB = balanceService.getBalance(
                entryDate, accountCode, "SUB02", "D02", "", false);

        assertThat(resultA).isPresent();
        assertThat(resultA.get().getDebitAmount()).isEqualByComparingTo(new BigDecimal("10000.00"));

        assertThat(resultB).isPresent();
        assertThat(resultB.get().getDebitAmount()).isEqualByComparingTo(new BigDecimal("20000.00"));
    }

    @Test
    @DisplayName("決算仕訳と通常仕訳を別管理できる")
    void testSettlementFlagSeparation() {
        // Given: 同じキーで決算仕訳と通常仕訳
        LocalDate entryDate = LocalDate.of(2025, 3, 31);
        String accountCode = "1020";

        // 通常仕訳
        DailyBalance normalBalance = new DailyBalance();
        normalBalance.setEntryDate(entryDate);
        normalBalance.setAccountCode(accountCode);
        normalBalance.setSubAccountCode("");
        normalBalance.setDepartmentCode("");
        normalBalance.setProjectCode("");
        normalBalance.setSettlement(false);
        normalBalance.setDebitAmount(new BigDecimal("100000.00"));
        normalBalance.setCreditAmount(BigDecimal.ZERO);

        // 決算仕訳
        DailyBalance settlementBalance = new DailyBalance();
        settlementBalance.setEntryDate(entryDate);
        settlementBalance.setAccountCode(accountCode);
        settlementBalance.setSubAccountCode("");
        settlementBalance.setDepartmentCode("");
        settlementBalance.setProjectCode("");
        settlementBalance.setSettlement(true);
        settlementBalance.setDebitAmount(BigDecimal.ZERO);
        settlementBalance.setCreditAmount(new BigDecimal("100000.00"));

        // When: 両方を登録
        balanceService.updateDailyBalance(normalBalance);
        balanceService.updateDailyBalance(settlementBalance);

        // Then: 別々に管理されている
        Optional<DailyBalance> normalResult = balanceService.getBalance(
                entryDate, accountCode, "", "", "", false);
        Optional<DailyBalance> settlementResult = balanceService.getBalance(
                entryDate, accountCode, "", "", "", true);

        assertThat(normalResult).isPresent();
        assertThat(normalResult.get().getDebitAmount()).isEqualByComparingTo(new BigDecimal("100000.00"));

        assertThat(settlementResult).isPresent();
        assertThat(settlementResult.get().getCreditAmount()).isEqualByComparingTo(new BigDecimal("100000.00"));
    }

    @Test
    @DisplayName("指定日の全残高を取得できる")
    void testGetBalancesByDate() {
        // Given: 複数の残高データ
        LocalDate entryDate = LocalDate.of(2025, 4, 1);

        DailyBalance balance1 = createBalance(entryDate, "1020", new BigDecimal("10000"));
        DailyBalance balance2 = createBalance(entryDate, "1030", new BigDecimal("20000"));
        DailyBalance balance3 = createBalance(entryDate, "2010", new BigDecimal("30000"));

        balanceService.updateDailyBalance(balance1);
        balanceService.updateDailyBalance(balance2);
        balanceService.updateDailyBalance(balance3);

        // When: 指定日の全残高を取得
        List<DailyBalance> balances = balanceService.getBalancesByDate(entryDate);

        // Then: 3件取得できる
        assertThat(balances).hasSize(3);
        assertThat(balances)
                .extracting(DailyBalance::getAccountCode)
                .containsExactlyInAnyOrder("1020", "1030", "2010");
    }

    @Test
    @DisplayName("指定期間の残高を取得できる")
    void testGetBalancesByDateRange() {
        // Given: 複数日の残高データ
        LocalDate date1 = LocalDate.of(2025, 5, 1);
        LocalDate date2 = LocalDate.of(2025, 5, 2);
        LocalDate date3 = LocalDate.of(2025, 5, 3);

        balanceService.updateDailyBalance(createBalance(date1, "1020", new BigDecimal("10000")));
        balanceService.updateDailyBalance(createBalance(date2, "1020", new BigDecimal("20000")));
        balanceService.updateDailyBalance(createBalance(date3, "1020", new BigDecimal("30000")));

        // When: 期間指定で取得
        List<DailyBalance> balances = balanceService.getBalancesByDateRange(date1, date2);

        // Then: 2件取得できる
        assertThat(balances).hasSize(2);
    }

    @Test
    @DisplayName("指定勘定科目の残高を取得できる")
    void testGetBalancesByAccountCode() {
        // Given: 同じ勘定科目で複数日の残高
        String accountCode = "1020";
        LocalDate date1 = LocalDate.of(2025, 6, 1);
        LocalDate date2 = LocalDate.of(2025, 6, 2);

        balanceService.updateDailyBalance(createBalance(date1, accountCode, new BigDecimal("10000")));
        balanceService.updateDailyBalance(createBalance(date2, accountCode, new BigDecimal("20000")));

        // When: 勘定科目指定で取得
        List<DailyBalance> balances = balanceService.getBalancesByAccountCode(accountCode);

        // Then: 2件取得できる
        assertThat(balances).hasSize(2);
        assertThat(balances)
                .allMatch(b -> accountCode.equals(b.getAccountCode()));
    }

    /**
     * テストデータ作成ヘルパー
     */
    private DailyBalance createBalance(LocalDate entryDate, String accountCode, BigDecimal amount) {
        DailyBalance balance = new DailyBalance();
        balance.setEntryDate(entryDate);
        balance.setAccountCode(accountCode);
        balance.setSubAccountCode("");
        balance.setDepartmentCode("");
        balance.setProjectCode("");
        balance.setSettlement(false);
        balance.setDebitAmount(amount);
        balance.setCreditAmount(BigDecimal.ZERO);
        return balance;
    }
}
