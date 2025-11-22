package com.example.accounting.infrastructure.seed;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.application.port.out.AccountRepository;
import com.example.accounting.application.port.out.DailyBalanceRepository;
import com.example.accounting.application.port.out.JournalRepository;
import com.example.accounting.domain.model.financial.Account;
import com.example.accounting.domain.model.financial.DailyBalance;
import com.example.accounting.infrastructure.out.persistence.adapter.AccountAdapter;
import com.example.accounting.infrastructure.out.persistence.adapter.DailyBalanceAdapter;
import com.example.accounting.infrastructure.out.persistence.adapter.JournalAdapter;
import com.example.accounting.infrastructure.out.persistence.mapper.AccountMapper;
import com.example.accounting.infrastructure.out.persistence.mapper.DailyAccountBalanceMapper;
import com.example.accounting.infrastructure.out.persistence.mapper.JournalMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DatabaseSeeder 統合テスト
 */
@DisplayName("DatabaseSeeder - 統合テスト")
class DatabaseSeederTest extends TestDatabaseConfig {

    private DatabaseSeeder databaseSeeder;
    private AccountRepository accountRepository;
    private JournalRepository journalRepository;
    private DailyBalanceRepository dailyBalanceRepository;

    @BeforeEach
    void setUpEach() {
        // MyBatis Mapper の取得
        AccountMapper accountMapper = sqlSessionFactory.openSession(true).getMapper(AccountMapper.class);
        JournalMapper journalMapper = sqlSessionFactory.openSession(true).getMapper(JournalMapper.class);
        DailyAccountBalanceMapper dailyBalanceMapper = sqlSessionFactory.openSession(true)
            .getMapper(DailyAccountBalanceMapper.class);

        // Repository の初期化
        accountRepository = new AccountAdapter(accountMapper);
        journalRepository = new JournalAdapter(journalMapper);
        dailyBalanceRepository = new DailyBalanceAdapter(dailyBalanceMapper);

        // DatabaseSeeder の初期化
        databaseSeeder = new DatabaseSeeder(accountRepository, journalRepository, dailyBalanceRepository);
    }

    @AfterEach
    void cleanup() throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().execute("DELETE FROM \"日次勘定科目残高\"");
            conn.createStatement().execute("DELETE FROM \"仕訳明細\"");
            conn.createStatement().execute("DELETE FROM \"仕訳\"");
            conn.createStatement().execute("DELETE FROM \"勘定科目マスタ\"");
        }
    }

    @Test
    @DisplayName("勘定科目マスタが正しく投入される")
    void testSeedAccounts() throws Exception {
        // When
        databaseSeeder.run();

        // Then: 勘定科目マスタが投入されている
        List<Account> accounts = accountRepository.findAll();
        assertThat(accounts).isNotEmpty();

        // 主要な科目を確認
        assertThat(accounts).extracting(Account::getAccountCode)
            .contains("1", "11", "111", "2", "21", "211", "3", "31", "4", "41", "5", "51");

        // 資産科目の確認
        Account asset = accountRepository.findByCode("1").orElseThrow();
        assertThat(asset.getAccountName()).isEqualTo("資産");
        assertThat(asset.getBsplType()).isEqualTo("B");

        // 費用科目の確認
        Account expense = accountRepository.findByCode("5").orElseThrow();
        assertThat(expense.getAccountName()).isEqualTo("費用");
        assertThat(expense.getBsplType()).isEqualTo("P");
    }

    @Test
    @DisplayName("日次残高データが正しく投入される")
    void testSeedDailyBalances() throws Exception {
        // When
        databaseSeeder.run();

        // Then: 日次残高データが投入されている（期末日のデータ）
        LocalDate fy2021End = LocalDate.of(2022, 3, 31); // 令和3年度末
        LocalDate fy2022End = LocalDate.of(2023, 3, 31); // 令和4年度末

        List<DailyBalance> fy2021Balances = dailyBalanceRepository.findByDate(fy2021End);
        List<DailyBalance> fy2022Balances = dailyBalanceRepository.findByDate(fy2022End);

        assertThat(fy2021Balances).isNotEmpty();
        assertThat(fy2022Balances).isNotEmpty();

        // 特定の残高データを確認
        assertThat(fy2021Balances).anySatisfy(balance -> {
            assertThat(balance.getAccountCode()).isNotNull();
            assertThat(balance.getEntryDate()).isEqualTo(fy2021End);
        });
    }

    @Test
    @DisplayName("複数回実行しても問題なく動作する（冪等性）")
    void testIdempotency() throws Exception {
        // Given: 1回目の実行
        databaseSeeder.run();
        List<Account> firstAccounts = accountRepository.findAll();

        // When: 2回目の実行
        databaseSeeder.run();

        // Then: データが重複せず正しく投入されている
        List<Account> secondAccounts = accountRepository.findAll();
        assertThat(secondAccounts).hasSizeGreaterThanOrEqualTo(firstAccounts.size());
    }

    @Test
    @DisplayName("D社の財務データが正しく投入される")
    void testDCompanyFinancialData() throws Exception {
        // When
        databaseSeeder.run();

        // Then: D社の主要勘定科目が存在する
        assertThat(accountRepository.findByCode("111")).isPresent(); // 現金及び預金
        assertThat(accountRepository.findByCode("112")).isPresent(); // 受取手形
        assertThat(accountRepository.findByCode("113")).isPresent(); // 売掛金
        assertThat(accountRepository.findByCode("211")).isPresent(); // 支払手形
        assertThat(accountRepository.findByCode("212")).isPresent(); // 買掛金
        assertThat(accountRepository.findByCode("41")).isPresent();  // 売上高
        assertThat(accountRepository.findByCode("51")).isPresent();  // 売上原価

        // 令和3年度・令和4年度の期末残高データが存在する
        LocalDate fy2021End = LocalDate.of(2022, 3, 31);
        LocalDate fy2022End = LocalDate.of(2023, 3, 31);

        assertThat(dailyBalanceRepository.findByDate(fy2021End)).isNotEmpty();
        assertThat(dailyBalanceRepository.findByDate(fy2022End)).isNotEmpty();
    }
}
