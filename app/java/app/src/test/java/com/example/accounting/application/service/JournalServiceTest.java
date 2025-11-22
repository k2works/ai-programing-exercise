package com.example.accounting.application.service;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.application.exception.JournalNotFoundException;
import com.example.accounting.application.port.out.JournalRepository;
import com.example.accounting.application.service.finacial.JournalService;
import com.example.accounting.domain.model.financial.Journal;
import com.example.accounting.domain.model.financial.JournalEntry;
import com.example.accounting.domain.model.financial.JournalLine;
import com.example.accounting.infrastructure.out.persistence.adapter.JournalAdapter;
import com.example.accounting.infrastructure.out.persistence.mapper.JournalMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 仕訳サービスの統合テスト
 */
@DisplayName("仕訳サービス - 統合テスト")
class JournalServiceTest extends TestDatabaseConfig {

    private JournalService journalService;
    private JournalMapper mapper;

    @BeforeAll
    static void setUpOnce() throws Exception {
        // テスト用勘定科目を登録
        insertTestAccounts();
    }

    /**
     * テスト用勘定科目を登録
     */
    private static void insertTestAccounts() {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().executeUpdate("""
                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
                VALUES
                ('1010', '現金', '資産'::account_type, false, true, 0),
                ('1020', '普通預金', '資産'::account_type, false, true, 0),
                ('2010', '買掛金', '負債'::account_type, false, true, 0),
                ('4010', '売上高', '収益'::account_type, false, true, 0),
                ('5010', '仕入高', '費用'::account_type, false, true, 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING
                """);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUpEach() {
        // MyBatis Mapper の取得
        mapper = sqlSessionFactory.openSession(true).getMapper(JournalMapper.class);

        // Repository と Service の初期化
        JournalRepository repository = new JournalAdapter(mapper);
        journalService = new JournalService(repository);
    }

    @AfterEach
    void cleanup() throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().execute("DELETE FROM \"仕訳貸借明細\"");
            conn.createStatement().execute("DELETE FROM \"仕訳明細\"");
            conn.createStatement().execute("DELETE FROM \"仕訳\"");
        }
    }

    @Test
    @DisplayName("新規仕訳を作成できる")
    void testCreateJournal() {
        // Given: 新規仕訳（現金仕入）
        Journal journal = createSimpleJournal("J_CREATE_001", LocalDate.of(2025, 1, 1),
                "5010", new BigDecimal("10000"),  // 借方：仕入高
                "1010", new BigDecimal("10000")); // 貸方：現金

        // When: 仕訳を作成
        Journal created = journalService.createJournal(journal);

        // Then: 正しく作成されている
        assertThat(created).isNotNull();
        assertThat(created.getJournalNo()).isEqualTo("J_CREATE_001");
        // 各JournalLineは別々の明細行として保存される
        assertThat(created.getEntries()).hasSize(2);
        assertThat(created.getEntries().get(0).getLines()).hasSize(1);
        assertThat(created.getEntries().get(1).getLines()).hasSize(1);
    }

    @Test
    @DisplayName("重複する仕訳番号で作成するとエラー")
    void testCreateJournalWithDuplicateNo() {
        // Given: 既存の仕訳
        Journal journal = createSimpleJournal("J_DUP_001", LocalDate.of(2025, 1, 1),
                "5010", new BigDecimal("10000"),
                "1010", new BigDecimal("10000"));
        journalService.createJournal(journal);

        // When/Then: 同じ仕訳番号で作成すると例外
        Journal duplicate = createSimpleJournal("J_DUP_001", LocalDate.of(2025, 1, 2),
                "5010", new BigDecimal("5000"),
                "1010", new BigDecimal("5000"));
        assertThatThrownBy(() -> journalService.createJournal(duplicate))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("既に存在します");
    }

    @Test
    @DisplayName("全仕訳を取得できる")
    void testGetAllJournals() {
        // Given: 複数の仕訳
        journalService.createJournal(createSimpleJournal("J_ALL_001", LocalDate.of(2025, 1, 1),
                "5010", new BigDecimal("10000"), "1010", new BigDecimal("10000")));
        journalService.createJournal(createSimpleJournal("J_ALL_002", LocalDate.of(2025, 1, 2),
                "5010", new BigDecimal("20000"), "1020", new BigDecimal("20000")));

        // When: 全仕訳を取得
        List<Journal> journals = journalService.getAllJournals();

        // Then: 2件取得できる
        assertThat(journals).hasSize(2);
        assertThat(journals)
                .extracting(Journal::getJournalNo)
                .containsExactlyInAnyOrder("J_ALL_001", "J_ALL_002");
    }

    @Test
    @DisplayName("仕訳番号で仕訳を取得できる")
    void testGetJournalByNo() {
        // Given: 仕訳
        journalService.createJournal(createSimpleJournal("J_GET_001", LocalDate.of(2025, 1, 1),
                "5010", new BigDecimal("10000"), "1010", new BigDecimal("10000")));

        // When: 仕訳番号で取得
        Journal journal = journalService.getJournalByNo("J_GET_001");

        // Then: 正しく取得できる
        assertThat(journal).isNotNull();
        assertThat(journal.getJournalNo()).isEqualTo("J_GET_001");
        assertThat(journal.getEntries()).isNotEmpty();
    }

    @Test
    @DisplayName("存在しない仕訳番号で取得するとエラー")
    void testGetJournalByNoNotFound() {
        // When/Then: 存在しない仕訳番号で取得すると例外
        assertThatThrownBy(() -> journalService.getJournalByNo("J_NOT_FOUND"))
                .isInstanceOf(JournalNotFoundException.class)
                .hasMessageContaining("が見つかりません");
    }

    @Test
    @DisplayName("仕訳を更新できる")
    void testUpdateJournal() {
        // Given: 既存の仕訳
        Journal original = createSimpleJournal("J_UPD_001", LocalDate.of(2025, 1, 1),
                "5010", new BigDecimal("10000"), "1010", new BigDecimal("10000"));
        journalService.createJournal(original);

        // When: 金額を変更
        Journal updated = createSimpleJournal("J_UPD_001", LocalDate.of(2025, 1, 1),
                "5010", new BigDecimal("15000"), "1010", new BigDecimal("15000"));
        Journal result = journalService.updateJournal("J_UPD_001", updated);

        // Then: 更新されている
        assertThat(result).isNotNull();
        Journal fetched = journalService.getJournalByNo("J_UPD_001");
        assertThat(fetched.getEntries().get(0).getLines().get(0).getAmount())
                .isEqualByComparingTo(new BigDecimal("15000"));
    }

    @Test
    @DisplayName("仕訳番号を変更しようとするとエラー")
    void testUpdateJournalNoChange() {
        // Given: 既存の仕訳
        Journal original = createSimpleJournal("J_CHG_001", LocalDate.of(2025, 1, 1),
                "5010", new BigDecimal("10000"), "1010", new BigDecimal("10000"));
        journalService.createJournal(original);

        // When/Then: 仕訳番号を変更しようとすると例外
        Journal updated = createSimpleJournal("J_CHG_002", LocalDate.of(2025, 1, 1),
                "5010", new BigDecimal("10000"), "1010", new BigDecimal("10000"));
        assertThatThrownBy(() -> journalService.updateJournal("J_CHG_001", updated))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("変更できません");
    }

    @Test
    @DisplayName("仕訳を削除できる")
    void testDeleteJournal() {
        // Given: 既存の仕訳
        Journal journal = createSimpleJournal("J_DEL_001", LocalDate.of(2025, 1, 1),
                "5010", new BigDecimal("10000"), "1010", new BigDecimal("10000"));
        journalService.createJournal(journal);

        // When: 仕訳を削除
        journalService.deleteJournal("J_DEL_001");

        // Then: 削除されている
        assertThatThrownBy(() -> journalService.getJournalByNo("J_DEL_001"))
                .isInstanceOf(JournalNotFoundException.class);
    }

    @Test
    @DisplayName("存在しない仕訳を削除しようとするとエラー")
    void testDeleteJournalNotFound() {
        // When/Then: 存在しない仕訳を削除しようとすると例外
        assertThatThrownBy(() -> journalService.deleteJournal("J_DEL_NOT_FOUND"))
                .isInstanceOf(JournalNotFoundException.class)
                .hasMessageContaining("が見つかりません");
    }

    @Test
    @DisplayName("複合仕訳を作成できる")
    void testCreateComplexJournal() {
        // Given: 複合仕訳（現金・預金で仕入）
        Journal journal = new Journal();
        journal.setJournalNo("J_COMPLEX_001");
        journal.setJournalDate(LocalDate.of(2025, 1, 1));
        journal.setInputDate(LocalDate.of(2025, 1, 1));
        journal.setSettlementFlag(false);
        journal.setSingleEntryFlag(false);
        journal.setJournalType(1);
        journal.setRecurringFlag(false);

        JournalEntry entry = new JournalEntry();
        entry.setLineNumber(1);
        entry.setDescription("商品仕入");
        entry.setLines(new ArrayList<>());

        // 借方：仕入高 30,000
        entry.addLine(createJournalLine("D", "5010", new BigDecimal("30000")));
        // 貸方：現金 10,000
        entry.addLine(createJournalLine("C", "1010", new BigDecimal("10000")));
        // 貸方：普通預金 20,000
        entry.addLine(createJournalLine("C", "1020", new BigDecimal("20000")));

        journal.setEntries(new ArrayList<>());
        journal.addEntry(entry);

        // When: 複合仕訳を作成
        Journal created = journalService.createJournal(journal);

        // Then: 正しく作成されている（各JournalLineは別々の明細行として保存される）
        assertThat(created.getEntries()).hasSize(3);

        // 全明細行から借方合計と貸方合計を計算
        BigDecimal debitTotal = created.getEntries().stream()
                .flatMap(e -> e.getLines().stream())
                .filter(JournalLine::isDebit)
                .map(JournalLine::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        BigDecimal creditTotal = created.getEntries().stream()
                .flatMap(e -> e.getLines().stream())
                .filter(JournalLine::isCredit)
                .map(JournalLine::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        assertThat(debitTotal).isEqualByComparingTo(creditTotal);
    }

    /**
     * 単純仕訳作成ヘルパー（借方1件、貸方1件）
     */
    private Journal createSimpleJournal(String journalNo, LocalDate date,
                                        String debitAccount, BigDecimal debitAmount,
                                        String creditAccount, BigDecimal creditAmount) {
        Journal journal = new Journal();
        journal.setJournalNo(journalNo);
        journal.setJournalDate(date);
        journal.setInputDate(date);
        journal.setSettlementFlag(false);
        journal.setSingleEntryFlag(false);
        journal.setJournalType(1);
        journal.setRecurringFlag(false);

        JournalEntry entry = new JournalEntry();
        entry.setLineNumber(1);
        entry.setDescription("テスト仕訳");
        entry.setLines(new ArrayList<>());

        // 借方
        entry.addLine(createJournalLine("D", debitAccount, debitAmount));
        // 貸方
        entry.addLine(createJournalLine("C", creditAccount, creditAmount));

        journal.setEntries(new ArrayList<>());
        journal.addEntry(entry);

        return journal;
    }

    /**
     * 仕訳明細項目作成ヘルパー
     */
    private JournalLine createJournalLine(String debitCreditFlag, String accountCode, BigDecimal amount) {
        JournalLine line = new JournalLine();
        line.setDebitCreditFlag(debitCreditFlag);
        line.setAccountCode(accountCode);
        line.setAmount(amount);
        line.setBaseAmount(amount);
        line.setCurrencyCode("JPY");
        line.setExchangeRate(BigDecimal.ONE);
        return line;
    }
}
