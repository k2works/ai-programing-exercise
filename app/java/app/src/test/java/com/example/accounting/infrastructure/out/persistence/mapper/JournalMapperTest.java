package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.infrastructure.out.persistence.entity.Journal;
import com.example.accounting.infrastructure.out.persistence.entity.JournalDetail;
import com.example.accounting.infrastructure.out.persistence.entity.JournalDetailItem;
import org.apache.ibatis.session.SqlSession;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("仕訳 Mapper - MyBatis テスト")
class JournalMapperTest extends TestDatabaseConfig {

    @BeforeAll
    static void setUp() throws Exception {
        // テスト用勘定科目を登録
        try (SqlSession session = sqlSessionFactory.openSession()) {
            session.getConnection().createStatement().executeUpdate("""
                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
                VALUES
                ('1100', '現金', '資産'::account_type, false, true, 0),
                ('1200', '普通預金', '資産'::account_type, false, true, 0),
                ('1300', '売掛金', '資産'::account_type, false, true, 0),
                ('5110', '仕入', '費用'::account_type, false, true, 0),
                ('6200', '支払手数料', '費用'::account_type, false, true, 0)
                """);
            session.commit();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @AfterEach
    void cleanup() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            JournalMapper mapper = session.getMapper(JournalMapper.class);
            session.getConnection().createStatement().execute("TRUNCATE TABLE \"仕訳\" CASCADE");
            session.commit();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    @Order(1)
    @DisplayName("単純な仕訳を登録できる")
    void testInsertSimpleJournal() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            JournalMapper mapper = session.getMapper(JournalMapper.class);

            // 仕訳ヘッダー作成
            Journal journal = new Journal("JE-20250101-001",
                    LocalDate.of(2025, 1, 1),
                    LocalDate.of(2025, 1, 1));
            journal.setSingleEntryFlag(1);
            mapper.insertJournal(journal);

            // 仕訳明細作成
            JournalDetail detail = new JournalDetail("JE-20250101-001", 1, "商品仕入");
            mapper.insertJournalDetail(detail);

            // 仕訳貸借明細作成（借方：仕入、貸方：現金）
            JournalDetailItem debitItem = new JournalDetailItem("JE-20250101-001", 1, "D",
                    "5110", new BigDecimal("100000.00"));
            mapper.insertJournalDetailItem(debitItem);

            JournalDetailItem creditItem = new JournalDetailItem("JE-20250101-001", 1, "C",
                    "1100", new BigDecimal("100000.00"));
            mapper.insertJournalDetailItem(creditItem);

            session.commit();

            // 検証
            Journal found = mapper.findByJournalNo("JE-20250101-001");
            assertThat(found).isNotNull();
            assertThat(found.getJournalNo()).isEqualTo("JE-20250101-001");
            assertThat(found.getSingleEntryFlag()).isEqualTo(1);
            assertThat(found.getDetails()).hasSize(1);

            JournalDetail foundDetail = found.getDetails().get(0);
            assertThat(foundDetail.getDescription()).isEqualTo("商品仕入");
            assertThat(foundDetail.getItems()).hasSize(2);

            // 借方・貸方の検証
            JournalDetailItem foundDebit = foundDetail.getItems().stream()
                    .filter(item -> "D".equals(item.getDebitCreditFlag()))
                    .findFirst()
                    .orElseThrow();
            assertThat(foundDebit.getAccountCode()).isEqualTo("5110");
            assertThat(foundDebit.getAmount()).isEqualByComparingTo(new BigDecimal("100000.00"));

            JournalDetailItem foundCredit = foundDetail.getItems().stream()
                    .filter(item -> "C".equals(item.getDebitCreditFlag()))
                    .findFirst()
                    .orElseThrow();
            assertThat(foundCredit.getAccountCode()).isEqualTo("1100");
            assertThat(foundCredit.getAmount()).isEqualByComparingTo(new BigDecimal("100000.00"));
        }
    }

    @Test
    @Order(2)
    @DisplayName("複合仕訳を登録できる")
    void testInsertComplexJournal() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            JournalMapper mapper = session.getMapper(JournalMapper.class);

            // 仕訳ヘッダー作成
            Journal journal = new Journal("JE-20250102-001",
                    LocalDate.of(2025, 1, 2),
                    LocalDate.of(2025, 1, 2));
            journal.setSingleEntryFlag(0);  // 複合仕訳
            mapper.insertJournal(journal);

            // 仕訳明細1: 売掛金回収
            JournalDetail detail1 = new JournalDetail("JE-20250102-001", 1, "売掛金回収（A社）");
            mapper.insertJournalDetail(detail1);

            JournalDetailItem item1Debit = new JournalDetailItem("JE-20250102-001", 1, "D",
                    "1200", new BigDecimal("99560.00"));
            item1Debit.setCashFlowFlag(1);
            mapper.insertJournalDetailItem(item1Debit);

            JournalDetailItem item1Credit = new JournalDetailItem("JE-20250102-001", 1, "C",
                    "1300", new BigDecimal("99560.00"));
            mapper.insertJournalDetailItem(item1Credit);

            // 仕訳明細2: 振込手数料
            JournalDetail detail2 = new JournalDetail("JE-20250102-001", 2, "振込手数料");
            mapper.insertJournalDetail(detail2);

            JournalDetailItem item2Debit = new JournalDetailItem("JE-20250102-001", 2, "D",
                    "6200", new BigDecimal("440.00"));
            mapper.insertJournalDetailItem(item2Debit);

            JournalDetailItem item2Credit = new JournalDetailItem("JE-20250102-001", 2, "C",
                    "1300", new BigDecimal("440.00"));
            mapper.insertJournalDetailItem(item2Credit);

            session.commit();

            // 検証
            Journal found = mapper.findByJournalNo("JE-20250102-001");
            assertThat(found).isNotNull();
            assertThat(found.getSingleEntryFlag()).isEqualTo(0);
            assertThat(found.getDetails()).hasSize(2);

            // 明細1の検証
            JournalDetail foundDetail1 = found.getDetails().get(0);
            assertThat(foundDetail1.getDescription()).isEqualTo("売掛金回収（A社）");
            assertThat(foundDetail1.getItems()).hasSize(2);

            // 明細2の検証
            JournalDetail foundDetail2 = found.getDetails().get(1);
            assertThat(foundDetail2.getDescription()).isEqualTo("振込手数料");
            assertThat(foundDetail2.getItems()).hasSize(2);

            // 全体の借方・貸方合計を検証
            BigDecimal totalDebit = found.getDetails().stream()
                    .flatMap(d -> d.getItems().stream())
                    .filter(item -> "D".equals(item.getDebitCreditFlag()))
                    .map(JournalDetailItem::getAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);

            BigDecimal totalCredit = found.getDetails().stream()
                    .flatMap(d -> d.getItems().stream())
                    .filter(item -> "C".equals(item.getDebitCreditFlag()))
                    .map(JournalDetailItem::getAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);

            assertThat(totalDebit).isEqualByComparingTo(totalCredit);
            assertThat(totalDebit).isEqualByComparingTo(new BigDecimal("100000.00"));
        }
    }

    @Test
    @Order(3)
    @DisplayName("仕訳を削除できる（CASCADE により明細も削除）")
    void testDeleteJournal() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            JournalMapper mapper = session.getMapper(JournalMapper.class);

            // 仕訳を登録
            Journal journal = new Journal("JE-20250103-001",
                    LocalDate.of(2025, 1, 3),
                    LocalDate.of(2025, 1, 3));
            mapper.insertJournal(journal);

            JournalDetail detail = new JournalDetail("JE-20250103-001", 1, "テスト");
            mapper.insertJournalDetail(detail);

            JournalDetailItem item = new JournalDetailItem("JE-20250103-001", 1, "D",
                    "1100", new BigDecimal("10000.00"));
            mapper.insertJournalDetailItem(item);

            session.commit();

            // 仕訳を削除
            mapper.deleteByJournalNo("JE-20250103-001");
            session.commit();

            // 削除されたことを確認
            Journal found = mapper.findByJournalNo("JE-20250103-001");
            assertThat(found).isNull();
        }
    }

    @Test
    @Order(4)
    @DisplayName("全仕訳を取得できる")
    void testFindAll() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            JournalMapper mapper = session.getMapper(JournalMapper.class);

            // 2件登録
            Journal journal1 = new Journal("JE-20250104-001",
                    LocalDate.of(2025, 1, 4),
                    LocalDate.of(2025, 1, 4));
            mapper.insertJournal(journal1);

            Journal journal2 = new Journal("JE-20250104-002",
                    LocalDate.of(2025, 1, 5),
                    LocalDate.of(2025, 1, 5));
            mapper.insertJournal(journal2);

            session.commit();

            // 全件取得
            var journals = mapper.findAll();
            assertThat(journals).hasSize(2);
        }
    }
}
