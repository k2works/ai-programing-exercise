package com.example.accounting.infrastructure.persistence.mapper;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.infrastructure.persistence.entity.AutoJournalPattern;
import com.example.accounting.infrastructure.persistence.entity.AutoJournalPatternItem;
import org.apache.ibatis.session.SqlSession;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 自動仕訳パターンマッパーのテスト
 */
class AutoJournalPatternMapperTest extends TestDatabaseConfig {

    @BeforeAll
    static void setUp() throws Exception {
        // 親クラスのsetUpDatabaseが自動的に呼ばれる
        // テスト用勘定科目を登録
        insertTestAccounts();
    }

    @AfterEach
    void cleanup() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AutoJournalPatternMapper mapper = session.getMapper(AutoJournalPatternMapper.class);
            List<AutoJournalPattern> patterns = mapper.findAll();
            for (AutoJournalPattern pattern : patterns) {
                mapper.delete(pattern.getId());
            }
            session.commit();
        }
    }

    @Test
    @DisplayName("パターンを登録し、IDで取得できる")
    void testInsertAndFindById() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AutoJournalPatternMapper mapper = session.getMapper(AutoJournalPatternMapper.class);

            // Given: パターンを挿入
            AutoJournalPattern pattern = new AutoJournalPattern();
            pattern.setPatternCode("SALES_001");
            pattern.setPatternName("売上仕訳自動生成");
            pattern.setSourceTableName("売上テーブル");
            pattern.setDescription("売上データから仕訳を自動生成");
            pattern.setIsActive(true);

            mapper.insert(pattern);
            session.commit();

            // When: IDで取得
            AutoJournalPattern found = mapper.findById(pattern.getId());

            // Then: 正しく取得できる
            assertThat(found).isNotNull();
            assertThat(found.getPatternCode()).isEqualTo("SALES_001");
            assertThat(found.getPatternName()).isEqualTo("売上仕訳自動生成");
            assertThat(found.getIsActive()).isTrue();
        }
    }

    @Test
    @DisplayName("パターンと明細を登録し、階層構造で取得できる")
    void testInsertPatternWithItems() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AutoJournalPatternMapper patternMapper = session.getMapper(AutoJournalPatternMapper.class);
            AutoJournalPatternItemMapper itemMapper = session.getMapper(AutoJournalPatternItemMapper.class);

            // Given: パターンを挿入
            AutoJournalPattern pattern = new AutoJournalPattern();
            pattern.setPatternCode("SALES_002");
            pattern.setPatternName("売上仕訳（税込）");
            pattern.setSourceTableName("売上テーブル");
            pattern.setIsActive(true);

            patternMapper.insert(pattern);
            session.commit();

            // パターン明細を挿入（借方：売掛金、貸方：売上）
            AutoJournalPatternItem debitItem = new AutoJournalPatternItem();
            debitItem.setPatternId(pattern.getId());
            debitItem.setLineNumber(1);
            debitItem.setDebitCreditFlag("D");
            debitItem.setAccountCode("1300");
            debitItem.setAmountExpression("amount");
            debitItem.setDescriptionTemplate("売掛金計上");

            itemMapper.insert(debitItem);

            AutoJournalPatternItem creditItem = new AutoJournalPatternItem();
            creditItem.setPatternId(pattern.getId());
            creditItem.setLineNumber(2);
            creditItem.setDebitCreditFlag("C");
            creditItem.setAccountCode("4100");
            creditItem.setAmountExpression("amount");
            creditItem.setDescriptionTemplate("売上計上");

            itemMapper.insert(creditItem);
            session.commit();

            // When: パターンを取得（明細も一緒に取得される）
            AutoJournalPattern found = patternMapper.findById(pattern.getId());

            // Then: パターンと明細が正しく取得できる
            assertThat(found).isNotNull();
            assertThat(found.getItems()).hasSize(2);

            AutoJournalPatternItem item1 = found.getItems().get(0);
            assertThat(item1.getLineNumber()).isEqualTo(1);
            assertThat(item1.getDebitCreditFlag()).isEqualTo("D");
            assertThat(item1.getAccountCode()).isEqualTo("1300");

            AutoJournalPatternItem item2 = found.getItems().get(1);
            assertThat(item2.getLineNumber()).isEqualTo(2);
            assertThat(item2.getDebitCreditFlag()).isEqualTo("C");
            assertThat(item2.getAccountCode()).isEqualTo("4100");
        }
    }

    @Test
    @DisplayName("パターンコードで検索できる")
    void testFindByPatternCode() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AutoJournalPatternMapper mapper = session.getMapper(AutoJournalPatternMapper.class);

            // Given: パターンを挿入
            AutoJournalPattern pattern = new AutoJournalPattern();
            pattern.setPatternCode("PURCHASE_001");
            pattern.setPatternName("仕入仕訳自動生成");
            pattern.setSourceTableName("仕入テーブル");
            pattern.setIsActive(true);

            mapper.insert(pattern);
            session.commit();

            // When: パターンコードで検索
            AutoJournalPattern found = mapper.findByPatternCode("PURCHASE_001");

            // Then: 正しく取得できる
            assertThat(found).isNotNull();
            assertThat(found.getPatternName()).isEqualTo("仕入仕訳自動生成");
        }
    }

    @Test
    @DisplayName("有効なパターンのみ取得できる")
    void testFindActivePatterns() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AutoJournalPatternMapper mapper = session.getMapper(AutoJournalPatternMapper.class);

            // Given: 有効なパターンと無効なパターンを挿入
            AutoJournalPattern activePattern = new AutoJournalPattern();
            activePattern.setPatternCode("ACTIVE_001");
            activePattern.setPatternName("有効パターン");
            activePattern.setSourceTableName("テーブル1");
            activePattern.setIsActive(true);

            mapper.insert(activePattern);

            AutoJournalPattern inactivePattern = new AutoJournalPattern();
            inactivePattern.setPatternCode("INACTIVE_001");
            inactivePattern.setPatternName("無効パターン");
            inactivePattern.setSourceTableName("テーブル2");
            inactivePattern.setIsActive(false);

            mapper.insert(inactivePattern);
            session.commit();

            // When: 有効なパターンを取得
            List<AutoJournalPattern> activePatterns = mapper.findActivePatterns();

            // Then: 有効なパターンのみ取得される
            assertThat(activePatterns).hasSize(1);
            assertThat(activePatterns.get(0).getPatternCode()).isEqualTo("ACTIVE_001");
        }
    }

    @Test
    @DisplayName("パターンを更新できる")
    void testUpdatePattern() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AutoJournalPatternMapper mapper = session.getMapper(AutoJournalPatternMapper.class);

            // Given: パターンを挿入
            AutoJournalPattern pattern = new AutoJournalPattern();
            pattern.setPatternCode("UPDATE_TEST");
            pattern.setPatternName("更新テスト");
            pattern.setSourceTableName("テストテーブル");
            pattern.setIsActive(true);

            mapper.insert(pattern);
            session.commit();

            // When: パターンを更新
            pattern.setPatternName("更新後の名前");
            pattern.setIsActive(false);
            mapper.update(pattern);
            session.commit();

            // Then: 更新内容が反映される
            AutoJournalPattern found = mapper.findById(pattern.getId());
            assertThat(found.getPatternName()).isEqualTo("更新後の名前");
            assertThat(found.getIsActive()).isFalse();
        }
    }

    @Test
    @DisplayName("パターン削除時に明細も削除される（CASCADE）")
    void testDeletePatternCascade() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AutoJournalPatternMapper patternMapper = session.getMapper(AutoJournalPatternMapper.class);
            AutoJournalPatternItemMapper itemMapper = session.getMapper(AutoJournalPatternItemMapper.class);

            // Given: パターンと明細を挿入
            AutoJournalPattern pattern = new AutoJournalPattern();
            pattern.setPatternCode("DELETE_TEST");
            pattern.setPatternName("削除テスト");
            pattern.setSourceTableName("テストテーブル");
            pattern.setIsActive(true);

            patternMapper.insert(pattern);
            session.commit();

            AutoJournalPatternItem item = new AutoJournalPatternItem();
            item.setPatternId(pattern.getId());
            item.setLineNumber(1);
            item.setDebitCreditFlag("D");
            item.setAccountCode("1100");
            item.setAmountExpression("amount");

            itemMapper.insert(item);
            session.commit();

            // When: パターンを削除
            patternMapper.delete(pattern.getId());
            session.commit();

            // Then: パターンと明細が削除される
            AutoJournalPattern foundPattern = patternMapper.findById(pattern.getId());
            assertThat(foundPattern).isNull();

            List<AutoJournalPatternItem> foundItems = itemMapper.findByPatternId(pattern.getId());
            assertThat(foundItems).isEmpty();
        }
    }

    /**
     * テスト用勘定科目を登録
     */
    private static void insertTestAccounts() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            session.getConnection().createStatement().executeUpdate("""
                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
                VALUES
                ('1100', '現金', '資産'::account_type, false, true, 0),
                ('1300', '売掛金', '資産'::account_type, false, true, 0),
                ('4100', '売上', '収益'::account_type, false, true, 0)
                """);
            session.commit();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
