package com.example.accounting.mapper;

import com.example.accounting.entity.Account;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.io.InputStream;
import java.math.BigDecimal;
import java.util.List;
import java.util.Properties;

import static org.assertj.core.api.Assertions.assertThat;

@Testcontainers
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("勘定科目マスタ - MyBatis Mapper テスト")
class AccountMapperTest {

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine")
            .withDatabaseName("testdb")
            .withUsername("testuser")
            .withPassword("testpass");

    private static SqlSessionFactory sqlSessionFactory;

    @BeforeAll
    static void setUp() throws Exception {
        // Flyway マイグレーション実行
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
    }

    @Test
    @Order(1)
    @DisplayName("新しい勘定科目を登録できる")
    void testInsert() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountMapper mapper = session.getMapper(AccountMapper.class);

            Account newAccount = new Account(
                    "1101",
                    "現金",
                    "資産",
                    false
            );
            newAccount.setAccountNameKana("ゲンキン");
            newAccount.setBsplType("B");  // CHAR(1): B=貸借対照表, P=損益計算書
            newAccount.setTransactionElementType("1");  // CHAR(1): 1=資産
            newAccount.setDisplayOrder(1);
            newAccount.setTaxCode("04");

            mapper.insert(newAccount);
            session.commit();

            Account found = mapper.findByCode("1101");
            assertThat(found).isNotNull();
            assertThat(found.getAccountName()).isEqualTo("現金");
            assertThat(found.getAccountType()).isEqualTo("資産");
            assertThat(found.getIsSummaryAccount()).isFalse();
            assertThat(found.getBalance()).isEqualByComparingTo(BigDecimal.ZERO);
        }
    }

    @Test
    @Order(2)
    @DisplayName("勘定科目コードで検索できる")
    void testFindByCode() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountMapper mapper = session.getMapper(AccountMapper.class);

            Account account = mapper.findByCode("1101");
            assertThat(account).isNotNull();
            assertThat(account.getAccountName()).isEqualTo("現金");
        }
    }

    @Test
    @Order(3)
    @DisplayName("勘定科目IDで検索できる")
    void testFindById() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountMapper mapper = session.getMapper(AccountMapper.class);

            // まず勘定科目コードで取得してIDを確認
            Account account = mapper.findByCode("1101");
            assertThat(account).isNotNull();
            assertThat(account.getAccountId()).isNotNull();

            // IDで検索
            Account foundById = mapper.findById(account.getAccountId());
            assertThat(foundById).isNotNull();
            assertThat(foundById.getAccountCode()).isEqualTo("1101");
        }
    }

    @Test
    @Order(4)
    @DisplayName("全ての勘定科目を取得できる")
    void testFindAll() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountMapper mapper = session.getMapper(AccountMapper.class);

            // 追加の勘定科目を登録
            Account account2 = new Account("2101", "買掛金", "負債", false);
            account2.setDisplayOrder(2);
            mapper.insert(account2);
            session.commit();

            List<Account> all = mapper.findAll();
            assertThat(all).hasSizeGreaterThanOrEqualTo(2);
        }
    }

    @Test
    @Order(5)
    @DisplayName("勘定科目種別で検索できる")
    void testFindByType() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountMapper mapper = session.getMapper(AccountMapper.class);

            List<Account> assets = mapper.findByType("資産");
            assertThat(assets).hasSizeGreaterThanOrEqualTo(1);
            assertThat(assets).allMatch(a -> a.getAccountType().equals("資産"));
        }
    }

    @Test
    @Order(6)
    @DisplayName("合計科目と明細科目を区別して取得できる")
    void testFindSummaryAndDetailAccounts() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountMapper mapper = session.getMapper(AccountMapper.class);

            // 合計科目を登録
            Account summaryAccount = new Account("11", "流動資産", "資産", true);
            summaryAccount.setDisplayOrder(10);
            mapper.insert(summaryAccount);
            session.commit();

            // 合計科目のみ取得
            List<Account> summaryAccounts = mapper.findSummaryAccounts();
            assertThat(summaryAccounts).hasSizeGreaterThanOrEqualTo(1);
            assertThat(summaryAccounts).allMatch(Account::getIsSummaryAccount);

            // 明細科目のみ取得
            List<Account> detailAccounts = mapper.findDetailAccounts();
            assertThat(detailAccounts).hasSizeGreaterThanOrEqualTo(2);
            assertThat(detailAccounts).allMatch(a -> !a.getIsSummaryAccount());
        }
    }

    @Test
    @Order(7)
    @DisplayName("勘定科目を更新できる")
    void testUpdate() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountMapper mapper = session.getMapper(AccountMapper.class);

            Account account = mapper.findByCode("1101");
            account.setAccountName("現金及び預金");
            account.setBalance(new BigDecimal("100000"));
            mapper.update(account);
            session.commit();

            Account updated = mapper.findByCode("1101");
            assertThat(updated.getAccountName()).isEqualTo("現金及び預金");
            assertThat(updated.getBalance()).isEqualByComparingTo(new BigDecimal("100000"));
        }
    }

    @Test
    @Order(8)
    @DisplayName("勘定科目の残高を更新できる")
    void testUpdateBalance() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountMapper mapper = session.getMapper(AccountMapper.class);

            mapper.updateBalance("1101", new BigDecimal("200000"));
            session.commit();

            Account updated = mapper.findByCode("1101");
            assertThat(updated.getBalance()).isEqualByComparingTo(new BigDecimal("200000"));
        }
    }

    @Test
    @Order(9)
    @DisplayName("勘定科目を削除できる")
    void testDelete() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountMapper mapper = session.getMapper(AccountMapper.class);

            // テスト用の勘定科目を作成
            Account testAccount = new Account("9999", "テスト科目", "資産", false);
            mapper.insert(testAccount);
            session.commit();

            // 削除
            mapper.delete("9999");
            session.commit();

            // 確認
            Account deleted = mapper.findByCode("9999");
            assertThat(deleted).isNull();
        }
    }
}
