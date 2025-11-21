package com.example.accounting.mapper;

import com.example.accounting.domain.TaxTransaction;
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
@DisplayName("課税取引マスタ - MyBatis 動的 SQL テスト")
class TaxTransactionMapperTest {

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
    @DisplayName("初期データが投入されている")
    void testInitialData() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            TaxTransactionMapper mapper = session.getMapper(TaxTransactionMapper.class);

            List<TaxTransaction> all = mapper.findAll();
            assertThat(all).hasSize(4);

            TaxTransaction taxable = mapper.findByCode("01");
            assertThat(taxable.getTaxName()).isEqualTo("課税");
            assertThat(taxable.getTaxRate()).isEqualByComparingTo(new BigDecimal("0.100"));
        }
    }

    @Test
    @Order(2)
    @DisplayName("新しい課税取引を登録できる")
    void testInsert() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            TaxTransactionMapper mapper = session.getMapper(TaxTransactionMapper.class);

            TaxTransaction newTax = new TaxTransaction(
                    "05",
                    "軽減税率",
                    new BigDecimal("0.08"),
                    "軽減税率対象取引（食品など）"
            );
            mapper.insert(newTax);
            session.commit();

            TaxTransaction found = mapper.findByCode("05");
            assertThat(found).isNotNull();
            assertThat(found.getTaxName()).isEqualTo("軽減税率");
            assertThat(found.getTaxRate()).isEqualByComparingTo(new BigDecimal("0.08"));

            // クリーンアップ
            mapper.delete("05");
            session.commit();
        }
    }

    @Test
    @Order(3)
    @DisplayName("動的 SQL - 課税取引名で部分一致検索")
    void testSearchByName() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            TaxTransactionMapper mapper = session.getMapper(TaxTransactionMapper.class);

            // "課税"を含む取引を検索（"課税"と"非課税"が該当）
            List<TaxTransaction> results = mapper.search("課税", null, null);
            assertThat(results).hasSizeGreaterThanOrEqualTo(2);
            assertThat(results).extracting(TaxTransaction::getTaxName)
                    .contains("課税", "非課税");
        }
    }

    @Test
    @Order(4)
    @DisplayName("動的 SQL - 最小税率で検索")
    void testSearchByMinTaxRate() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            TaxTransactionMapper mapper = session.getMapper(TaxTransactionMapper.class);

            // 税率が0.05以上の取引を検索
            List<TaxTransaction> results = mapper.search(null, new BigDecimal("0.05"), null);
            assertThat(results).hasSize(1); // "課税"のみ（0.10）
            assertThat(results.get(0).getTaxName()).isEqualTo("課税");
        }
    }

    @Test
    @Order(5)
    @DisplayName("動的 SQL - 有効フラグで検索")
    void testSearchByActiveFlag() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            TaxTransactionMapper mapper = session.getMapper(TaxTransactionMapper.class);

            // まず、1件を無効化
            TaxTransaction tax = mapper.findByCode("04");
            tax.setIsActive(false);
            mapper.update(tax);
            session.commit();

            // 有効な取引のみを検索
            List<TaxTransaction> active = mapper.search(null, null, true);
            assertThat(active).hasSize(3); // "01", "02", "03"のみ

            // 無効な取引のみを検索
            List<TaxTransaction> inactive = mapper.search(null, null, false);
            assertThat(inactive).hasSize(1); // "04"のみ

            // 元に戻す
            tax.setIsActive(true);
            mapper.update(tax);
            session.commit();
        }
    }

    @Test
    @Order(6)
    @DisplayName("動的 SQL - 複数条件の組み合わせ")
    void testSearchWithMultipleConditions() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            TaxTransactionMapper mapper = session.getMapper(TaxTransactionMapper.class);

            // 課税取引名に"税"を含み、税率0以上、有効な取引
            List<TaxTransaction> results = mapper.search(
                    "税",
                    new BigDecimal("0.00"),
                    true
            );

            assertThat(results).hasSizeGreaterThanOrEqualTo(1);
            assertThat(results).allMatch(t -> t.getTaxName().contains("税"));
            assertThat(results).allMatch(t -> t.getTaxRate().compareTo(BigDecimal.ZERO) >= 0);
            assertThat(results).allMatch(TaxTransaction::getIsActive);
        }
    }

    @Test
    @Order(7)
    @DisplayName("課税取引を更新できる")
    void testUpdate() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            TaxTransactionMapper mapper = session.getMapper(TaxTransactionMapper.class);

            TaxTransaction tax = mapper.findByCode("01");
            tax.setTaxRate(new BigDecimal("0.12")); // 税率を12%に変更
            mapper.update(tax);
            session.commit();

            TaxTransaction updated = mapper.findByCode("01");
            assertThat(updated.getTaxRate()).isEqualByComparingTo(new BigDecimal("0.12"));

            // 元に戻す
            tax.setTaxRate(new BigDecimal("0.10"));
            mapper.update(tax);
            session.commit();
        }
    }
}
