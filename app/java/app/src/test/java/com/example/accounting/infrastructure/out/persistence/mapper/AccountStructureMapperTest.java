package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.TestDatabaseConfig;
import com.example.accounting.infrastructure.out.persistence.entity.AccountStructure;
import org.apache.ibatis.session.SqlSession;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("勘定科目構成マスタ - MyBatis 統合テスト")
class AccountStructureMapperTest extends TestDatabaseConfig {

    @BeforeAll
    static void setUp() throws Exception {
        // 勘定科目マスタにテストデータを投入
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {

            conn.createStatement().executeUpdate("""
                INSERT INTO "勘定科目マスタ" (
                    "勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高"
                ) VALUES
                    ('11', '資産の部', '資産', true, true, 0),
                    ('11000', '流動資産', '資産', true, true, 0),
                    ('11190', '現金及び預金', '資産', true, true, 0),
                    ('11110', '現金', '資産', false, true, 100000),
                    ('11120', '当座預金', '資産', false, true, 500000),
                    ('11130', '普通預金', '資産', false, true, 1000000)
                ON CONFLICT ("勘定科目コード") DO NOTHING
            """);
        }
    }

    @BeforeEach
    void cleanUp() throws SQLException {
        try (Connection conn = DriverManager.getConnection(
                POSTGRES.getJdbcUrl(),
                POSTGRES.getUsername(),
                POSTGRES.getPassword())) {
            conn.createStatement().executeUpdate("DELETE FROM \"勘定科目構成マスタ\"");
        }
    }

    @Test
    @Order(1)
    @DisplayName("勘定科目構成を登録できる")
    void testInsert() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountStructureMapper mapper = session.getMapper(AccountStructureMapper.class);

            // 階層構造を登録
            AccountStructure root = new AccountStructure("11", "11", 1, null, 1);
            mapper.insert(root);

            AccountStructure level2 = new AccountStructure("11000", "11~11000", 2, "11", 1);
            mapper.insert(level2);

            AccountStructure level3 = new AccountStructure("11190", "11~11000~11190", 3, "11000", 1);
            mapper.insert(level3);

            session.commit();

            // 検証
            AccountStructure found = mapper.findByCode("11190");
            assertThat(found).isNotNull();
            assertThat(found.getAccountPath()).isEqualTo("11~11000~11190");
            assertThat(found.getHierarchyLevel()).isEqualTo(3);
            assertThat(found.getParentAccountCode()).isEqualTo("11000");
        }
    }

    @Test
    @Order(2)
    @DisplayName("全ての勘定科目構成を取得できる")
    void testFindAll() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountStructureMapper mapper = session.getMapper(AccountStructureMapper.class);

            // テストデータ登録
            mapper.insert(new AccountStructure("11", "11", 1, null, 1));
            mapper.insert(new AccountStructure("11000", "11~11000", 2, "11", 1));
            mapper.insert(new AccountStructure("11190", "11~11000~11190", 3, "11000", 1));
            session.commit();

            // 全件取得
            List<AccountStructure> all = mapper.findAll();
            assertThat(all).hasSize(3);
            assertThat(all).extracting(AccountStructure::getAccountCode)
                    .containsExactly("11", "11000", "11190");
        }
    }

    @Test
    @Order(3)
    @DisplayName("特定科目配下の子孫を取得できる（チルダ連結検索）")
    void testFindChildren() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountStructureMapper mapper = session.getMapper(AccountStructureMapper.class);

            // 階層データ登録
            mapper.insert(new AccountStructure("11", "11", 1, null, 1));
            mapper.insert(new AccountStructure("11000", "11~11000", 2, "11", 1));
            mapper.insert(new AccountStructure("11190", "11~11000~11190", 3, "11000", 1));
            mapper.insert(new AccountStructure("11110", "11~11000~11190~11110", 4, "11190", 1));
            mapper.insert(new AccountStructure("11120", "11~11000~11190~11120", 4, "11190", 2));
            mapper.insert(new AccountStructure("11130", "11~11000~11190~11130", 4, "11190", 3));
            session.commit();

            // 「現金及び預金」（11190）配下を検索
            List<AccountStructure> children = mapper.findChildren("11190");

            // 自身 + 子孫の4件が取得される
            assertThat(children).hasSize(4);
            assertThat(children).extracting(AccountStructure::getAccountCode)
                    .containsExactlyInAnyOrder("11190", "11110", "11120", "11130");
        }
    }

    @Test
    @Order(4)
    @DisplayName("特定階層レベルの科目を取得できる")
    void testFindByLevel() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountStructureMapper mapper = session.getMapper(AccountStructureMapper.class);

            mapper.insert(new AccountStructure("11", "11", 1, null, 1));
            mapper.insert(new AccountStructure("11000", "11~11000", 2, "11", 1));
            mapper.insert(new AccountStructure("11190", "11~11000~11190", 3, "11000", 1));
            mapper.insert(new AccountStructure("11110", "11~11000~11190~11110", 4, "11190", 1));
            session.commit();

            // 階層レベル4の科目を検索
            List<AccountStructure> level4 = mapper.findByLevel(4);
            assertThat(level4).hasSize(1);
            assertThat(level4.get(0).getAccountCode()).isEqualTo("11110");
        }
    }

    @Test
    @Order(5)
    @DisplayName("勘定科目構成を更新できる")
    void testUpdate() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountStructureMapper mapper = session.getMapper(AccountStructureMapper.class);

            // 初期データ
            AccountStructure structure = new AccountStructure("11110", "11~11000~11190~11110", 4, "11190", 1);
            mapper.insert(structure);
            session.commit();

            // 更新（表示順序を変更）
            structure.setDisplayOrder(99);
            mapper.update(structure);
            session.commit();

            // 検証
            AccountStructure updated = mapper.findByCode("11110");
            assertThat(updated.getDisplayOrder()).isEqualTo(99);
        }
    }

    @Test
    @Order(6)
    @DisplayName("勘定科目構成を削除できる")
    void testDelete() {
        try (SqlSession session = sqlSessionFactory.openSession()) {
            AccountStructureMapper mapper = session.getMapper(AccountStructureMapper.class);

            mapper.insert(new AccountStructure("11110", "11~11000~11190~11110", 4, "11190", 1));
            session.commit();

            mapper.delete("11110");
            session.commit();

            AccountStructure deleted = mapper.findByCode("11110");
            assertThat(deleted).isNull();
        }
    }
}
