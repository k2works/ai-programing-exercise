package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.CompanyGroup;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 取引先グループマスタMapperのテストクラス
 */
class CompanyGroupMapperTest extends AbstractDatabaseTest {

    @Autowired
    private CompanyGroupMapper companyGroupMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        // テストデータをクリア（外部キー制約により、参照元を先に削除）
        // 第7章のテーブル
        jdbcTemplate.execute("DELETE FROM 与信残高データ");
        jdbcTemplate.execute("DELETE FROM 自動採番マスタ");

        // 第6章のテーブル
        jdbcTemplate.execute("DELETE FROM 請求データ明細");
        jdbcTemplate.execute("DELETE FROM 請求データ");
        jdbcTemplate.execute("DELETE FROM 入金データ");
        jdbcTemplate.execute("DELETE FROM 支払データ");
        jdbcTemplate.execute("DELETE FROM 入金口座マスタ");

        // 第5章のテーブル
        jdbcTemplate.execute("DELETE FROM 在庫データ");
        jdbcTemplate.execute("DELETE FROM 仕入データ明細");
        jdbcTemplate.execute("DELETE FROM 仕入データ");
        jdbcTemplate.execute("DELETE FROM 発注データ明細");
        jdbcTemplate.execute("DELETE FROM 発注データ");

        // 第4章のテーブル
        jdbcTemplate.execute("DELETE FROM 売上データ明細");
        jdbcTemplate.execute("DELETE FROM 売上データ");
        jdbcTemplate.execute("DELETE FROM 受注データ明細");
        jdbcTemplate.execute("DELETE FROM 受注データ");

        // 第3章以前のテーブル
        jdbcTemplate.execute("DELETE FROM 仕入先マスタ");
        jdbcTemplate.execute("DELETE FROM 顧客マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類所属マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先グループマスタ");
    }

    @Test
    void 取引先グループを登録できる() {
        // Given
        CompanyGroup companyGroup = createTestCompanyGroup("0001");

        // When
        int result = companyGroupMapper.insert(companyGroup);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CompanyGroup> saved = companyGroupMapper.findById("0001");
        assertThat(saved).isPresent();
        assertThat(saved.get().getCompanyGroupCode()).isEqualTo("0001");
        assertThat(saved.get().getCompanyGroupName()).isEqualTo("テストグループ");
    }

    @Test
    void 取引先グループを更新できる() {
        // Given
        CompanyGroup companyGroup = createTestCompanyGroup("0001");
        companyGroupMapper.insert(companyGroup);

        // When
        companyGroup.setCompanyGroupName("更新後グループ");
        companyGroup.setUpdatedBy("updater");
        int result = companyGroupMapper.update(companyGroup);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CompanyGroup> updated = companyGroupMapper.findById("0001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getCompanyGroupName()).isEqualTo("更新後グループ");
        assertThat(updated.get().getUpdatedBy()).isEqualTo("updater");
    }

    @Test
    void 取引先グループを削除できる() {
        // Given
        CompanyGroup companyGroup = createTestCompanyGroup("0001");
        companyGroupMapper.insert(companyGroup);

        // When
        int result = companyGroupMapper.delete("0001");

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CompanyGroup> deleted = companyGroupMapper.findById("0001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void 取引先グループコードで取引先グループを取得できる() {
        // Given
        CompanyGroup companyGroup = createTestCompanyGroup("0001");
        companyGroupMapper.insert(companyGroup);

        // When
        Optional<CompanyGroup> found = companyGroupMapper.findById("0001");

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getCompanyGroupCode()).isEqualTo("0001");
    }

    @Test
    void すべての取引先グループを取得できる() {
        // Given
        companyGroupMapper.insert(createTestCompanyGroup("0001"));
        companyGroupMapper.insert(createTestCompanyGroup("0002"));

        // When
        List<CompanyGroup> companyGroups = companyGroupMapper.findAll();

        // Then
        assertThat(companyGroups).hasSize(2);
        assertThat(companyGroups)
            .extracting(CompanyGroup::getCompanyGroupCode)
            .containsExactly("0001", "0002");
    }

    private CompanyGroup createTestCompanyGroup(String code) {
        CompanyGroup companyGroup = new CompanyGroup();
        companyGroup.setCompanyGroupCode(code);
        companyGroup.setCompanyGroupName("テストグループ");
        companyGroup.setCreatedBy("tester");
        companyGroup.setUpdatedBy("tester");
        return companyGroup;
    }
}
