package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.CategoryType;
import com.example.sales.domain.model.CompanyCategory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 取引先分類マスタMapperのテストクラス
 */
class CompanyCategoryMapperTest extends AbstractDatabaseTest {

    @Autowired
    private CompanyCategoryMapper companyCategoryMapper;

    @Autowired
    private CategoryTypeMapper categoryTypeMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        // テストデータをクリア（外部キー制約により、参照元を先に削除）
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
        jdbcTemplate.execute("DELETE FROM 取引先分類所属マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類種別マスタ");

        // 取引先分類種別を作成
        CategoryType categoryType = new CategoryType();
        categoryType.setCategoryTypeCode("01");
        categoryType.setCategoryTypeName("業種");
        categoryType.setCreatedBy("tester");
        categoryType.setUpdatedBy("tester");
        categoryTypeMapper.insert(categoryType);
    }

    @Test
    void 取引先分類を登録できる() {
        // Given
        CompanyCategory companyCategory = createTestCompanyCategory("01", "00000001");

        // When
        int result = companyCategoryMapper.insert(companyCategory);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CompanyCategory> saved = companyCategoryMapper.findById("01", "00000001");
        assertThat(saved).isPresent();
        assertThat(saved.get().getCategoryTypeCode()).isEqualTo("01");
        assertThat(saved.get().getCompanyCategoryCode()).isEqualTo("00000001");
        assertThat(saved.get().getCompanyCategoryName()).isEqualTo("IT業");
    }

    @Test
    void 取引先分類を更新できる() {
        // Given
        CompanyCategory companyCategory = createTestCompanyCategory("01", "00000001");
        companyCategoryMapper.insert(companyCategory);

        // When
        companyCategory.setCompanyCategoryName("製造業");
        companyCategory.setUpdatedBy("updater");
        int result = companyCategoryMapper.update(companyCategory);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CompanyCategory> updated = companyCategoryMapper.findById("01", "00000001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getCompanyCategoryName()).isEqualTo("製造業");
        assertThat(updated.get().getUpdatedBy()).isEqualTo("updater");
    }

    @Test
    void 取引先分類を削除できる() {
        // Given
        CompanyCategory companyCategory = createTestCompanyCategory("01", "00000001");
        companyCategoryMapper.insert(companyCategory);

        // When
        int result = companyCategoryMapper.delete("01", "00000001");

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CompanyCategory> deleted = companyCategoryMapper.findById("01", "00000001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void 複合主キーで取引先分類を取得できる() {
        // Given
        CompanyCategory companyCategory = createTestCompanyCategory("01", "00000001");
        companyCategoryMapper.insert(companyCategory);

        // When
        Optional<CompanyCategory> found = companyCategoryMapper.findById("01", "00000001");

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getCategoryTypeCode()).isEqualTo("01");
        assertThat(found.get().getCompanyCategoryCode()).isEqualTo("00000001");
    }

    @Test
    void 取引先分類種別コードで取引先分類を取得できる() {
        // Given
        companyCategoryMapper.insert(createTestCompanyCategory("01", "00000001"));
        companyCategoryMapper.insert(createTestCompanyCategory("01", "00000002"));

        // When
        List<CompanyCategory> categories = companyCategoryMapper.findByCategoryTypeCode("01");

        // Then
        assertThat(categories).hasSize(2);
        assertThat(categories)
            .extracting(CompanyCategory::getCompanyCategoryCode)
            .containsExactly("00000001", "00000002");
    }

    @Test
    void すべての取引先分類を取得できる() {
        // Given
        companyCategoryMapper.insert(createTestCompanyCategory("01", "00000001"));
        companyCategoryMapper.insert(createTestCompanyCategory("01", "00000002"));

        // When
        List<CompanyCategory> categories = companyCategoryMapper.findAll();

        // Then
        assertThat(categories).hasSize(2);
    }

    private CompanyCategory createTestCompanyCategory(String typeCode, String categoryCode) {
        CompanyCategory companyCategory = new CompanyCategory();
        companyCategory.setCategoryTypeCode(typeCode);
        companyCategory.setCompanyCategoryCode(categoryCode);
        companyCategory.setCompanyCategoryName("IT業");
        companyCategory.setCreatedBy("tester");
        companyCategory.setUpdatedBy("tester");
        return companyCategory;
    }
}
