package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.CategoryType;
import com.example.sales.domain.model.Company;
import com.example.sales.domain.model.CompanyCategory;
import com.example.sales.domain.model.CompanyCategoryGroup;
import com.example.sales.domain.model.CompanyGroup;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 取引先分類所属マスタMapperのテストクラス
 */
class CompanyCategoryGroupMapperTest extends AbstractDatabaseTest {

    @Autowired
    private CompanyCategoryGroupMapper companyCategoryGroupMapper;

    @Autowired
    private CompanyCategoryMapper companyCategoryMapper;

    @Autowired
    private CategoryTypeMapper categoryTypeMapper;

    @Autowired
    private CompanyMapper companyMapper;

    @Autowired
    private CompanyGroupMapper companyGroupMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        // テストデータをクリア（外部キー制約により、参照元を先に削除）
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
        jdbcTemplate.execute("DELETE FROM 仕入先マスタ");
        jdbcTemplate.execute("DELETE FROM 顧客マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先グループマスタ");

        // 取引先グループを作成
        CompanyGroup companyGroup = new CompanyGroup();
        companyGroup.setCompanyGroupCode("0001");
        companyGroup.setCompanyGroupName("テストグループ");
        companyGroup.setCreatedBy("tester");
        companyGroup.setUpdatedBy("tester");
        companyGroupMapper.insert(companyGroup);

        // 取引先を作成
        Company company = new Company();
        company.setCompanyCode("00000001");
        company.setCompanyName("テスト取引先");
        company.setCompanyNameKana("テストトリヒキサキ");
        company.setSupplierType(0);
        company.setCompanyGroupCode("0001");
        company.setCreatedBy("tester");
        company.setUpdatedBy("tester");
        companyMapper.insert(company);

        // 取引先分類種別を作成
        CategoryType categoryType = new CategoryType();
        categoryType.setCategoryTypeCode("01");
        categoryType.setCategoryTypeName("業種");
        categoryType.setCreatedBy("tester");
        categoryType.setUpdatedBy("tester");
        categoryTypeMapper.insert(categoryType);

        // 取引先分類を作成
        CompanyCategory companyCategory = new CompanyCategory();
        companyCategory.setCategoryTypeCode("01");
        companyCategory.setCompanyCategoryCode("00000001");
        companyCategory.setCompanyCategoryName("IT業");
        companyCategory.setCreatedBy("tester");
        companyCategory.setUpdatedBy("tester");
        companyCategoryMapper.insert(companyCategory);
    }

    @Test
    void 取引先分類所属を登録できる() {
        // Given
        CompanyCategoryGroup group = createTestCompanyCategoryGroup("01", "00000001", "00000001");

        // When
        int result = companyCategoryGroupMapper.insert(group);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CompanyCategoryGroup> saved = companyCategoryGroupMapper.findById("01", "00000001", "00000001");
        assertThat(saved).isPresent();
        assertThat(saved.get().getCategoryTypeCode()).isEqualTo("01");
        assertThat(saved.get().getCompanyCategoryCode()).isEqualTo("00000001");
        assertThat(saved.get().getCompanyCode()).isEqualTo("00000001");
    }

    @Test
    void 取引先分類所属を更新できる() {
        // Given
        CompanyCategoryGroup group = createTestCompanyCategoryGroup("01", "00000001", "00000001");
        companyCategoryGroupMapper.insert(group);

        // When
        group.setUpdatedBy("updater");
        int result = companyCategoryGroupMapper.update(group);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CompanyCategoryGroup> updated = companyCategoryGroupMapper.findById("01", "00000001", "00000001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getUpdatedBy()).isEqualTo("updater");
    }

    @Test
    void 取引先分類所属を削除できる() {
        // Given
        CompanyCategoryGroup group = createTestCompanyCategoryGroup("01", "00000001", "00000001");
        companyCategoryGroupMapper.insert(group);

        // When
        int result = companyCategoryGroupMapper.delete("01", "00000001", "00000001");

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CompanyCategoryGroup> deleted = companyCategoryGroupMapper.findById("01", "00000001", "00000001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void 複合主キーで取引先分類所属を取得できる() {
        // Given
        CompanyCategoryGroup group = createTestCompanyCategoryGroup("01", "00000001", "00000001");
        companyCategoryGroupMapper.insert(group);

        // When
        Optional<CompanyCategoryGroup> found = companyCategoryGroupMapper.findById("01", "00000001", "00000001");

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getCategoryTypeCode()).isEqualTo("01");
        assertThat(found.get().getCompanyCategoryCode()).isEqualTo("00000001");
        assertThat(found.get().getCompanyCode()).isEqualTo("00000001");
    }

    @Test
    void 取引先コードで取引先分類所属を取得できる() {
        // Given
        companyCategoryGroupMapper.insert(createTestCompanyCategoryGroup("01", "00000001", "00000001"));

        // When
        List<CompanyCategoryGroup> groups = companyCategoryGroupMapper.findByCompanyCode("00000001");

        // Then
        assertThat(groups).hasSize(1);
        assertThat(groups.get(0).getCompanyCode()).isEqualTo("00000001");
    }

    @Test
    void 取引先分類コードで取引先分類所属を取得できる() {
        // Given
        companyCategoryGroupMapper.insert(createTestCompanyCategoryGroup("01", "00000001", "00000001"));

        // When
        List<CompanyCategoryGroup> groups = companyCategoryGroupMapper.findByCategoryCode("01", "00000001");

        // Then
        assertThat(groups).hasSize(1);
        assertThat(groups.get(0).getCategoryTypeCode()).isEqualTo("01");
        assertThat(groups.get(0).getCompanyCategoryCode()).isEqualTo("00000001");
    }

    @Test
    void すべての取引先分類所属を取得できる() {
        // Given
        companyCategoryGroupMapper.insert(createTestCompanyCategoryGroup("01", "00000001", "00000001"));

        // When
        List<CompanyCategoryGroup> groups = companyCategoryGroupMapper.findAll();

        // Then
        assertThat(groups).hasSize(1);
    }

    private CompanyCategoryGroup createTestCompanyCategoryGroup(
            String typeCode, String categoryCode, String companyCode) {
        CompanyCategoryGroup group = new CompanyCategoryGroup();
        group.setCategoryTypeCode(typeCode);
        group.setCompanyCategoryCode(categoryCode);
        group.setCompanyCode(companyCode);
        group.setCreatedBy("tester");
        group.setUpdatedBy("tester");
        return group;
    }
}
