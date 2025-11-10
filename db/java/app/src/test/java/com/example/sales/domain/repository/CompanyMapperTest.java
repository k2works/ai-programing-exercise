package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.Company;
import com.example.sales.domain.model.CompanyGroup;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 取引先マスタMapperのテストクラス
 */
class CompanyMapperTest extends AbstractDatabaseTest {

    @Autowired
    private CompanyMapper companyMapper;

    @Autowired
    private CompanyGroupMapper companyGroupMapper;

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
        jdbcTemplate.execute("DELETE FROM 仕入先マスタ");
        jdbcTemplate.execute("DELETE FROM 顧客マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類所属マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先グループマスタ");

        // 取引先グループを作成
        CompanyGroup companyGroup = new CompanyGroup();
        companyGroup.setCompanyGroupCode("0001");
        companyGroup.setCompanyGroupName("テストグループ");
        companyGroup.setCreatedBy("tester");
        companyGroup.setUpdatedBy("tester");
        companyGroupMapper.insert(companyGroup);
    }

    @Test
    void 取引先を登録できる() {
        // Given
        Company company = createTestCompany("00000001");

        // When
        int result = companyMapper.insert(company);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Company> saved = companyMapper.findById("00000001");
        assertThat(saved).isPresent();
        assertThat(saved.get().getCompanyCode()).isEqualTo("00000001");
        assertThat(saved.get().getCompanyName()).isEqualTo("テスト取引先");
    }

    @Test
    void 取引先を更新できる() {
        // Given
        Company company = createTestCompany("00000001");
        companyMapper.insert(company);

        // When
        company.setCompanyName("更新後取引先");
        company.setUpdatedBy("updater");
        int result = companyMapper.update(company);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Company> updated = companyMapper.findById("00000001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getCompanyName()).isEqualTo("更新後取引先");
        assertThat(updated.get().getUpdatedBy()).isEqualTo("updater");
    }

    @Test
    void 取引先を削除できる() {
        // Given
        Company company = createTestCompany("00000001");
        companyMapper.insert(company);

        // When
        int result = companyMapper.delete("00000001");

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Company> deleted = companyMapper.findById("00000001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void 取引先コードで取引先を取得できる() {
        // Given
        Company company = createTestCompany("00000001");
        companyMapper.insert(company);

        // When
        Optional<Company> found = companyMapper.findById("00000001");

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getCompanyCode()).isEqualTo("00000001");
    }

    @Test
    void すべての取引先を取得できる() {
        // Given
        companyMapper.insert(createTestCompany("00000001"));
        companyMapper.insert(createTestCompany("00000002"));

        // When
        List<Company> companies = companyMapper.findAll();

        // Then
        assertThat(companies).hasSize(2);
        assertThat(companies)
            .extracting(Company::getCompanyCode)
            .containsExactly("00000001", "00000002");
    }

    @Test
    void 取引先グループコードで取引先を取得できる() {
        // Given
        companyMapper.insert(createTestCompany("00000001"));
        companyMapper.insert(createTestCompany("00000002"));

        // When
        List<Company> companies = companyMapper.findByGroupCode("0001");

        // Then
        assertThat(companies).hasSize(2);
        assertThat(companies)
            .extracting(Company::getCompanyCode)
            .containsExactly("00000001", "00000002");
    }

    private Company createTestCompany(String code) {
        Company company = new Company();
        company.setCompanyCode(code);
        company.setCompanyName("テスト取引先");
        company.setCompanyNameKana("テストトリヒキサキ");
        company.setSupplierType(0);
        company.setZipCode("1000001");
        company.setState("東京都");
        company.setAddress1("千代田区");
        company.setAddress2("1-1-1");
        company.setNoSalesFlag(0);
        company.setWideUseType(0);
        company.setCompanyGroupCode("0001");
        company.setMaxCredit(1000000);
        company.setTempCreditUp(0);
        company.setCreatedBy("tester");
        company.setUpdatedBy("tester");
        return company;
    }
}
