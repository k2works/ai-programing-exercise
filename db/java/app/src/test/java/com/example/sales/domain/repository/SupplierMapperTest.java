package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.Company;
import com.example.sales.domain.model.CompanyGroup;
import com.example.sales.domain.model.Supplier;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 仕入先マスタMapperのテストクラス
 */
class SupplierMapperTest extends AbstractDatabaseTest {

    @Autowired
    private SupplierMapper supplierMapper;

    @Autowired
    private CompanyMapper companyMapper;

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
        company.setSupplierType(1);
        company.setCompanyGroupCode("0001");
        company.setCreatedBy("tester");
        company.setUpdatedBy("tester");
        companyMapper.insert(company);
    }

    @Test
    void 仕入先を登録できる() {
        // Given
        Supplier supplier = createTestSupplier("00000001", 1);

        // When
        int result = supplierMapper.insert(supplier);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Supplier> saved = supplierMapper.findById("00000001", 1);
        assertThat(saved).isPresent();
        assertThat(saved.get().getSupplierCode()).isEqualTo("00000001");
        assertThat(saved.get().getSupplierBranch()).isEqualTo(1);
        assertThat(saved.get().getSupplierName()).isEqualTo("テスト仕入先");
    }

    @Test
    void 仕入先を更新できる() {
        // Given
        Supplier supplier = createTestSupplier("00000001", 1);
        supplierMapper.insert(supplier);

        // When
        supplier.setSupplierName("更新後仕入先");
        supplier.setUpdatedBy("updater");
        int result = supplierMapper.update(supplier);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Supplier> updated = supplierMapper.findById("00000001", 1);
        assertThat(updated).isPresent();
        assertThat(updated.get().getSupplierName()).isEqualTo("更新後仕入先");
        assertThat(updated.get().getUpdatedBy()).isEqualTo("updater");
    }

    @Test
    void 仕入先を削除できる() {
        // Given
        Supplier supplier = createTestSupplier("00000001", 1);
        supplierMapper.insert(supplier);

        // When
        int result = supplierMapper.delete("00000001", 1);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Supplier> deleted = supplierMapper.findById("00000001", 1);
        assertThat(deleted).isEmpty();
    }

    @Test
    void 仕入先コードと枝番で仕入先を取得できる() {
        // Given
        Supplier supplier = createTestSupplier("00000001", 1);
        supplierMapper.insert(supplier);

        // When
        Optional<Supplier> found = supplierMapper.findById("00000001", 1);

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getSupplierCode()).isEqualTo("00000001");
        assertThat(found.get().getSupplierBranch()).isEqualTo(1);
    }

    @Test
    void 仕入先コードで複数の仕入先を取得できる() {
        // Given
        supplierMapper.insert(createTestSupplier("00000001", 1));
        supplierMapper.insert(createTestSupplier("00000001", 2));

        // When
        List<Supplier> suppliers = supplierMapper.findBySupplierCode("00000001");

        // Then
        assertThat(suppliers).hasSize(2);
        assertThat(suppliers)
            .extracting(Supplier::getSupplierBranch)
            .containsExactly(1, 2);
    }

    @Test
    void 取引先コードで仕入先を取得できる() {
        // Given
        supplierMapper.insert(createTestSupplier("00000001", 1));
        supplierMapper.insert(createTestSupplier("00000001", 2));

        // When
        List<Supplier> suppliers = supplierMapper.findByCompanyCode("00000001");

        // Then
        assertThat(suppliers).hasSize(2);
    }

    @Test
    void すべての仕入先を取得できる() {
        // Given
        supplierMapper.insert(createTestSupplier("00000001", 1));
        supplierMapper.insert(createTestSupplier("00000001", 2));

        // When
        List<Supplier> suppliers = supplierMapper.findAll();

        // Then
        assertThat(suppliers).hasSize(2);
    }

    private Supplier createTestSupplier(String code, Integer branch) {
        Supplier supplier = new Supplier();
        supplier.setSupplierCode(code);
        supplier.setSupplierBranch(branch);
        supplier.setSupplierName("テスト仕入先");
        supplier.setSupplierNameKana("テストシイレサキ");
        supplier.setSupplierUserName("テスト担当者");
        supplier.setSupplierDepartmentName("営業部");
        supplier.setSupplierZipCode("1000001");
        supplier.setSupplierState("東京都");
        supplier.setSupplierAddress1("千代田区");
        supplier.setSupplierAddress2("1-1-1");
        supplier.setSupplierTel("03-1234-5678");
        supplier.setSupplierFax("03-1234-5679");
        supplier.setSupplierEmail("test@example.com");
        supplier.setSupplierCloseDate(31);
        supplier.setSupplierPayMonths(1);
        supplier.setSupplierPayDates(31);
        supplier.setSupplierPayMethod(1);
        supplier.setCreatedBy("tester");
        supplier.setUpdatedBy("tester");
        return supplier;
    }
}
