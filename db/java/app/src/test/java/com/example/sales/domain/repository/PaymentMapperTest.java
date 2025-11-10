package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 支払データMapperのテストクラス
 */
class PaymentMapperTest extends AbstractDatabaseTest {

    @Autowired
    private PaymentMapper paymentMapper;

    @Autowired
    private DepartmentMapper departmentMapper;

    @Autowired
    private CompanyMapper companyMapper;

    @Autowired
    private CompanyGroupMapper companyGroupMapper;

    @Autowired
    private SupplierMapper supplierMapper;

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
        jdbcTemplate.execute("DELETE FROM 倉庫マスタ");
        jdbcTemplate.execute("DELETE FROM 顧客マスタ");
        jdbcTemplate.execute("DELETE FROM 仕入先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類所属マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先グループマスタ");
        jdbcTemplate.execute("DELETE FROM 社員マスタ");
        jdbcTemplate.execute("DELETE FROM 部門マスタ");

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

        // 部門を作成
        Department department = new Department();
        department.setDepartmentCode("10000");
        department.setStartDate(LocalDate.of(2024, 1, 1));
        department.setEndDate(LocalDate.of(9999, 12, 31));
        department.setDepartmentName("営業部");
        department.setOrganizationLevel(1);
        department.setDepartmentPath("10000");
        department.setLowestLevelFlag(1);
        department.setSlipInputFlag(0);
        department.setCreatedBy("tester");
        department.setUpdatedBy("tester");
        departmentMapper.insert(department);

        // 仕入先を作成
        Supplier supplier = new Supplier();
        supplier.setSupplierCode("00000001");
        supplier.setSupplierBranch(1);
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
        supplierMapper.insert(supplier);
    }

    @Test
    void testInsertAndFindById() {
        // 支払を作成
        Payment payment = new Payment();
        payment.setPaymentNo("P000000001");
        payment.setPaymentDate(20240131);
        payment.setDepartmentCode("10000");
        payment.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        payment.setSupplierCode("00000001");
        payment.setSupplierBranch(1);
        payment.setPaymentMethodType(1);
        payment.setPaymentAmount(100000);
        payment.setConsumptionTax(10000);
        payment.setCompleteFlag(0);
        payment.setCreatedBy("tester");
        payment.setUpdatedBy("tester");

        // 挿入
        int result = paymentMapper.insert(payment);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<Payment> found = paymentMapper.findById("P000000001");
        assertThat(found).isPresent();
        assertThat(found.get().getPaymentNo()).isEqualTo("P000000001");
        assertThat(found.get().getSupplierCode()).isEqualTo("00000001");
        assertThat(found.get().getPaymentAmount()).isEqualTo(100000);
    }

    @Test
    void testUpdate() {
        // 支払を作成
        Payment payment = new Payment();
        payment.setPaymentNo("P000000001");
        payment.setPaymentDate(20240131);
        payment.setDepartmentCode("10000");
        payment.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        payment.setSupplierCode("00000001");
        payment.setSupplierBranch(1);
        payment.setPaymentMethodType(1);
        payment.setPaymentAmount(100000);
        payment.setConsumptionTax(10000);
        payment.setCompleteFlag(0);
        payment.setCreatedBy("tester");
        payment.setUpdatedBy("tester");
        paymentMapper.insert(payment);

        // 更新
        payment.setCompleteFlag(1);
        int updateResult = paymentMapper.update(payment);
        assertThat(updateResult).isEqualTo(1);

        // 確認
        Optional<Payment> updated = paymentMapper.findById("P000000001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getCompleteFlag()).isEqualTo(1);
    }

    @Test
    void testDelete() {
        // 支払を作成
        Payment payment = new Payment();
        payment.setPaymentNo("P000000001");
        payment.setPaymentDate(20240131);
        payment.setDepartmentCode("10000");
        payment.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        payment.setSupplierCode("00000001");
        payment.setSupplierBranch(1);
        payment.setPaymentAmount(100000);
        payment.setCreatedBy("tester");
        payment.setUpdatedBy("tester");
        paymentMapper.insert(payment);

        // 削除
        int deleteResult = paymentMapper.delete("P000000001");
        assertThat(deleteResult).isEqualTo(1);

        // 確認
        Optional<Payment> deleted = paymentMapper.findById("P000000001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void testFindBySupplierCode() {
        // 支払を2件作成
        Payment payment1 = new Payment();
        payment1.setPaymentNo("P000000001");
        payment1.setPaymentDate(20240131);
        payment1.setDepartmentCode("10000");
        payment1.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        payment1.setSupplierCode("00000001");
        payment1.setSupplierBranch(1);
        payment1.setPaymentAmount(100000);
        payment1.setCreatedBy("tester");
        payment1.setUpdatedBy("tester");
        paymentMapper.insert(payment1);

        Payment payment2 = new Payment();
        payment2.setPaymentNo("P000000002");
        payment2.setPaymentDate(20240229);
        payment2.setDepartmentCode("10000");
        payment2.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        payment2.setSupplierCode("00000001");
        payment2.setSupplierBranch(1);
        payment2.setPaymentAmount(200000);
        payment2.setCreatedBy("tester");
        payment2.setUpdatedBy("tester");
        paymentMapper.insert(payment2);

        // 仕入先コードで検索
        List<Payment> payments = paymentMapper.findBySupplierCode("00000001", 1);
        assertThat(payments).hasSize(2);
    }

    @Test
    void testFindByPaymentDate() {
        // 支払を作成
        Payment payment = new Payment();
        payment.setPaymentNo("P000000001");
        payment.setPaymentDate(20240131);
        payment.setDepartmentCode("10000");
        payment.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        payment.setSupplierCode("00000001");
        payment.setSupplierBranch(1);
        payment.setPaymentAmount(100000);
        payment.setCreatedBy("tester");
        payment.setUpdatedBy("tester");
        paymentMapper.insert(payment);

        // 支払日で検索
        List<Payment> payments = paymentMapper.findByPaymentDate(20240131);
        assertThat(payments).hasSize(1);
        assertThat(payments.get(0).getPaymentNo()).isEqualTo("P000000001");
    }

    @Test
    void testFindAll() {
        // 支払を2件作成
        Payment payment1 = new Payment();
        payment1.setPaymentNo("P000000001");
        payment1.setPaymentDate(20240131);
        payment1.setDepartmentCode("10000");
        payment1.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        payment1.setSupplierCode("00000001");
        payment1.setSupplierBranch(1);
        payment1.setPaymentAmount(100000);
        payment1.setCreatedBy("tester");
        payment1.setUpdatedBy("tester");
        paymentMapper.insert(payment1);

        Payment payment2 = new Payment();
        payment2.setPaymentNo("P000000002");
        payment2.setPaymentDate(20240229);
        payment2.setDepartmentCode("10000");
        payment2.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        payment2.setSupplierCode("00000001");
        payment2.setSupplierBranch(1);
        payment2.setPaymentAmount(200000);
        payment2.setCreatedBy("tester");
        payment2.setUpdatedBy("tester");
        paymentMapper.insert(payment2);

        // 全件取得
        List<Payment> payments = paymentMapper.findAll();
        assertThat(payments).hasSize(2);
    }
}
