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
 * 入金データMapperのテストクラス
 */
class CreditMapperTest extends AbstractDatabaseTest {

    @Autowired
    private CreditMapper creditMapper;

    @Autowired
    private BankAccountMapper bankAccountMapper;

    @Autowired
    private DepartmentMapper departmentMapper;

    @Autowired
    private CompanyMapper companyMapper;

    @Autowired
    private CompanyGroupMapper companyGroupMapper;

    @Autowired
    private CustomerMapper customerMapper;

    @Autowired
    private EmployeeMapper employeeMapper;

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

        // 社員を作成
        Employee employee = new Employee();
        employee.setEmployeeCode("0000000001");
        employee.setDepartmentCode("10000");
        employee.setEmployeeName("テスト社員");
        employee.setEmployeeNameKana("テストシャイン");
        employee.setGender("M");
        employee.setBirthDate(LocalDate.of(1990, 1, 1));
        employee.setJoinDate(LocalDate.of(2020, 4, 1));
        employee.setCreatedBy("tester");
        employee.setUpdatedBy("tester");
        employeeMapper.insert(employee);

        // 顧客を作成
        Customer customer = new Customer();
        customer.setCustomerCode("00000001");
        customer.setCustomerBranch(1);
        customer.setCustomerType(1);
        customer.setArCode("00000001");
        customer.setArBranch(1);
        customer.setPayerCode("00000001");
        customer.setPayerBranch(1);
        customer.setCustomerName("テスト顧客");
        customer.setCustomerNameKana("テストコキャク");
        customer.setEmployeeCode("0000000001");
        customer.setCustomerUserName("テスト担当者");
        customer.setCustomerDepartmentName("営業部");
        customer.setCustomerZipCode("1000001");
        customer.setCustomerState("東京都");
        customer.setCustomerAddress1("千代田区");
        customer.setCustomerAddress2("1-1-1");
        customer.setCustomerTel("03-1234-5678");
        customer.setCustomerFax("03-1234-5679");
        customer.setCustomerEmail("test@example.com");
        customer.setCustomerArType(0);
        customer.setCustomerCloseDate1(31);
        customer.setCustomerPayMonths1(1);
        customer.setCustomerPayDates1(31);
        customer.setCustomerPayMethod1(1);
        customer.setCustomerCloseDate2(31);
        customer.setCustomerPayMonths2(1);
        customer.setCustomerPayDates2(31);
        customer.setCustomerPayMethod2(1);
        customer.setCreatedBy("tester");
        customer.setUpdatedBy("tester");
        customerMapper.insert(customer);

        // 入金口座を作成
        BankAccount bankAccount = new BankAccount();
        bankAccount.setBankAccountCode("00000001");
        bankAccount.setAccountHolder("株式会社テスト");
        bankAccount.setBankName("テスト銀行");
        bankAccount.setBranchName("テスト支店");
        bankAccount.setAccountType(1);
        bankAccount.setAccountNumber("1234567");
        bankAccount.setCreatedBy("tester");
        bankAccount.setUpdatedBy("tester");
        bankAccountMapper.insert(bankAccount);
    }

    @Test
    void testInsertAndFindById() {
        // 入金を作成
        Credit credit = new Credit();
        credit.setCreditNo("C000000001");
        credit.setCreditDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        credit.setDepartmentCode("10000");
        credit.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        credit.setCustomerCode("00000001");
        credit.setCustomerBranch(1);
        credit.setPaymentMethodType(1);
        credit.setBankAccountCode("00000001");
        credit.setCreditAmount(100000);
        credit.setAppliedAmount(0);
        credit.setCreatedBy("tester");
        credit.setUpdatedBy("tester");

        // 挿入
        int result = creditMapper.insert(credit);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<Credit> found = creditMapper.findById("C000000001");
        assertThat(found).isPresent();
        assertThat(found.get().getCreditNo()).isEqualTo("C000000001");
        assertThat(found.get().getCustomerCode()).isEqualTo("00000001");
        assertThat(found.get().getCreditAmount()).isEqualTo(100000);
    }

    @Test
    void testUpdate() {
        // 入金を作成
        Credit credit = new Credit();
        credit.setCreditNo("C000000001");
        credit.setCreditDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        credit.setDepartmentCode("10000");
        credit.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        credit.setCustomerCode("00000001");
        credit.setCustomerBranch(1);
        credit.setPaymentMethodType(1);
        credit.setBankAccountCode("00000001");
        credit.setCreditAmount(100000);
        credit.setAppliedAmount(0);
        credit.setCreatedBy("tester");
        credit.setUpdatedBy("tester");
        creditMapper.insert(credit);

        // 更新
        credit.setAppliedAmount(100000);
        int updateResult = creditMapper.update(credit);
        assertThat(updateResult).isEqualTo(1);

        // 確認
        Optional<Credit> updated = creditMapper.findById("C000000001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getAppliedAmount()).isEqualTo(100000);
    }

    @Test
    void testDelete() {
        // 入金を作成
        Credit credit = new Credit();
        credit.setCreditNo("C000000001");
        credit.setCreditDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        credit.setDepartmentCode("10000");
        credit.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        credit.setCustomerCode("00000001");
        credit.setCustomerBranch(1);
        credit.setPaymentMethodType(1);
        credit.setBankAccountCode("00000001");
        credit.setCreditAmount(100000);
        credit.setCreatedBy("tester");
        credit.setUpdatedBy("tester");
        creditMapper.insert(credit);

        // 削除
        int deleteResult = creditMapper.delete("C000000001");
        assertThat(deleteResult).isEqualTo(1);

        // 確認
        Optional<Credit> deleted = creditMapper.findById("C000000001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void testFindByCustomerCode() {
        // 入金を2件作成
        Credit credit1 = new Credit();
        credit1.setCreditNo("C000000001");
        credit1.setCreditDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        credit1.setDepartmentCode("10000");
        credit1.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        credit1.setCustomerCode("00000001");
        credit1.setCustomerBranch(1);
        credit1.setCreditAmount(100000);
        credit1.setCreatedBy("tester");
        credit1.setUpdatedBy("tester");
        creditMapper.insert(credit1);

        Credit credit2 = new Credit();
        credit2.setCreditNo("C000000002");
        credit2.setCreditDate(LocalDateTime.of(2024, 2, 28, 0, 0));
        credit2.setDepartmentCode("10000");
        credit2.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        credit2.setCustomerCode("00000001");
        credit2.setCustomerBranch(1);
        credit2.setCreditAmount(200000);
        credit2.setCreatedBy("tester");
        credit2.setUpdatedBy("tester");
        creditMapper.insert(credit2);

        // 顧客コードで検索
        List<Credit> credits = creditMapper.findByCustomerCode("00000001", 1);
        assertThat(credits).hasSize(2);
    }

    @Test
    void testFindAll() {
        // 入金を2件作成
        Credit credit1 = new Credit();
        credit1.setCreditNo("C000000001");
        credit1.setCreditDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        credit1.setDepartmentCode("10000");
        credit1.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        credit1.setCustomerCode("00000001");
        credit1.setCustomerBranch(1);
        credit1.setCreditAmount(100000);
        credit1.setCreatedBy("tester");
        credit1.setUpdatedBy("tester");
        creditMapper.insert(credit1);

        Credit credit2 = new Credit();
        credit2.setCreditNo("C000000002");
        credit2.setCreditDate(LocalDateTime.of(2024, 2, 28, 0, 0));
        credit2.setDepartmentCode("10000");
        credit2.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        credit2.setCustomerCode("00000001");
        credit2.setCustomerBranch(1);
        credit2.setCreditAmount(200000);
        credit2.setCreatedBy("tester");
        credit2.setUpdatedBy("tester");
        creditMapper.insert(credit2);

        // 全件取得
        List<Credit> credits = creditMapper.findAll();
        assertThat(credits).hasSize(2);
    }
}
