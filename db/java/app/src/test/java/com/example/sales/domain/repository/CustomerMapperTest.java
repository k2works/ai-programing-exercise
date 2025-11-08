package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.Company;
import com.example.sales.domain.model.CompanyGroup;
import com.example.sales.domain.model.Customer;
import com.example.sales.domain.model.Department;
import com.example.sales.domain.model.Employee;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 顧客マスタMapperのテストクラス
 */
class CustomerMapperTest extends AbstractDatabaseTest {

    @Autowired
    private CustomerMapper customerMapper;

    @Autowired
    private CompanyMapper companyMapper;

    @Autowired
    private CompanyGroupMapper companyGroupMapper;

    @Autowired
    private DepartmentMapper departmentMapper;

    @Autowired
    private EmployeeMapper employeeMapper;

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
    }

    @Test
    void 顧客を登録できる() {
        // Given
        Customer customer = createTestCustomer("00000001", 1);

        // When
        int result = customerMapper.insert(customer);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Customer> saved = customerMapper.findById("00000001", 1);
        assertThat(saved).isPresent();
        assertThat(saved.get().getCustomerCode()).isEqualTo("00000001");
        assertThat(saved.get().getCustomerBranch()).isEqualTo(1);
        assertThat(saved.get().getCustomerName()).isEqualTo("テスト顧客");
    }

    @Test
    void 顧客を更新できる() {
        // Given
        Customer customer = createTestCustomer("00000001", 1);
        customerMapper.insert(customer);

        // When
        customer.setCustomerName("更新後顧客");
        customer.setUpdatedBy("updater");
        int result = customerMapper.update(customer);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Customer> updated = customerMapper.findById("00000001", 1);
        assertThat(updated).isPresent();
        assertThat(updated.get().getCustomerName()).isEqualTo("更新後顧客");
        assertThat(updated.get().getUpdatedBy()).isEqualTo("updater");
    }

    @Test
    void 顧客を削除できる() {
        // Given
        Customer customer = createTestCustomer("00000001", 1);
        customerMapper.insert(customer);

        // When
        int result = customerMapper.delete("00000001", 1);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Customer> deleted = customerMapper.findById("00000001", 1);
        assertThat(deleted).isEmpty();
    }

    @Test
    void 顧客コードと枝番で顧客を取得できる() {
        // Given
        Customer customer = createTestCustomer("00000001", 1);
        customerMapper.insert(customer);

        // When
        Optional<Customer> found = customerMapper.findById("00000001", 1);

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getCustomerCode()).isEqualTo("00000001");
        assertThat(found.get().getCustomerBranch()).isEqualTo(1);
    }

    @Test
    void 顧客コードで複数の顧客を取得できる() {
        // Given
        customerMapper.insert(createTestCustomer("00000001", 1));
        customerMapper.insert(createTestCustomer("00000001", 2));

        // When
        List<Customer> customers = customerMapper.findByCustomerCode("00000001");

        // Then
        assertThat(customers).hasSize(2);
        assertThat(customers)
            .extracting(Customer::getCustomerBranch)
            .containsExactly(1, 2);
    }

    @Test
    void 取引先コードで顧客を取得できる() {
        // Given
        customerMapper.insert(createTestCustomer("00000001", 1));
        customerMapper.insert(createTestCustomer("00000001", 2));

        // When
        List<Customer> customers = customerMapper.findByCompanyCode("00000001");

        // Then
        assertThat(customers).hasSize(2);
    }

    @Test
    void すべての顧客を取得できる() {
        // Given
        customerMapper.insert(createTestCustomer("00000001", 1));
        customerMapper.insert(createTestCustomer("00000001", 2));

        // When
        List<Customer> customers = customerMapper.findAll();

        // Then
        assertThat(customers).hasSize(2);
    }

    private Customer createTestCustomer(String code, Integer branch) {
        Customer customer = new Customer();
        customer.setCustomerCode(code);
        customer.setCustomerBranch(branch);
        customer.setCustomerType(0);
        customer.setArCode(code);
        customer.setArBranch(branch);
        customer.setPayerCode(code);
        customer.setPayerBranch(branch);
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
        return customer;
    }
}
