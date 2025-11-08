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
 * 売上データMapperのテストクラス
 */
class SalesMapperTest extends AbstractDatabaseTest {

    @Autowired
    private SalesMapper salesMapper;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private DepartmentMapper departmentMapper;

    @Autowired
    private EmployeeMapper employeeMapper;

    @Autowired
    private CompanyMapper companyMapper;

    @Autowired
    private CompanyGroupMapper companyGroupMapper;

    @Autowired
    private CustomerMapper customerMapper;

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

        // 受注を作成
        Order order = new Order();
        order.setOrderNo("O000000001");
        order.setOrderDate(LocalDateTime.of(2024, 1, 15, 10, 0));
        order.setDepartmentCode("10000");
        order.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        order.setCustomerCode("00000001");
        order.setCustomerBranch(1);
        order.setEmployeeCode("0000000001");
        order.setWarehouseCode("001");
        order.setOrderAmount(100000);
        order.setConsumptionTax(10000);
        order.setCreatedBy("tester");
        order.setUpdatedBy("tester");
        orderMapper.insert(order);
    }

    @Test
    void testInsertAndFindById() {
        // 売上を作成
        Sales sales = new Sales();
        sales.setSalesNo("S000000001");
        sales.setOrderNo("O000000001");
        sales.setSalesDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        sales.setSalesDivision(1);
        sales.setDepartmentCode("10000");
        sales.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        sales.setCompanyCode("00000001");
        sales.setEmployeeCode("0000000001");
        sales.setSalesAmount(100000);
        sales.setConsumptionTax(10000);
        sales.setSlipComment("テスト売上");
        sales.setCreatedBy("tester");
        sales.setUpdatedBy("tester");

        // 挿入
        int result = salesMapper.insert(sales);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<Sales> found = salesMapper.findById("S000000001");
        assertThat(found).isPresent();
        assertThat(found.get().getSalesNo()).isEqualTo("S000000001");
        assertThat(found.get().getOrderNo()).isEqualTo("O000000001");
        assertThat(found.get().getSalesAmount()).isEqualTo(100000);
    }

    @Test
    void testUpdate() {
        // 売上を作成
        Sales sales = new Sales();
        sales.setSalesNo("S000000001");
        sales.setOrderNo("O000000001");
        sales.setSalesDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        sales.setSalesDivision(1);
        sales.setDepartmentCode("10000");
        sales.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        sales.setCompanyCode("00000001");
        sales.setEmployeeCode("0000000001");
        sales.setSalesAmount(100000);
        sales.setConsumptionTax(10000);
        sales.setCreatedBy("tester");
        sales.setUpdatedBy("tester");
        salesMapper.insert(sales);

        // 更新
        sales.setSalesAmount(120000);
        sales.setConsumptionTax(12000);
        sales.setUpdatedBy("updater");
        int result = salesMapper.update(sales);
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Sales> found = salesMapper.findById("S000000001");
        assertThat(found).isPresent();
        assertThat(found.get().getSalesAmount()).isEqualTo(120000);
        assertThat(found.get().getConsumptionTax()).isEqualTo(12000);
    }

    @Test
    void testDelete() {
        // 売上を作成
        Sales sales = new Sales();
        sales.setSalesNo("S000000001");
        sales.setOrderNo("O000000001");
        sales.setSalesDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        sales.setSalesDivision(1);
        sales.setDepartmentCode("10000");
        sales.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        sales.setCompanyCode("00000001");
        sales.setEmployeeCode("0000000001");
        sales.setSalesAmount(100000);
        sales.setConsumptionTax(10000);
        sales.setCreatedBy("tester");
        sales.setUpdatedBy("tester");
        salesMapper.insert(sales);

        // 削除
        int result = salesMapper.delete("S000000001");
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Sales> found = salesMapper.findById("S000000001");
        assertThat(found).isEmpty();
    }

    @Test
    void testFindByOrderNo() {
        // 売上1を作成
        Sales sales1 = new Sales();
        sales1.setSalesNo("S000000001");
        sales1.setOrderNo("O000000001");
        sales1.setSalesDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        sales1.setSalesDivision(1);
        sales1.setDepartmentCode("10000");
        sales1.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        sales1.setCompanyCode("00000001");
        sales1.setEmployeeCode("0000000001");
        sales1.setSalesAmount(100000);
        sales1.setCreatedBy("tester");
        sales1.setUpdatedBy("tester");
        salesMapper.insert(sales1);

        // 売上2を作成
        Sales sales2 = new Sales();
        sales2.setSalesNo("S000000002");
        sales2.setOrderNo("O000000001");
        sales2.setSalesDate(LocalDateTime.of(2024, 1, 25, 10, 0));
        sales2.setSalesDivision(1);
        sales2.setDepartmentCode("10000");
        sales2.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        sales2.setCompanyCode("00000001");
        sales2.setEmployeeCode("0000000001");
        sales2.setSalesAmount(50000);
        sales2.setCreatedBy("tester");
        sales2.setUpdatedBy("tester");
        salesMapper.insert(sales2);

        // 受注番号で検索
        List<Sales> salesList = salesMapper.findByOrderNo("O000000001");
        assertThat(salesList).hasSize(2);
        assertThat(salesList.get(0).getSalesNo()).isEqualTo("S000000001");
        assertThat(salesList.get(1).getSalesNo()).isEqualTo("S000000002");
    }

    @Test
    void testFindAll() {
        // 売上1を作成
        Sales sales1 = new Sales();
        sales1.setSalesNo("S000000001");
        sales1.setOrderNo("O000000001");
        sales1.setSalesDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        sales1.setSalesDivision(1);
        sales1.setDepartmentCode("10000");
        sales1.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        sales1.setCompanyCode("00000001");
        sales1.setEmployeeCode("0000000001");
        sales1.setSalesAmount(100000);
        sales1.setCreatedBy("tester");
        sales1.setUpdatedBy("tester");
        salesMapper.insert(sales1);

        // 売上2を作成
        Sales sales2 = new Sales();
        sales2.setSalesNo("S000000002");
        sales2.setOrderNo("O000000001");
        sales2.setSalesDate(LocalDateTime.of(2024, 1, 25, 10, 0));
        sales2.setSalesDivision(1);
        sales2.setDepartmentCode("10000");
        sales2.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        sales2.setCompanyCode("00000001");
        sales2.setEmployeeCode("0000000001");
        sales2.setSalesAmount(50000);
        sales2.setCreatedBy("tester");
        sales2.setUpdatedBy("tester");
        salesMapper.insert(sales2);

        // 全件取得
        List<Sales> salesList = salesMapper.findAll();
        assertThat(salesList).hasSize(2);
    }
}
