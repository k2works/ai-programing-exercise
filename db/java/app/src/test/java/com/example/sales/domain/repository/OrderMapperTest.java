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
 * 受注データMapperのテストクラス
 */
class OrderMapperTest extends AbstractDatabaseTest {

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
    }

    @Test
    void testInsertAndFindById() {
        // 受注を作成
        Order order = new Order();
        order.setOrderNo("O000000001");
        order.setOrderDate(LocalDateTime.of(2024, 1, 15, 10, 0));
        order.setDepartmentCode("10000");
        order.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        order.setCustomerCode("00000001");
        order.setCustomerBranch(1);
        order.setEmployeeCode("0000000001");
        order.setRequiredDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        order.setWarehouseCode("001");
        order.setOrderAmount(100000);
        order.setConsumptionTax(10000);
        order.setSlipComment("テスト受注");
        order.setCreatedBy("tester");
        order.setUpdatedBy("tester");

        // 挿入
        int result = orderMapper.insert(order);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<Order> found = orderMapper.findById("O000000001");
        assertThat(found).isPresent();
        assertThat(found.get().getOrderNo()).isEqualTo("O000000001");
        assertThat(found.get().getCustomerCode()).isEqualTo("00000001");
        assertThat(found.get().getOrderAmount()).isEqualTo(100000);
    }

    @Test
    void testUpdate() {
        // 受注を作成
        Order order = new Order();
        order.setOrderNo("O000000001");
        order.setOrderDate(LocalDateTime.of(2024, 1, 15, 10, 0));
        order.setDepartmentCode("10000");
        order.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        order.setCustomerCode("00000001");
        order.setCustomerBranch(1);
        order.setEmployeeCode("0000000001");
        order.setRequiredDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        order.setWarehouseCode("001");
        order.setOrderAmount(100000);
        order.setConsumptionTax(10000);
        order.setCreatedBy("tester");
        order.setUpdatedBy("tester");
        orderMapper.insert(order);

        // 更新
        order.setOrderAmount(150000);
        order.setConsumptionTax(15000);
        order.setUpdatedBy("updater");
        int result = orderMapper.update(order);
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Order> found = orderMapper.findById("O000000001");
        assertThat(found).isPresent();
        assertThat(found.get().getOrderAmount()).isEqualTo(150000);
        assertThat(found.get().getConsumptionTax()).isEqualTo(15000);
    }

    @Test
    void testDelete() {
        // 受注を作成
        Order order = new Order();
        order.setOrderNo("O000000001");
        order.setOrderDate(LocalDateTime.of(2024, 1, 15, 10, 0));
        order.setDepartmentCode("10000");
        order.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        order.setCustomerCode("00000001");
        order.setCustomerBranch(1);
        order.setEmployeeCode("0000000001");
        order.setRequiredDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        order.setWarehouseCode("001");
        order.setOrderAmount(100000);
        order.setConsumptionTax(10000);
        order.setCreatedBy("tester");
        order.setUpdatedBy("tester");
        orderMapper.insert(order);

        // 削除
        int result = orderMapper.delete("O000000001");
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Order> found = orderMapper.findById("O000000001");
        assertThat(found).isEmpty();
    }

    @Test
    void testFindByCustomerCode() {
        // 受注1を作成
        Order order1 = new Order();
        order1.setOrderNo("O000000001");
        order1.setOrderDate(LocalDateTime.of(2024, 1, 15, 10, 0));
        order1.setDepartmentCode("10000");
        order1.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        order1.setCustomerCode("00000001");
        order1.setCustomerBranch(1);
        order1.setEmployeeCode("0000000001");
        order1.setWarehouseCode("001");
        order1.setOrderAmount(100000);
        order1.setCreatedBy("tester");
        order1.setUpdatedBy("tester");
        orderMapper.insert(order1);

        // 受注2を作成
        Order order2 = new Order();
        order2.setOrderNo("O000000002");
        order2.setOrderDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        order2.setDepartmentCode("10000");
        order2.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        order2.setCustomerCode("00000001");
        order2.setCustomerBranch(1);
        order2.setEmployeeCode("0000000001");
        order2.setWarehouseCode("001");
        order2.setOrderAmount(200000);
        order2.setCreatedBy("tester");
        order2.setUpdatedBy("tester");
        orderMapper.insert(order2);

        // 顧客コードで検索
        List<Order> orders = orderMapper.findByCustomerCode("00000001");
        assertThat(orders).hasSize(2);
        assertThat(orders.get(0).getOrderNo()).isEqualTo("O000000001");
        assertThat(orders.get(1).getOrderNo()).isEqualTo("O000000002");
    }

    @Test
    void testFindAll() {
        // 受注1を作成
        Order order1 = new Order();
        order1.setOrderNo("O000000001");
        order1.setOrderDate(LocalDateTime.of(2024, 1, 15, 10, 0));
        order1.setDepartmentCode("10000");
        order1.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        order1.setCustomerCode("00000001");
        order1.setCustomerBranch(1);
        order1.setEmployeeCode("0000000001");
        order1.setWarehouseCode("001");
        order1.setOrderAmount(100000);
        order1.setCreatedBy("tester");
        order1.setUpdatedBy("tester");
        orderMapper.insert(order1);

        // 受注2を作成
        Order order2 = new Order();
        order2.setOrderNo("O000000002");
        order2.setOrderDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        order2.setDepartmentCode("10000");
        order2.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        order2.setCustomerCode("00000001");
        order2.setCustomerBranch(1);
        order2.setEmployeeCode("0000000001");
        order2.setWarehouseCode("001");
        order2.setOrderAmount(200000);
        order2.setCreatedBy("tester");
        order2.setUpdatedBy("tester");
        orderMapper.insert(order2);

        // 全件取得
        List<Order> orders = orderMapper.findAll();
        assertThat(orders).hasSize(2);
    }
}
