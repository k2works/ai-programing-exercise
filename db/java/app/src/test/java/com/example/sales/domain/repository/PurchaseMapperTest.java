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
 * 仕入データMapperのテストクラス
 */
class PurchaseMapperTest extends AbstractDatabaseTest {

    @Autowired
    private PurchaseMapper purchaseMapper;

    @Autowired
    private PurchaseOrderMapper purchaseOrderMapper;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private SupplierMapper supplierMapper;

    @Autowired
    private WarehouseMapper warehouseMapper;

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
        // テストデータをクリア（外部キー制約を考慮した順序）
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
        jdbcTemplate.execute("DELETE FROM 売上データ明細");
        jdbcTemplate.execute("DELETE FROM 売上データ");
        jdbcTemplate.execute("DELETE FROM 受注データ明細");
        jdbcTemplate.execute("DELETE FROM 受注データ");
        jdbcTemplate.execute("DELETE FROM 顧客別販売単価");
        jdbcTemplate.execute("DELETE FROM 代替商品");
        jdbcTemplate.execute("DELETE FROM 商品マスタ");
        jdbcTemplate.execute("DELETE FROM 商品分類マスタ");
        jdbcTemplate.execute("DELETE FROM 倉庫マスタ");
        jdbcTemplate.execute("DELETE FROM 仕入先マスタ");
        jdbcTemplate.execute("DELETE FROM 顧客マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類所属マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先グループマスタ");
        jdbcTemplate.execute("DELETE FROM 社員マスタ");
        jdbcTemplate.execute("DELETE FROM 部門マスタ");

        setupBaseData();
    }

    private void setupBaseData() {
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

        // 仕入先を作成
        Supplier supplier = createTestSupplier();
        supplierMapper.insert(supplier);

        // 倉庫を作成
        Warehouse warehouse = new Warehouse();
        warehouse.setWarehouseCode("001");
        warehouse.setWarehouseName("東京倉庫");
        warehouse.setWarehouseType(1);
        warehouse.setManagerCode("0000000001");
        warehouse.setCreatedBy("tester");
        warehouse.setUpdatedBy("tester");
        warehouseMapper.insert(warehouse);

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
        order.setCreatedBy("tester");
        order.setUpdatedBy("tester");
        orderMapper.insert(order);

        // 発注を作成
        PurchaseOrder po = new PurchaseOrder();
        po.setPoNo("PO00000001");
        po.setPoDate(LocalDateTime.of(2024, 1, 16, 10, 0));
        po.setOrderNo("O000000001");
        po.setSupplierCode("00000001");
        po.setSupplierBranch(0);
        po.setEmployeeCode("0000000001");
        po.setWarehouseCode("001");
        po.setPoAmount(80000);
        po.setCreatedBy("tester");
        po.setUpdatedBy("tester");
        purchaseOrderMapper.insert(po);
    }

    @Test
    void testInsertAndFindById() {
        // 仕入を作成
        Purchase purchase = new Purchase();
        purchase.setPurchaseNo("PR00000001");
        purchase.setPurchaseDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        purchase.setSupplierCode("00000001");
        purchase.setSupplierBranch(0);
        purchase.setEmployeeCode("0000000001");
        purchase.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        purchase.setPoNo("PO00000001");
        purchase.setDepartmentCode("10000");
        purchase.setPurchaseAmount(80000);
        purchase.setConsumptionTax(8000);
        purchase.setSlipComment("テスト仕入");
        purchase.setCreatedBy("tester");
        purchase.setUpdatedBy("tester");

        // 挿入
        int result = purchaseMapper.insert(purchase);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<Purchase> found = purchaseMapper.findById("PR00000001");
        assertThat(found).isPresent();
        assertThat(found.get().getPurchaseNo()).isEqualTo("PR00000001");
        assertThat(found.get().getPoNo()).isEqualTo("PO00000001");
        assertThat(found.get().getPurchaseAmount()).isEqualTo(80000);
    }

    @Test
    void testUpdate() {
        // 仕入を作成
        Purchase purchase = new Purchase();
        purchase.setPurchaseNo("PR00000001");
        purchase.setPurchaseDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        purchase.setSupplierCode("00000001");
        purchase.setSupplierBranch(0);
        purchase.setEmployeeCode("0000000001");
        purchase.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        purchase.setPoNo("PO00000001");
        purchase.setDepartmentCode("10000");
        purchase.setPurchaseAmount(80000);
        purchase.setConsumptionTax(8000);
        purchase.setCreatedBy("tester");
        purchase.setUpdatedBy("tester");
        purchaseMapper.insert(purchase);

        // 更新
        purchase.setPurchaseAmount(85000);
        purchase.setConsumptionTax(8500);
        purchase.setUpdatedBy("updater");
        int result = purchaseMapper.update(purchase);
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Purchase> found = purchaseMapper.findById("PR00000001");
        assertThat(found).isPresent();
        assertThat(found.get().getPurchaseAmount()).isEqualTo(85000);
        assertThat(found.get().getConsumptionTax()).isEqualTo(8500);
    }

    @Test
    void testDelete() {
        // 仕入を作成
        Purchase purchase = new Purchase();
        purchase.setPurchaseNo("PR00000001");
        purchase.setPurchaseDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        purchase.setSupplierCode("00000001");
        purchase.setSupplierBranch(0);
        purchase.setEmployeeCode("0000000001");
        purchase.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        purchase.setPoNo("PO00000001");
        purchase.setDepartmentCode("10000");
        purchase.setPurchaseAmount(80000);
        purchase.setCreatedBy("tester");
        purchase.setUpdatedBy("tester");
        purchaseMapper.insert(purchase);

        // 削除
        int result = purchaseMapper.delete("PR00000001");
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Purchase> found = purchaseMapper.findById("PR00000001");
        assertThat(found).isEmpty();
    }

    @Test
    void testFindBySupplierCode() {
        // 仕入を作成
        Purchase purchase = new Purchase();
        purchase.setPurchaseNo("PR00000001");
        purchase.setPurchaseDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        purchase.setSupplierCode("00000001");
        purchase.setSupplierBranch(0);
        purchase.setEmployeeCode("0000000001");
        purchase.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        purchase.setDepartmentCode("10000");
        purchase.setPurchaseAmount(80000);
        purchase.setCreatedBy("tester");
        purchase.setUpdatedBy("tester");
        purchaseMapper.insert(purchase);

        // 仕入先コードで検索
        List<Purchase> purchases = purchaseMapper.findBySupplierCode("00000001");
        assertThat(purchases).hasSize(1);
        assertThat(purchases.get(0).getPurchaseNo()).isEqualTo("PR00000001");
    }

    @Test
    void testFindByPoNo() {
        // 仕入を作成
        Purchase purchase = new Purchase();
        purchase.setPurchaseNo("PR00000001");
        purchase.setPurchaseDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        purchase.setSupplierCode("00000001");
        purchase.setSupplierBranch(0);
        purchase.setEmployeeCode("0000000001");
        purchase.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        purchase.setPoNo("PO00000001");
        purchase.setDepartmentCode("10000");
        purchase.setPurchaseAmount(80000);
        purchase.setCreatedBy("tester");
        purchase.setUpdatedBy("tester");
        purchaseMapper.insert(purchase);

        // 発注番号で検索
        List<Purchase> purchases = purchaseMapper.findByPoNo("PO00000001");
        assertThat(purchases).hasSize(1);
        assertThat(purchases.get(0).getPurchaseNo()).isEqualTo("PR00000001");
    }

    @Test
    void testFindAll() {
        // 仕入を作成
        Purchase purchase = new Purchase();
        purchase.setPurchaseNo("PR00000001");
        purchase.setPurchaseDate(LocalDateTime.of(2024, 1, 20, 10, 0));
        purchase.setSupplierCode("00000001");
        purchase.setSupplierBranch(0);
        purchase.setEmployeeCode("0000000001");
        purchase.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        purchase.setDepartmentCode("10000");
        purchase.setPurchaseAmount(80000);
        purchase.setCreatedBy("tester");
        purchase.setUpdatedBy("tester");
        purchaseMapper.insert(purchase);

        // 全件取得
        List<Purchase> purchases = purchaseMapper.findAll();
        assertThat(purchases).hasSize(1);
    }

    private Supplier createTestSupplier() {
        Supplier supplier = new Supplier();
        supplier.setSupplierCode("00000001");
        supplier.setSupplierBranch(0);
        supplier.setSupplierName("テスト仕入先");
        supplier.setSupplierNameKana("テストシイレサキ");
        supplier.setSupplierUserName("テスト担当者");
        supplier.setSupplierDepartmentName("購買部");
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
