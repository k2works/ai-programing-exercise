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
 * 発注データ明細Mapperのテストクラス
 */
class PurchaseOrderDetailMapperTest extends AbstractDatabaseTest {

    @Autowired
    private PurchaseOrderDetailMapper purchaseOrderDetailMapper;

    @Autowired
    private PurchaseOrderMapper purchaseOrderMapper;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private SupplierMapper supplierMapper;

    @Autowired
    private WarehouseMapper warehouseMapper;

    @Autowired
    private ProductCategoryMapper productCategoryMapper;

    @Autowired
    private ProductMapper productMapper;

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

        // 商品分類と商品を作成
        createTestProductAndCategory();

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
        // 発注明細を作成
        PurchaseOrderDetail detail = new PurchaseOrderDetail();
        detail.setPoNo("PO00000001");
        detail.setPoLineNo(1);
        detail.setProductCode("P0000000001");
        detail.setProductName("テスト商品");
        detail.setPoUnitPrice(800);
        detail.setPoQuantity(10);
        detail.setReceivedQuantity(0);
        detail.setCompletedFlag(0);
        detail.setCreatedBy("tester");
        detail.setUpdatedBy("tester");

        // 挿入
        int result = purchaseOrderDetailMapper.insert(detail);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<PurchaseOrderDetail> found =
            purchaseOrderDetailMapper.findById("PO00000001", 1);
        assertThat(found).isPresent();
        assertThat(found.get().getPoNo()).isEqualTo("PO00000001");
        assertThat(found.get().getPoLineNo()).isEqualTo(1);
        assertThat(found.get().getPoQuantity()).isEqualTo(10);
    }

    @Test
    void testUpdate() {
        // 発注明細を作成
        PurchaseOrderDetail detail = new PurchaseOrderDetail();
        detail.setPoNo("PO00000001");
        detail.setPoLineNo(1);
        detail.setProductCode("P0000000001");
        detail.setProductName("テスト商品");
        detail.setPoUnitPrice(800);
        detail.setPoQuantity(10);
        detail.setReceivedQuantity(0);
        detail.setCompletedFlag(0);
        detail.setCreatedBy("tester");
        detail.setUpdatedBy("tester");
        purchaseOrderDetailMapper.insert(detail);

        // 更新
        detail.setPoQuantity(15);
        detail.setReceivedQuantity(5);
        detail.setUpdatedBy("updater");
        int result = purchaseOrderDetailMapper.update(detail);
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<PurchaseOrderDetail> found =
            purchaseOrderDetailMapper.findById("PO00000001", 1);
        assertThat(found).isPresent();
        assertThat(found.get().getPoQuantity()).isEqualTo(15);
        assertThat(found.get().getReceivedQuantity()).isEqualTo(5);
    }

    @Test
    void testDelete() {
        // 発注明細を作成
        PurchaseOrderDetail detail = new PurchaseOrderDetail();
        detail.setPoNo("PO00000001");
        detail.setPoLineNo(1);
        detail.setProductCode("P0000000001");
        detail.setProductName("テスト商品");
        detail.setPoUnitPrice(800);
        detail.setPoQuantity(10);
        detail.setReceivedQuantity(0);
        detail.setCompletedFlag(0);
        detail.setCreatedBy("tester");
        detail.setUpdatedBy("tester");
        purchaseOrderDetailMapper.insert(detail);

        // 削除
        int result = purchaseOrderDetailMapper.delete("PO00000001", 1);
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<PurchaseOrderDetail> found =
            purchaseOrderDetailMapper.findById("PO00000001", 1);
        assertThat(found).isEmpty();
    }

    @Test
    void testFindByPoNo() {
        // 発注明細1を作成
        PurchaseOrderDetail detail1 = new PurchaseOrderDetail();
        detail1.setPoNo("PO00000001");
        detail1.setPoLineNo(1);
        detail1.setProductCode("P0000000001");
        detail1.setProductName("テスト商品");
        detail1.setPoUnitPrice(800);
        detail1.setPoQuantity(10);
        detail1.setReceivedQuantity(0);
        detail1.setCompletedFlag(0);
        detail1.setCreatedBy("tester");
        detail1.setUpdatedBy("tester");
        purchaseOrderDetailMapper.insert(detail1);

        // 発注明細2を作成
        PurchaseOrderDetail detail2 = new PurchaseOrderDetail();
        detail2.setPoNo("PO00000001");
        detail2.setPoLineNo(2);
        detail2.setProductCode("P0000000001");
        detail2.setProductName("テスト商品");
        detail2.setPoUnitPrice(800);
        detail2.setPoQuantity(5);
        detail2.setReceivedQuantity(0);
        detail2.setCompletedFlag(0);
        detail2.setCreatedBy("tester");
        detail2.setUpdatedBy("tester");
        purchaseOrderDetailMapper.insert(detail2);

        // 発注番号で検索
        List<PurchaseOrderDetail> details =
            purchaseOrderDetailMapper.findByPoNo("PO00000001");
        assertThat(details).hasSize(2);
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

    private void createTestProductAndCategory() {
        // 商品分類を作成
        ProductCategory category = new ProductCategory();
        category.setProductCategoryCode("01");
        category.setProductCategoryName("テスト分類");
        category.setProductCategoryLevel(1);
        category.setProductCategoryPath("01");
        category.setLowestLevelFlag(1);
        category.setCreatedBy("tester");
        category.setUpdatedBy("tester");
        productCategoryMapper.insert(category);

        // 商品を作成
        Product product = new Product();
        product.setProductCode("P0000000001");
        product.setProductFormalName("テスト商品");
        product.setProductAbbreviation("テスト");
        product.setProductNameKana("テストショウヒン");
        product.setProductType("1");
        product.setModelNumber("MODEL-001");
        product.setSellingPrice(1000);
        product.setPurchasePrice(700);
        product.setCostOfSales(600);
        product.setTaxType(1);
        product.setProductCategoryCode("01");
        product.setMiscellaneousType(0);
        product.setInventoryManagementFlag(1);
        product.setInventoryAllocationFlag(0);
        product.setSupplierCode("00000001");
        product.setSupplierBranch(0);
        product.setCreatedBy("tester");
        product.setUpdatedBy("tester");
        productMapper.insert(product);
    }
}
