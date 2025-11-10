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
 * 売上データ明細Mapperのテストクラス
 */
class SalesDetailMapperTest extends AbstractDatabaseTest {

    @Autowired
    private SalesDetailMapper salesDetailMapper;

    @Autowired
    private SalesMapper salesMapper;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private ProductMapper productMapper;

    @Autowired
    private ProductCategoryMapper productCategoryMapper;

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
        jdbcTemplate.execute("DELETE FROM 商品マスタ");
        jdbcTemplate.execute("DELETE FROM 商品分類マスタ");
        jdbcTemplate.execute("DELETE FROM 倉庫マスタ");
        jdbcTemplate.execute("DELETE FROM 顧客マスタ");
        jdbcTemplate.execute("DELETE FROM 仕入先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類所属マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先グループマスタ");
        jdbcTemplate.execute("DELETE FROM 社員マスタ");
        jdbcTemplate.execute("DELETE FROM 部門マスタ");

        // 商品分類を作成
        ProductCategory category = new ProductCategory();
        category.setProductCategoryCode("CAT001");
        category.setProductCategoryName("電子機器");
        category.setProductCategoryLevel(1);
        category.setProductCategoryPath("CAT001");
        category.setLowestLevelFlag(1);
        category.setCreatedBy("tester");
        category.setUpdatedBy("tester");
        productCategoryMapper.insert(category);

        // 商品を作成
        Product product = new Product();
        product.setProductCode("PROD001");
        product.setProductAbbreviation("PC-A");
        product.setProductFormalName("ノートパソコン A型");
        product.setProductNameKana("ノートパソコンエーガタ");
        product.setProductCategoryCode("CAT001");
        product.setProductType("1");
        product.setSellingPrice(150000);
        product.setCostOfSales(100000);
        product.setTaxType(1);
        product.setInventoryManagementFlag(1);
        product.setCreatedBy("tester");
        product.setUpdatedBy("tester");
        productMapper.insert(product);

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
        sales.setSalesAmount(0);
        sales.setConsumptionTax(0);
        sales.setCreatedBy("tester");
        sales.setUpdatedBy("tester");
        salesMapper.insert(sales);
    }

    @Test
    void testInsertAndFindById() {
        // 売上明細を作成
        SalesDetail detail = new SalesDetail();
        detail.setSalesNo("S000000001");
        detail.setSalesRowNo(1);
        detail.setProductCode("PROD001");
        detail.setProductName("ノートパソコン A型");
        detail.setUnitPrice(150000);
        detail.setShippedQuantity(2);
        detail.setSalesQuantity(2);
        detail.setDiscount(0);
        detail.setCreatedBy("tester");
        detail.setUpdatedBy("tester");

        // 挿入
        int result = salesDetailMapper.insert(detail);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<SalesDetail> found = salesDetailMapper.findById("S000000001", 1);
        assertThat(found).isPresent();
        assertThat(found.get().getSalesNo()).isEqualTo("S000000001");
        assertThat(found.get().getSalesRowNo()).isEqualTo(1);
        assertThat(found.get().getProductCode()).isEqualTo("PROD001");
        assertThat(found.get().getSalesQuantity()).isEqualTo(2);
    }

    @Test
    void testUpdate() {
        // 売上明細を作成
        SalesDetail detail = new SalesDetail();
        detail.setSalesNo("S000000001");
        detail.setSalesRowNo(1);
        detail.setProductCode("PROD001");
        detail.setProductName("ノートパソコン A型");
        detail.setUnitPrice(150000);
        detail.setShippedQuantity(2);
        detail.setSalesQuantity(2);
        detail.setDiscount(0);
        detail.setCreatedBy("tester");
        detail.setUpdatedBy("tester");
        salesDetailMapper.insert(detail);

        // 更新
        detail.setSalesQuantity(3);
        detail.setUpdatedBy("updater");
        int result = salesDetailMapper.update(detail);
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<SalesDetail> found = salesDetailMapper.findById("S000000001", 1);
        assertThat(found).isPresent();
        assertThat(found.get().getSalesQuantity()).isEqualTo(3);
    }

    @Test
    void testDelete() {
        // 売上明細を作成
        SalesDetail detail = new SalesDetail();
        detail.setSalesNo("S000000001");
        detail.setSalesRowNo(1);
        detail.setProductCode("PROD001");
        detail.setProductName("ノートパソコン A型");
        detail.setUnitPrice(150000);
        detail.setShippedQuantity(2);
        detail.setSalesQuantity(2);
        detail.setDiscount(0);
        detail.setCreatedBy("tester");
        detail.setUpdatedBy("tester");
        salesDetailMapper.insert(detail);

        // 削除
        int result = salesDetailMapper.delete("S000000001", 1);
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<SalesDetail> found = salesDetailMapper.findById("S000000001", 1);
        assertThat(found).isEmpty();
    }

    @Test
    void testFindBySalesNo() {
        // 売上明細1を作成
        SalesDetail detail1 = new SalesDetail();
        detail1.setSalesNo("S000000001");
        detail1.setSalesRowNo(1);
        detail1.setProductCode("PROD001");
        detail1.setProductName("ノートパソコン A型");
        detail1.setUnitPrice(150000);
        detail1.setShippedQuantity(2);
        detail1.setSalesQuantity(2);
        detail1.setDiscount(0);
        detail1.setCreatedBy("tester");
        detail1.setUpdatedBy("tester");
        salesDetailMapper.insert(detail1);

        // 売上明細2を作成
        SalesDetail detail2 = new SalesDetail();
        detail2.setSalesNo("S000000001");
        detail2.setSalesRowNo(2);
        detail2.setProductCode("PROD001");
        detail2.setProductName("ノートパソコン A型");
        detail2.setUnitPrice(150000);
        detail2.setShippedQuantity(1);
        detail2.setSalesQuantity(1);
        detail2.setDiscount(0);
        detail2.setCreatedBy("tester");
        detail2.setUpdatedBy("tester");
        salesDetailMapper.insert(detail2);

        // 売上番号で検索
        List<SalesDetail> details = salesDetailMapper.findBySalesNo("S000000001");
        assertThat(details).hasSize(2);
        assertThat(details.get(0).getSalesRowNo()).isEqualTo(1);
        assertThat(details.get(1).getSalesRowNo()).isEqualTo(2);
    }

    @Test
    void testFindByProductCode() {
        // 売上明細を作成
        SalesDetail detail = new SalesDetail();
        detail.setSalesNo("S000000001");
        detail.setSalesRowNo(1);
        detail.setProductCode("PROD001");
        detail.setProductName("ノートパソコン A型");
        detail.setUnitPrice(150000);
        detail.setShippedQuantity(2);
        detail.setSalesQuantity(2);
        detail.setDiscount(0);
        detail.setCreatedBy("tester");
        detail.setUpdatedBy("tester");
        salesDetailMapper.insert(detail);

        // 商品コードで検索
        List<SalesDetail> details = salesDetailMapper.findByProductCode("PROD001");
        assertThat(details).hasSize(1);
        assertThat(details.get(0).getSalesNo()).isEqualTo("S000000001");
    }

    @Test
    void testFindAll() {
        // 売上明細を作成
        SalesDetail detail = new SalesDetail();
        detail.setSalesNo("S000000001");
        detail.setSalesRowNo(1);
        detail.setProductCode("PROD001");
        detail.setProductName("ノートパソコン A型");
        detail.setUnitPrice(150000);
        detail.setShippedQuantity(2);
        detail.setSalesQuantity(2);
        detail.setDiscount(0);
        detail.setCreatedBy("tester");
        detail.setUpdatedBy("tester");
        salesDetailMapper.insert(detail);

        // 全件取得
        List<SalesDetail> details = salesDetailMapper.findAll();
        assertThat(details).hasSize(1);
    }
}
