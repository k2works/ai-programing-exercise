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
 * 請求データ明細Mapperのテストクラス
 */
class InvoiceDetailMapperTest extends AbstractDatabaseTest {

    @Autowired
    private InvoiceDetailMapper invoiceDetailMapper;

    @Autowired
    private InvoiceMapper invoiceMapper;

    @Autowired
    private SalesMapper salesMapper;

    @Autowired
    private SalesDetailMapper salesDetailMapper;

    @Autowired
    private CompanyMapper companyMapper;

    @Autowired
    private CompanyGroupMapper companyGroupMapper;

    @Autowired
    private DepartmentMapper departmentMapper;

    @Autowired
    private EmployeeMapper employeeMapper;

    @Autowired
    private CustomerMapper customerMapper;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private ProductMapper productMapper;

    @Autowired
    private ProductCategoryMapper productCategoryMapper;

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
        jdbcTemplate.execute("DELETE FROM 商品マスタ");
        jdbcTemplate.execute("DELETE FROM 商品分類マスタ");
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

        // 商品分類を作成
        ProductCategory category = new ProductCategory();
        category.setProductCategoryCode("CAT001");
        category.setProductCategoryName("テスト分類");
        category.setProductCategoryLevel(1);
        category.setProductCategoryPath("CAT001");
        category.setLowestLevelFlag(1);
        category.setCreatedBy("tester");
        category.setUpdatedBy("tester");
        productCategoryMapper.insert(category);

        // 商品を作成
        Product product = new Product();
        product.setProductCode("P0001");
        product.setProductAbbreviation("TEST");
        product.setProductFormalName("テスト商品");
        product.setProductNameKana("テストショウヒン");
        product.setProductCategoryCode("CAT001");
        product.setProductType("1");
        product.setSellingPrice(10000);
        product.setCostOfSales(8000);
        product.setTaxType(1);
        product.setInventoryManagementFlag(1);
        product.setCreatedBy("tester");
        product.setUpdatedBy("tester");
        productMapper.insert(product);

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

        // 受注データを作成
        Order order = new Order();
        order.setOrderNo("O000000001");
        order.setOrderDate(LocalDateTime.of(2024, 1, 10, 0, 0));
        order.setDepartmentCode("10000");
        order.setStartDate(LocalDateTime.of(2024, 1, 1, 0, 0));
        order.setCustomerCode("00000001");
        order.setCustomerBranch(1);
        order.setEmployeeCode("0000000001");
        order.setWarehouseCode("001");
        order.setCreatedBy("tester");
        order.setUpdatedBy("tester");
        orderMapper.insert(order);

        // 売上データを作成
        Sales sales = new Sales();
        sales.setSalesNo("S000000001");
        sales.setOrderNo("O000000001");
        sales.setSalesDate(LocalDateTime.of(2024, 1, 15, 0, 0));
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

        // 売上データ明細を作成
        SalesDetail salesDetail = new SalesDetail();
        salesDetail.setSalesNo("S000000001");
        salesDetail.setSalesRowNo(1);
        salesDetail.setProductCode("P0001");
        salesDetail.setProductName("テスト商品");
        salesDetail.setSalesQuantity(10);
        salesDetail.setUnitPrice(10000);
        salesDetail.setCreatedBy("tester");
        salesDetail.setUpdatedBy("tester");
        salesDetailMapper.insert(salesDetail);

        // 請求データを作成
        Invoice invoice = new Invoice();
        invoice.setInvoiceNo("I000000001");
        invoice.setInvoiceDate(LocalDateTime.of(2024, 1, 31, 0, 0));
        invoice.setCompanyCode("00000001");
        invoice.setCustomerBranch(0);
        invoice.setMonthInvoiceAmount(110000);
        invoice.setCreatedBy("tester");
        invoice.setUpdatedBy("tester");
        invoiceMapper.insert(invoice);
    }

    @Test
    void testInsertAndFindById() {
        // 請求明細を作成
        InvoiceDetail invoiceDetail = new InvoiceDetail();
        invoiceDetail.setInvoiceNo("I000000001");
        invoiceDetail.setInvoiceLineNo(1);
        invoiceDetail.setSalesNo("S000000001");
        invoiceDetail.setSalesLineNo(1);
        invoiceDetail.setInvoiceAmount(110000);
        invoiceDetail.setCreatedBy("tester");
        invoiceDetail.setUpdatedBy("tester");

        // 挿入
        int result = invoiceDetailMapper.insert(invoiceDetail);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<InvoiceDetail> found = invoiceDetailMapper.findById("I000000001", 1);
        assertThat(found).isPresent();
        assertThat(found.get().getInvoiceNo()).isEqualTo("I000000001");
        assertThat(found.get().getInvoiceLineNo()).isEqualTo(1);
        assertThat(found.get().getSalesNo()).isEqualTo("S000000001");
        assertThat(found.get().getInvoiceAmount()).isEqualTo(110000);
    }

    @Test
    void testUpdate() {
        // 請求明細を作成
        InvoiceDetail invoiceDetail = new InvoiceDetail();
        invoiceDetail.setInvoiceNo("I000000001");
        invoiceDetail.setInvoiceLineNo(1);
        invoiceDetail.setSalesNo("S000000001");
        invoiceDetail.setSalesLineNo(1);
        invoiceDetail.setInvoiceAmount(110000);
        invoiceDetail.setCreatedBy("tester");
        invoiceDetail.setUpdatedBy("tester");
        invoiceDetailMapper.insert(invoiceDetail);

        // 更新
        invoiceDetail.setInvoiceAmount(120000);
        int updateResult = invoiceDetailMapper.update(invoiceDetail);
        assertThat(updateResult).isEqualTo(1);

        // 確認
        Optional<InvoiceDetail> updated = invoiceDetailMapper.findById("I000000001", 1);
        assertThat(updated).isPresent();
        assertThat(updated.get().getInvoiceAmount()).isEqualTo(120000);
    }

    @Test
    void testDelete() {
        // 請求明細を作成
        InvoiceDetail invoiceDetail = new InvoiceDetail();
        invoiceDetail.setInvoiceNo("I000000001");
        invoiceDetail.setInvoiceLineNo(1);
        invoiceDetail.setSalesNo("S000000001");
        invoiceDetail.setSalesLineNo(1);
        invoiceDetail.setInvoiceAmount(110000);
        invoiceDetail.setCreatedBy("tester");
        invoiceDetail.setUpdatedBy("tester");
        invoiceDetailMapper.insert(invoiceDetail);

        // 削除
        int deleteResult = invoiceDetailMapper.delete("I000000001", 1);
        assertThat(deleteResult).isEqualTo(1);

        // 確認
        Optional<InvoiceDetail> deleted = invoiceDetailMapper.findById("I000000001", 1);
        assertThat(deleted).isEmpty();
    }

    @Test
    void testFindByInvoiceNo() {
        // 請求明細を2件作成
        InvoiceDetail detail1 = new InvoiceDetail();
        detail1.setInvoiceNo("I000000001");
        detail1.setInvoiceLineNo(1);
        detail1.setSalesNo("S000000001");
        detail1.setSalesLineNo(1);
        detail1.setInvoiceAmount(110000);
        detail1.setCreatedBy("tester");
        detail1.setUpdatedBy("tester");
        invoiceDetailMapper.insert(detail1);

        InvoiceDetail detail2 = new InvoiceDetail();
        detail2.setInvoiceNo("I000000001");
        detail2.setInvoiceLineNo(2);
        detail2.setSalesNo("S000000001");
        detail2.setSalesLineNo(1);
        detail2.setInvoiceAmount(50000);
        detail2.setCreatedBy("tester");
        detail2.setUpdatedBy("tester");
        invoiceDetailMapper.insert(detail2);

        // 請求番号で検索
        List<InvoiceDetail> details = invoiceDetailMapper.findByInvoiceNo("I000000001");
        assertThat(details).hasSize(2);
    }

    @Test
    void testFindBySalesDetail() {
        // 請求明細を作成
        InvoiceDetail invoiceDetail = new InvoiceDetail();
        invoiceDetail.setInvoiceNo("I000000001");
        invoiceDetail.setInvoiceLineNo(1);
        invoiceDetail.setSalesNo("S000000001");
        invoiceDetail.setSalesLineNo(1);
        invoiceDetail.setInvoiceAmount(110000);
        invoiceDetail.setCreatedBy("tester");
        invoiceDetail.setUpdatedBy("tester");
        invoiceDetailMapper.insert(invoiceDetail);

        // 売上番号と売上行番号で検索
        List<InvoiceDetail> details = invoiceDetailMapper.findBySalesDetail("S000000001", 1);
        assertThat(details).hasSize(1);
        assertThat(details.get(0).getInvoiceNo()).isEqualTo("I000000001");
    }
}
