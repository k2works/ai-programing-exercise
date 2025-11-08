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
 * 在庫データMapperのテストクラス
 */
class StockMapperTest extends AbstractDatabaseTest {

    @Autowired
    private StockMapper stockMapper;

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

        // 部門を作成
        Department department = new Department();
        department.setDepartmentCode("10000");
        department.setStartDate(LocalDate.of(2024, 1, 1));
        department.setEndDate(LocalDate.of(9999, 12, 31));
        department.setDepartmentName("物流部");
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
    }

    @Test
    void testInsertAndFindById() {
        // 在庫を作成（5つのフィールドの複合主キー）
        Stock stock = new Stock();
        stock.setWarehouseCode("001");
        stock.setProductCode("P0000000001");
        stock.setLotNo("LOT20240101");
        stock.setStockType("1");
        stock.setQualityType("G");
        stock.setActualQuantity(100);
        stock.setValidQuantity(100);
        stock.setCreatedBy("tester");
        stock.setUpdatedBy("tester");

        // 挿入
        int result = stockMapper.insert(stock);
        assertThat(result).isEqualTo(1);

        // 取得（5つのパラメータで検索）
        Optional<Stock> found = stockMapper.findById("001", "P0000000001",
                                                       "LOT20240101", "1", "G");
        assertThat(found).isPresent();
        assertThat(found.get().getWarehouseCode()).isEqualTo("001");
        assertThat(found.get().getProductCode()).isEqualTo("P0000000001");
        assertThat(found.get().getLotNo()).isEqualTo("LOT20240101");
        assertThat(found.get().getStockType()).isEqualTo("1");
        assertThat(found.get().getQualityType()).isEqualTo("G");
        assertThat(found.get().getActualQuantity()).isEqualTo(100);
        assertThat(found.get().getValidQuantity()).isEqualTo(100);
    }

    @Test
    void testUpdate() {
        // 在庫を作成
        Stock stock = new Stock();
        stock.setWarehouseCode("001");
        stock.setProductCode("P0000000001");
        stock.setLotNo("LOT20240101");
        stock.setStockType("1");
        stock.setQualityType("G");
        stock.setActualQuantity(100);
        stock.setValidQuantity(100);
        stock.setCreatedBy("tester");
        stock.setUpdatedBy("tester");
        stockMapper.insert(stock);

        // 更新
        stock.setActualQuantity(80);
        stock.setValidQuantity(80);
        stock.setUpdatedBy("updater");
        int result = stockMapper.update(stock);
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Stock> found = stockMapper.findById("001", "P0000000001",
                                                       "LOT20240101", "1", "G");
        assertThat(found).isPresent();
        assertThat(found.get().getActualQuantity()).isEqualTo(80);
        assertThat(found.get().getValidQuantity()).isEqualTo(80);
    }

    @Test
    void testDelete() {
        // 在庫を作成
        Stock stock = new Stock();
        stock.setWarehouseCode("001");
        stock.setProductCode("P0000000001");
        stock.setLotNo("LOT20240101");
        stock.setStockType("1");
        stock.setQualityType("G");
        stock.setActualQuantity(100);
        stock.setValidQuantity(100);
        stock.setCreatedBy("tester");
        stock.setUpdatedBy("tester");
        stockMapper.insert(stock);

        // 削除（5つのパラメータを指定）
        int result = stockMapper.delete("001", "P0000000001",
                                        "LOT20240101", "1", "G");
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Stock> found = stockMapper.findById("001", "P0000000001",
                                                       "LOT20240101", "1", "G");
        assertThat(found).isEmpty();
    }

    @Test
    void testFindByWarehouseCode() {
        // 在庫1を作成
        Stock stock1 = new Stock();
        stock1.setWarehouseCode("001");
        stock1.setProductCode("P0000000001");
        stock1.setLotNo("LOT20240101");
        stock1.setStockType("1");
        stock1.setQualityType("G");
        stock1.setActualQuantity(100);
        stock1.setValidQuantity(100);
        stock1.setCreatedBy("tester");
        stock1.setUpdatedBy("tester");
        stockMapper.insert(stock1);

        // 在庫2を作成（同じ倉庫、違うロット）
        Stock stock2 = new Stock();
        stock2.setWarehouseCode("001");
        stock2.setProductCode("P0000000001");
        stock2.setLotNo("LOT20240102");
        stock2.setStockType("1");
        stock2.setQualityType("G");
        stock2.setActualQuantity(50);
        stock2.setValidQuantity(50);
        stock2.setCreatedBy("tester");
        stock2.setUpdatedBy("tester");
        stockMapper.insert(stock2);

        // 倉庫コードで検索
        List<Stock> stocks = stockMapper.findByWarehouseCode("001");
        assertThat(stocks).hasSize(2);
    }

    @Test
    void testFindByProductCode() {
        // 在庫1を作成
        Stock stock1 = new Stock();
        stock1.setWarehouseCode("001");
        stock1.setProductCode("P0000000001");
        stock1.setLotNo("LOT20240101");
        stock1.setStockType("1");
        stock1.setQualityType("G");
        stock1.setActualQuantity(100);
        stock1.setValidQuantity(100);
        stock1.setCreatedBy("tester");
        stock1.setUpdatedBy("tester");
        stockMapper.insert(stock1);

        // 商品コードで検索
        List<Stock> stocks = stockMapper.findByProductCode("P0000000001");
        assertThat(stocks).hasSize(1);
        assertThat(stocks.get(0).getProductCode()).isEqualTo("P0000000001");
    }

    @Test
    void testUpdateValidQuantity() {
        // 在庫を作成
        Stock stock = new Stock();
        stock.setWarehouseCode("001");
        stock.setProductCode("P0000000001");
        stock.setLotNo("LOT20240101");
        stock.setStockType("1");
        stock.setQualityType("G");
        stock.setActualQuantity(100);
        stock.setValidQuantity(100);
        stock.setCreatedBy("tester");
        stock.setUpdatedBy("tester");
        stockMapper.insert(stock);

        // 有効在庫数を更新
        int result = stockMapper.updateValidQuantity("001", "P0000000001",
                                                      "LOT20240101", "1", "G", 70);
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Stock> found = stockMapper.findById("001", "P0000000001",
                                                       "LOT20240101", "1", "G");
        assertThat(found).isPresent();
        assertThat(found.get().getValidQuantity()).isEqualTo(70);
        assertThat(found.get().getActualQuantity()).isEqualTo(100); // 実在庫は変わらない
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
