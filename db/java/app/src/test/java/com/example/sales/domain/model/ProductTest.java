package com.example.sales.domain.model;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.repository.ProductCategoryMapper;
import com.example.sales.domain.repository.ProductMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 商品マスタのテスト(MyBatis版)
 */
@Transactional
class ProductTest extends AbstractDatabaseTest {

    @Autowired
    private ProductMapper productMapper;

    @Autowired
    private ProductCategoryMapper productCategoryMapper;

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
        jdbcTemplate.execute("DELETE FROM 顧客別販売単価");
        jdbcTemplate.execute("DELETE FROM 代替商品");
        jdbcTemplate.execute("DELETE FROM 商品マスタ");
        jdbcTemplate.execute("DELETE FROM 商品分類マスタ");
    }

    @Test
    void 商品を登録できる() {
        // 前提：商品分類が存在
        ProductCategory category = createTestCategory("CAT001", "電子機器");
        productCategoryMapper.insert(category);

        Product product = createTestProduct("PROD001", "ノートパソコン A型", "CAT001");
        productMapper.insert(product);

        Optional<Product> found = productMapper.findById("PROD001");
        assertThat(found).isPresent();
        assertThat(found.get().getProductFormalName()).isEqualTo("ノートパソコン A型");
        assertThat(found.get().getSellingPrice()).isEqualTo(150000);
    }

    @Test
    void 商品を更新できる() {
        ProductCategory category = createTestCategory("CAT001", "電子機器");
        productCategoryMapper.insert(category);

        Product product = createTestProduct("PROD001", "ノートパソコン A型", "CAT001");
        productMapper.insert(product);

        product.setSellingPrice(160000);
        product.setUpdatedAt(LocalDateTime.now());
        product.setUpdatedBy("updater");
        productMapper.update(product);

        Optional<Product> updated = productMapper.findById("PROD001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getSellingPrice()).isEqualTo(160000);
    }

    @Test
    void 商品を削除できる() {
        ProductCategory category = createTestCategory("CAT001", "電子機器");
        productCategoryMapper.insert(category);

        Product product = createTestProduct("PROD001", "ノートパソコン A型", "CAT001");
        productMapper.insert(product);

        productMapper.delete("PROD001");

        Optional<Product> deleted = productMapper.findById("PROD001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void 商品分類コードで商品を検索できる() {
        ProductCategory category = createTestCategory("CAT001", "電子機器");
        productCategoryMapper.insert(category);

        productMapper.insert(createTestProduct("PROD001", "ノートPC A型", "CAT001"));
        productMapper.insert(createTestProduct("PROD002", "ノートPC B型", "CAT001"));

        List<Product> products = productMapper.findByCategory("CAT001");
        assertThat(products).hasSize(2);
    }

    private ProductCategory createTestCategory(String code, String name) {
        ProductCategory category = new ProductCategory();
        category.setProductCategoryCode(code);
        category.setProductCategoryName(name);
        category.setProductCategoryLevel(1);
        category.setProductCategoryPath(code);
        category.setLowestLevelFlag(1);
        category.setCreatedAt(LocalDateTime.now());
        category.setCreatedBy("admin");
        category.setUpdatedAt(LocalDateTime.now());
        category.setUpdatedBy("admin");
        return category;
    }

    private Product createTestProduct(String code, String name, String categoryCode) {
        Product product = new Product();
        product.setProductCode(code);
        product.setProductFormalName(name);
        product.setProductAbbreviation("PC-A");
        product.setProductNameKana("ノートパソコン");
        product.setProductType("1");
        product.setModelNumber("MODEL-A-001");
        product.setSellingPrice(150000);
        product.setPurchasePrice(100000);
        product.setCostOfSales(90000);
        product.setTaxType(1);
        product.setProductCategoryCode(categoryCode);
        product.setMiscellaneousType(0);
        product.setInventoryManagementFlag(1);
        product.setInventoryAllocationFlag(0);
        product.setSupplierCode("SUP001");
        product.setSupplierBranch(1);
        product.setCreatedAt(LocalDateTime.now());
        product.setCreatedBy("admin");
        product.setUpdatedAt(LocalDateTime.now());
        product.setUpdatedBy("admin");
        return product;
    }
}
