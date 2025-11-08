package com.example.sales.domain.model;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.repository.AlternateProductMapper;
import com.example.sales.domain.repository.ProductCategoryMapper;
import com.example.sales.domain.repository.ProductMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 代替商品のテスト(MyBatis版)
 */
@Transactional
class AlternateProductTest extends AbstractDatabaseTest {

    @Autowired
    private AlternateProductMapper alternateProductMapper;

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
    void 代替商品を登録できる() {
        setupProducts();

        AlternateProduct alternate = createTestAlternate("PROD001", "PROD002", 1);
        alternateProductMapper.insert(alternate);

        List<AlternateProduct> alternates = alternateProductMapper.findByProductCode("PROD001");
        assertThat(alternates).hasSize(1);
        assertThat(alternates.get(0).getAlternateProductCode()).isEqualTo("PROD002");
        assertThat(alternates.get(0).getPriority()).isEqualTo(1);
    }

    @Test
    void 優先順位付きで複数の代替商品を登録できる() {
        setupProducts();

        alternateProductMapper.insert(createTestAlternate("PROD001", "PROD002", 1));
        alternateProductMapper.insert(createTestAlternate("PROD001", "PROD003", 2));

        List<AlternateProduct> alternates = alternateProductMapper.findByProductCode("PROD001");
        assertThat(alternates).hasSize(2);
        assertThat(alternates.get(0).getPriority()).isEqualTo(1);
        assertThat(alternates.get(1).getPriority()).isEqualTo(2);
    }

    @Test
    void 代替商品を商品情報とJOINして取得できる() {
        setupProducts();

        alternateProductMapper.insert(createTestAlternate("PROD001", "PROD002", 1));
        alternateProductMapper.insert(createTestAlternate("PROD001", "PROD003", 2));

        List<Map<String, Object>> alternates = alternateProductMapper.findAlternatesWithProductInfo("PROD001");
        assertThat(alternates).hasSize(2);
        assertThat(alternates.get(0).get("商品正式名")).isEqualTo("ノートパソコン B型");
        assertThat(alternates.get(0).get("販売単価")).isEqualTo(160000);
        assertThat(alternates.get(1).get("商品正式名")).isEqualTo("ノートパソコン C型");
        assertThat(alternates.get(1).get("販売単価")).isEqualTo(140000);
    }

    private void setupProducts() {
        ProductCategory category = new ProductCategory();
        category.setProductCategoryCode("CAT001");
        category.setProductCategoryName("電子機器");
        category.setProductCategoryLevel(1);
        category.setProductCategoryPath("CAT001");
        category.setLowestLevelFlag(1);
        category.setCreatedAt(LocalDateTime.now());
        category.setCreatedBy("admin");
        category.setUpdatedAt(LocalDateTime.now());
        category.setUpdatedBy("admin");
        productCategoryMapper.insert(category);

        productMapper.insert(createProduct("PROD001", "ノートパソコン A型", 150000));
        productMapper.insert(createProduct("PROD002", "ノートパソコン B型", 160000));
        productMapper.insert(createProduct("PROD003", "ノートパソコン C型", 140000));
    }

    private Product createProduct(String code, String name, Integer price) {
        Product product = new Product();
        product.setProductCode(code);
        product.setProductFormalName(name);
        product.setProductAbbreviation("PC");
        product.setProductNameKana("ノートパソコン");
        product.setProductType("1");
        product.setModelNumber("MODEL-001");
        product.setSellingPrice(price);
        product.setPurchasePrice(100000);
        product.setCostOfSales(90000);
        product.setTaxType(1);
        product.setProductCategoryCode("CAT001");
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

    private AlternateProduct createTestAlternate(String productCode, String alternateCode, Integer priority) {
        AlternateProduct alternate = new AlternateProduct();
        alternate.setProductCode(productCode);
        alternate.setAlternateProductCode(alternateCode);
        alternate.setPriority(priority);
        alternate.setCreatedAt(LocalDateTime.now());
        alternate.setCreatedBy("admin");
        alternate.setUpdatedAt(LocalDateTime.now());
        alternate.setUpdatedBy("admin");
        return alternate;
    }
}
