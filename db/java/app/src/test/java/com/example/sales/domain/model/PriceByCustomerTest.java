package com.example.sales.domain.model;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.repository.PriceByCustomerMapper;
import com.example.sales.domain.repository.ProductCategoryMapper;
import com.example.sales.domain.repository.ProductMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 顧客別販売単価のテスト(MyBatis版)
 */
@Transactional
class PriceByCustomerTest extends AbstractDatabaseTest {

    @Autowired
    private PriceByCustomerMapper priceByCustomerMapper;

    @Autowired
    private ProductMapper productMapper;

    @Autowired
    private ProductCategoryMapper productCategoryMapper;

    @Test
    void 顧客別販売単価を登録できる() {
        // 前提：商品が存在
        setupProduct("PROD001");

        PriceByCustomer price = createTestPrice("PROD001", "CUST001", 140000);
        priceByCustomerMapper.insert(price);

        Optional<PriceByCustomer> found = priceByCustomerMapper.findById("PROD001", "CUST001");
        assertThat(found).isPresent();
        assertThat(found.get().getSellingPrice()).isEqualTo(140000);
    }

    @Test
    void 複数の顧客に異なる価格を設定できる() {
        setupProduct("PROD001");

        priceByCustomerMapper.insert(createTestPrice("PROD001", "CUST001", 130000)); // VIP価格
        priceByCustomerMapper.insert(createTestPrice("PROD001", "CUST002", 145000)); // 通常割引

        List<PriceByCustomer> prices = priceByCustomerMapper.findByProductCode("PROD001");
        assertThat(prices).hasSize(2);
        assertThat(prices.get(0).getSellingPrice()).isEqualTo(130000);
        assertThat(prices.get(1).getSellingPrice()).isEqualTo(145000);
    }

    @Test
    void 顧客別販売単価を更新できる() {
        setupProduct("PROD001");

        PriceByCustomer price = createTestPrice("PROD001", "CUST001", 140000);
        priceByCustomerMapper.insert(price);

        price.setSellingPrice(135000);
        price.setUpdatedAt(LocalDateTime.now());
        priceByCustomerMapper.update(price);

        Optional<PriceByCustomer> updated = priceByCustomerMapper.findById("PROD001", "CUST001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getSellingPrice()).isEqualTo(135000);
    }

    private void setupProduct(String productCode) {
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

        Product product = new Product();
        product.setProductCode(productCode);
        product.setProductFormalName("ノートパソコン");
        product.setProductAbbreviation("PC");
        product.setProductNameKana("ノートパソコン");
        product.setProductType("1");
        product.setModelNumber("MODEL-001");
        product.setSellingPrice(150000);
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
        productMapper.insert(product);
    }

    private PriceByCustomer createTestPrice(String productCode, String customerCode, Integer price) {
        PriceByCustomer priceByCustomer = new PriceByCustomer();
        priceByCustomer.setProductCode(productCode);
        priceByCustomer.setCustomerCode(customerCode);
        priceByCustomer.setSellingPrice(price);
        priceByCustomer.setCreatedAt(LocalDateTime.now());
        priceByCustomer.setCreatedBy("admin");
        priceByCustomer.setUpdatedAt(LocalDateTime.now());
        priceByCustomer.setUpdatedBy("admin");
        return priceByCustomer;
    }
}
