package com.example.sales.domain.model;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.repository.ProductCategoryMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 商品分類マスタのテスト(MyBatis版)
 */
@Transactional
class ProductCategoryTest extends AbstractDatabaseTest {

    @Autowired
    private ProductCategoryMapper productCategoryMapper;

    @Test
    void 商品分類を登録できる() {
        ProductCategory category = createTestCategory("CAT001", "電子機器", 1, "CAT001");
        productCategoryMapper.insert(category);

        Optional<ProductCategory> found = productCategoryMapper.findById("CAT001");
        assertThat(found).isPresent();
        assertThat(found.get().getProductCategoryName()).isEqualTo("電子機器");
    }

    @Test
    void 階層構造の商品分類を登録できる() {
        // 第1階層
        ProductCategory parent = createTestCategory("CAT001", "電子機器", 1, "CAT001");
        parent.setLowestLevelFlag(0);
        productCategoryMapper.insert(parent);

        // 第2階層
        ProductCategory child = createTestCategory("CAT00101", "パソコン", 2, "CAT001/CAT00101");
        child.setLowestLevelFlag(0);
        productCategoryMapper.insert(child);

        // 第3階層
        ProductCategory grandChild = createTestCategory("CAT0010101", "ノートPC", 3, "CAT001/CAT00101/CAT0010101");
        grandChild.setLowestLevelFlag(1);
        productCategoryMapper.insert(grandChild);

        List<ProductCategory> categories = productCategoryMapper.findAll();
        assertThat(categories).hasSize(3);
        assertThat(categories.get(0).getProductCategoryLevel()).isEqualTo(1);
        assertThat(categories.get(1).getProductCategoryLevel()).isEqualTo(2);
        assertThat(categories.get(2).getProductCategoryLevel()).isEqualTo(3);
    }

    @Test
    void 階層パスで配下の商品分類を検索できる() {
        productCategoryMapper.insert(createTestCategory("CAT001", "電子機器", 1, "CAT001"));
        productCategoryMapper.insert(createTestCategory("CAT00101", "パソコン", 2, "CAT001/CAT00101"));
        productCategoryMapper.insert(createTestCategory("CAT0010101", "ノートPC", 3, "CAT001/CAT00101/CAT0010101"));
        productCategoryMapper.insert(createTestCategory("CAT002", "家具", 1, "CAT002"));

        List<ProductCategory> underCat001 = productCategoryMapper.findByPathPrefix("CAT001");
        assertThat(underCat001).hasSize(3);
    }

    private ProductCategory createTestCategory(String code, String name, Integer level, String path) {
        ProductCategory category = new ProductCategory();
        category.setProductCategoryCode(code);
        category.setProductCategoryName(name);
        category.setProductCategoryLevel(level);
        category.setProductCategoryPath(path);
        category.setLowestLevelFlag(1);
        category.setCreatedAt(LocalDateTime.now());
        category.setCreatedBy("admin");
        category.setUpdatedAt(LocalDateTime.now());
        category.setUpdatedBy("admin");
        return category;
    }
}
