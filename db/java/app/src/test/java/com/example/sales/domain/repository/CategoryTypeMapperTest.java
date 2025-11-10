package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.CategoryType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 取引先分類種別マスタMapperのテストクラス
 */
class CategoryTypeMapperTest extends AbstractDatabaseTest {

    @Autowired
    private CategoryTypeMapper categoryTypeMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        // テストデータをクリア（外部キー制約により、参照元を先に削除）
        // 第7章のテーブル
        jdbcTemplate.execute("DELETE FROM 与信残高データ");
        jdbcTemplate.execute("DELETE FROM 自動採番マスタ");

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
        jdbcTemplate.execute("DELETE FROM 取引先分類所属マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類種別マスタ");
    }

    @Test
    void 取引先分類種別を登録できる() {
        // Given
        CategoryType categoryType = createTestCategoryType("01");

        // When
        int result = categoryTypeMapper.insert(categoryType);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CategoryType> saved = categoryTypeMapper.findById("01");
        assertThat(saved).isPresent();
        assertThat(saved.get().getCategoryTypeCode()).isEqualTo("01");
        assertThat(saved.get().getCategoryTypeName()).isEqualTo("業種");
    }

    @Test
    void 取引先分類種別を更新できる() {
        // Given
        CategoryType categoryType = createTestCategoryType("01");
        categoryTypeMapper.insert(categoryType);

        // When
        categoryType.setCategoryTypeName("地域");
        categoryType.setUpdatedBy("updater");
        int result = categoryTypeMapper.update(categoryType);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CategoryType> updated = categoryTypeMapper.findById("01");
        assertThat(updated).isPresent();
        assertThat(updated.get().getCategoryTypeName()).isEqualTo("地域");
        assertThat(updated.get().getUpdatedBy()).isEqualTo("updater");
    }

    @Test
    void 取引先分類種別を削除できる() {
        // Given
        CategoryType categoryType = createTestCategoryType("01");
        categoryTypeMapper.insert(categoryType);

        // When
        int result = categoryTypeMapper.delete("01");

        // Then
        assertThat(result).isEqualTo(1);

        Optional<CategoryType> deleted = categoryTypeMapper.findById("01");
        assertThat(deleted).isEmpty();
    }

    @Test
    void 取引先分類種別コードで取引先分類種別を取得できる() {
        // Given
        CategoryType categoryType = createTestCategoryType("01");
        categoryTypeMapper.insert(categoryType);

        // When
        Optional<CategoryType> found = categoryTypeMapper.findById("01");

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getCategoryTypeCode()).isEqualTo("01");
    }

    @Test
    void すべての取引先分類種別を取得できる() {
        // Given
        categoryTypeMapper.insert(createTestCategoryType("01"));
        CategoryType categoryType2 = createTestCategoryType("02");
        categoryType2.setCategoryTypeName("地域");
        categoryTypeMapper.insert(categoryType2);

        // When
        List<CategoryType> categoryTypes = categoryTypeMapper.findAll();

        // Then
        assertThat(categoryTypes).hasSize(2);
        assertThat(categoryTypes)
            .extracting(CategoryType::getCategoryTypeCode)
            .containsExactly("01", "02");
    }

    private CategoryType createTestCategoryType(String code) {
        CategoryType categoryType = new CategoryType();
        categoryType.setCategoryTypeCode(code);
        categoryType.setCategoryTypeName("業種");
        categoryType.setCreatedBy("tester");
        categoryType.setUpdatedBy("tester");
        return categoryType;
    }
}
