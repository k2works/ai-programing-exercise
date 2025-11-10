package com.example.sales.domain.repository;

import com.example.sales.domain.model.Product;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 商品マスタのMapperインターフェース
 */
@Mapper
public interface ProductMapper {

    void insert(Product product);

    void update(Product product);

    void delete(@Param("productCode") String productCode);

    Optional<Product> findById(@Param("productCode") String productCode);

    List<Product> findAll();

    /**
     * 商品分類コードで商品を検索
     */
    List<Product> findByCategory(@Param("productCategoryCode") String productCategoryCode);

    /**
     * 商品コードの存在チェック
     */
    boolean existsById(@Param("productCode") String productCode);

    /**
     * ページング対応の商品一覧取得
     */
    List<Product> findAllWithPaging(
        @Param("offset") int offset,
        @Param("limit") int limit
    );

    /**
     * 商品の総件数取得
     */
    int count();
}
