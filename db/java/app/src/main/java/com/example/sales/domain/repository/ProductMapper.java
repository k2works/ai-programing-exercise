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
}
