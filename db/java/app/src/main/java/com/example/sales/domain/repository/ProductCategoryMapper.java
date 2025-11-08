package com.example.sales.domain.repository;

import com.example.sales.domain.model.ProductCategory;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 商品分類マスタのMapperインターフェース
 */
@Mapper
public interface ProductCategoryMapper {

    void insert(ProductCategory productCategory);

    void update(ProductCategory productCategory);

    void delete(@Param("productCategoryCode") String productCategoryCode);

    Optional<ProductCategory> findById(@Param("productCategoryCode") String productCategoryCode);

    List<ProductCategory> findAll();

    /**
     * 階層レベルで商品分類を検索
     */
    List<ProductCategory> findByLevel(@Param("level") Integer level);

    /**
     * パス配下の商品分類を検索
     */
    List<ProductCategory> findByPathPrefix(@Param("pathPrefix") String pathPrefix);
}
