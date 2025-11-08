package com.example.sales.domain.repository;

import com.example.sales.domain.model.AlternateProduct;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

/**
 * 代替商品のMapperインターフェース
 */
@Mapper
public interface AlternateProductMapper {

    void insert(AlternateProduct alternateProduct);

    void update(AlternateProduct alternateProduct);

    void delete(@Param("productCode") String productCode,
                @Param("alternateProductCode") String alternateProductCode);

    /**
     * 商品の代替商品を優先順位順に取得
     */
    List<AlternateProduct> findByProductCode(@Param("productCode") String productCode);

    /**
     * 代替商品を商品情報とJOINして取得
     */
    List<Map<String, Object>> findAlternatesWithProductInfo(@Param("productCode") String productCode);
}
