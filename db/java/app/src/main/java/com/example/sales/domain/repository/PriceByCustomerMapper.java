package com.example.sales.domain.repository;

import com.example.sales.domain.model.PriceByCustomer;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 顧客別販売単価のMapperインターフェース
 */
@Mapper
public interface PriceByCustomerMapper {

    void insert(PriceByCustomer priceByCustomer);

    void update(PriceByCustomer priceByCustomer);

    void delete(@Param("productCode") String productCode,
                @Param("customerCode") String customerCode);

    Optional<PriceByCustomer> findById(@Param("productCode") String productCode,
                                        @Param("customerCode") String customerCode);

    /**
     * 商品コードで顧客別価格を検索
     */
    List<PriceByCustomer> findByProductCode(@Param("productCode") String productCode);

    /**
     * 顧客コードで顧客別価格を検索
     */
    List<PriceByCustomer> findByCustomerCode(@Param("customerCode") String customerCode);
}
