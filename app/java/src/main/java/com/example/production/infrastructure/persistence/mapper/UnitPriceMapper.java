package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.purchase.UnitPrice;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDate;
import java.util.Optional;

@Mapper
public interface UnitPriceMapper {
    void insert(UnitPrice unitPrice);

    Optional<UnitPrice> findEffectiveUnitPrice(
            @Param("itemCode") String itemCode,
            @Param("supplierCode") String supplierCode,
            @Param("date") LocalDate date);

    void deleteAll();
}
