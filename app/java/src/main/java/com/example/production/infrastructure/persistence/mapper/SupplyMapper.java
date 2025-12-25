package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.subcontract.Supply;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface SupplyMapper {

    void insert(Supply supply);

    Optional<Supply> findById(@Param("id") Integer id);

    Optional<Supply> findBySupplyNumber(@Param("supplyNumber") String supplyNumber);

    List<Supply> findByPurchaseOrderDetail(@Param("purchaseOrderNumber") String purchaseOrderNumber,
                                           @Param("lineNumber") Integer lineNumber);

    Optional<String> findLatestSupplyNumber(@Param("prefix") String prefix);

    void deleteAll();
}
