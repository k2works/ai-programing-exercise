package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.subcontract.ConsumptionDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

@Mapper
public interface ConsumptionDetailMapper {
    void insert(ConsumptionDetail detail);
    Optional<ConsumptionDetail> findById(Integer id);
    List<ConsumptionDetail> findByConsumptionNumber(String consumptionNumber);
    BigDecimal sumByPurchaseOrderAndItem(@Param("purchaseOrderNumber") String purchaseOrderNumber,
                                         @Param("lineNumber") Integer lineNumber,
                                         @Param("itemCode") String itemCode);
    void deleteAll();
}
