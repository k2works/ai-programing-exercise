package com.example.production.application.port.out;

import com.example.production.domain.model.subcontract.ConsumptionDetail;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 消費明細リポジトリ
 */
public interface ConsumptionDetailRepository {
    void save(ConsumptionDetail detail);
    Optional<ConsumptionDetail> findById(Integer id);
    List<ConsumptionDetail> findByConsumptionNumber(String consumptionNumber);
    BigDecimal sumByPurchaseOrderAndItem(String purchaseOrderNumber, Integer lineNumber, String itemCode);
    void deleteAll();
}
