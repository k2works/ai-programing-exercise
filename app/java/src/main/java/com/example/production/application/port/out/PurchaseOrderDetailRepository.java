package com.example.production.application.port.out;

import com.example.production.domain.model.purchase.PurchaseOrderDetail;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 発注明細リポジトリ（Output Port）
 */
public interface PurchaseOrderDetailRepository {
    void save(PurchaseOrderDetail detail);

    Optional<PurchaseOrderDetail> findById(Integer id);

    List<PurchaseOrderDetail> findByPurchaseOrderNumber(String purchaseOrderNumber);

    void updateReceivedQuantity(Integer id, BigDecimal receivedQuantity);

    void updateAcceptedQuantity(Integer id, BigDecimal acceptedQuantity);

    void updateCompletedFlag(Integer id, Boolean completedFlag);

    void deleteAll();
}
