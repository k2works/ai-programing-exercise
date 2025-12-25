package com.example.production.application.port.out;

import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;

import java.util.List;
import java.util.Optional;

/**
 * 発注リポジトリ（Output Port）
 */
public interface PurchaseOrderRepository {
    void save(PurchaseOrder purchaseOrder);

    Optional<PurchaseOrder> findByPurchaseOrderNumber(String purchaseOrderNumber);

    Optional<String> findLatestPurchaseOrderNumber(String prefix);

    void updateStatus(String purchaseOrderNumber, PurchaseOrderStatus status);

    List<PurchaseOrder> findAll();

    void deleteAll();
}
