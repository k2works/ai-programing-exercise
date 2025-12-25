package com.example.production.application.port.in;

import com.example.production.application.port.in.command.CreatePurchaseOrderCommand;
import com.example.production.domain.model.purchase.PurchaseOrder;

import java.util.List;

/**
 * 発注ユースケース（Input Port）
 */
public interface PurchaseOrderUseCase {

    /**
     * 発注を登録する
     */
    PurchaseOrder createOrder(CreatePurchaseOrderCommand command);

    /**
     * 発注を取得する
     */
    PurchaseOrder getOrder(String orderNumber);

    /**
     * すべての発注を取得する
     */
    List<PurchaseOrder> getAllOrders();

    /**
     * 発注を確定する
     */
    PurchaseOrder confirmOrder(String orderNumber);

    /**
     * 発注を取消する
     */
    void cancelOrder(String orderNumber);
}
