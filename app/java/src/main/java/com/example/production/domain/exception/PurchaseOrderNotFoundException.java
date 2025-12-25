package com.example.production.domain.exception;

/**
 * 発注が見つからない例外
 */
public class PurchaseOrderNotFoundException extends DomainException {

    public PurchaseOrderNotFoundException(String orderNumber) {
        super("発注が見つかりません: " + orderNumber);
    }
}
