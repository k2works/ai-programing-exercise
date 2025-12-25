package com.example.production.domain.exception;

import java.math.BigDecimal;

/**
 * 在庫不足例外
 */
public class InsufficientInventoryException extends DomainException {

    public InsufficientInventoryException(String itemCode, BigDecimal required, BigDecimal available) {
        super(String.format("在庫が不足しています: %s (必要: %s, 有効: %s)",
                itemCode, required, available));
    }
}
