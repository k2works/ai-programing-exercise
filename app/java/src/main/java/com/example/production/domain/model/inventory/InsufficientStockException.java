package com.example.production.domain.model.inventory;

/**
 * 在庫不足例外
 */
public class InsufficientStockException extends RuntimeException {
    public InsufficientStockException(String message) {
        super(message);
    }
}
