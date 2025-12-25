package com.example.production.domain.exception;

/**
 * 品目が見つからない例外
 */
public class ItemNotFoundException extends DomainException {

    public ItemNotFoundException(String itemCode) {
        super("品目が見つかりません: " + itemCode);
    }
}
