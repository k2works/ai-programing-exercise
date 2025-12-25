package com.example.production.domain.exception;

/**
 * 品目コード重複例外
 */
public class DuplicateItemException extends DomainException {

    public DuplicateItemException(String itemCode) {
        super("品目コードが既に存在します: " + itemCode);
    }
}
