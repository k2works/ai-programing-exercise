package com.example.accounting.domain.repository;

/**
 * 並行修正例外
 *
 * イベントソーシングで楽観的ロックが失敗した場合にスロー
 */
public class ConcurrentModificationException extends RuntimeException {
    public ConcurrentModificationException(String message) {
        super(message);
    }

    public ConcurrentModificationException(String message, Throwable cause) {
        super(message, cause);
    }
}
