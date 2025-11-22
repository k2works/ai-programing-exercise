package com.example.accounting.application.exception;

/**
 * 仕訳が見つからない例外
 */
public class JournalNotFoundException extends RuntimeException {

    public JournalNotFoundException(String message) {
        super(message);
    }

    public JournalNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}
