package com.example.production.domain.exception;

/**
 * 帳票出力例外
 */
public class ReportExportException extends DomainException {

    public ReportExportException(String message) {
        super(message);
    }

    public ReportExportException(String message, Throwable cause) {
        super(message, cause);
    }
}
