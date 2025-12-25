package com.example.production.domain.exception;

/**
 * 作業指示が見つからない例外
 */
public class WorkOrderNotFoundException extends DomainException {

    public WorkOrderNotFoundException(String workOrderNumber) {
        super("作業指示が見つかりません: " + workOrderNumber);
    }
}
