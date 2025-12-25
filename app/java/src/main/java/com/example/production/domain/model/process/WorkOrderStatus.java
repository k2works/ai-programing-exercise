package com.example.production.domain.model.process;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum WorkOrderStatus {
    NOT_STARTED("未着手"),
    IN_PROGRESS("作業中"),
    COMPLETED("完了"),
    SUSPENDED("中断");

    private final String displayName;

    public static WorkOrderStatus fromDisplayName(String displayName) {
        for (WorkOrderStatus status : values()) {
            if (status.displayName.equals(displayName)) {
                return status;
            }
        }
        throw new IllegalArgumentException("Unknown work order status: " + displayName);
    }
}
