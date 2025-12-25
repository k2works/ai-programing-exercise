package com.example.production.domain.model.inventory;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * 在庫状態
 */
@Getter
@RequiredArgsConstructor
public enum StockStatus {
    PASSED("合格"),
    DEFECTIVE("不良"),
    UNINSPECTED("未検査");

    private final String displayName;

    public static StockStatus fromDisplayName(String displayName) {
        for (StockStatus status : values()) {
            if (status.displayName.equals(displayName)) {
                return status;
            }
        }
        throw new IllegalArgumentException("Unknown stock status: " + displayName);
    }
}
