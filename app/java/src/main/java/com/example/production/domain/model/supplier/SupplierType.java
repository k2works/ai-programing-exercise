package com.example.production.domain.model.supplier;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum SupplierType {
    VENDOR("仕入先"),
    SUBCONTRACTOR("外注先"),
    CUSTOMER("得意先"),
    VENDOR_AND_SUBCONTRACTOR("仕入先兼外注先");

    private final String displayName;

    public static SupplierType fromDisplayName(String displayName) {
        for (SupplierType type : values()) {
            if (type.displayName.equals(displayName)) {
                return type;
            }
        }
        throw new IllegalArgumentException("不正な取引先区分: " + displayName);
    }
}
