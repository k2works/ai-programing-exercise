package com.example.production.domain.model.subcontract;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * 支給区分
 */
@Getter
@RequiredArgsConstructor
public enum SupplyType {
    PAID("有償支給"),
    FREE("無償支給");

    private final String displayName;

    public static SupplyType fromDisplayName(String displayName) {
        for (SupplyType type : values()) {
            if (type.displayName.equals(displayName)) {
                return type;
            }
        }
        throw new IllegalArgumentException("Unknown supply type: " + displayName);
    }
}
