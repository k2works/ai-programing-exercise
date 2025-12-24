package com.example.production.domain.model.plan;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum OrderType {
    PURCHASE("購買"),
    MANUFACTURING("製造");

    private final String displayName;

    public static OrderType fromDisplayName(String displayName) {
        for (OrderType type : values()) {
            if (type.displayName.equals(displayName)) {
                return type;
            }
        }
        throw new IllegalArgumentException("Unknown order type: " + displayName);
    }
}
