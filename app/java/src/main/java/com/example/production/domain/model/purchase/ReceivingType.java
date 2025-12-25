package com.example.production.domain.model.purchase;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ReceivingType {
    NORMAL("通常入荷"),
    SPLIT("分割入荷"),
    RETURN("返品入荷");

    private final String displayName;

    public static ReceivingType fromDisplayName(String displayName) {
        for (ReceivingType type : values()) {
            if (type.displayName.equals(displayName)) {
                return type;
            }
        }
        throw new IllegalArgumentException("Unknown receiving type: " + displayName);
    }
}
