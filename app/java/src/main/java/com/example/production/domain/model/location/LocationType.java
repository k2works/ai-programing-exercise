package com.example.production.domain.model.location;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum LocationType {
    WAREHOUSE("倉庫"),
    MANUFACTURING("製造"),
    INSPECTION("検査"),
    SHIPPING("出荷"),
    SUBCONTRACT("外注");

    private final String displayName;

    public static LocationType fromDisplayName(String displayName) {
        for (LocationType type : values()) {
            if (type.displayName.equals(displayName)) {
                return type;
            }
        }
        throw new IllegalArgumentException("不正な場所区分: " + displayName);
    }
}
