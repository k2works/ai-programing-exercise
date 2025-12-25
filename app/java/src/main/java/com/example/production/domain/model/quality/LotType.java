package com.example.production.domain.model.quality;

/**
 * ロット種別
 */
public enum LotType {
    PURCHASE("購入ロット"),
    PRODUCTION("製造ロット");

    private final String displayName;

    LotType(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }

    public static LotType fromDisplayName(String displayName) {
        for (LotType type : values()) {
            if (type.displayName.equals(displayName)) {
                return type;
            }
        }
        throw new IllegalArgumentException("Unknown display name: " + displayName);
    }
}
