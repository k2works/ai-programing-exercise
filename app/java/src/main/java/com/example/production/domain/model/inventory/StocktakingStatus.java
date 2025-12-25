package com.example.production.domain.model.inventory;

/**
 * 棚卸ステータス
 */
public enum StocktakingStatus {
    ISSUED("発行済"),
    ENTERED("入力済"),
    CONFIRMED("確定");

    private final String displayName;

    StocktakingStatus(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }

    public static StocktakingStatus fromDisplayName(String displayName) {
        for (StocktakingStatus status : values()) {
            if (status.displayName.equals(displayName)) {
                return status;
            }
        }
        throw new IllegalArgumentException("Unknown display name: " + displayName);
    }
}
