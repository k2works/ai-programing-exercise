package com.example.production.domain.model.purchase;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum PurchaseOrderStatus {
    CREATING("作成中"),
    ORDERED("発注済"),
    PARTIALLY_RECEIVED("一部入荷"),
    RECEIVED("入荷完了"),
    ACCEPTED("検収完了"),
    CANCELLED("取消");

    private final String displayName;

    public static PurchaseOrderStatus fromDisplayName(String displayName) {
        for (PurchaseOrderStatus status : values()) {
            if (status.displayName.equals(displayName)) {
                return status;
            }
        }
        throw new IllegalArgumentException("Unknown purchase order status: " + displayName);
    }
}
