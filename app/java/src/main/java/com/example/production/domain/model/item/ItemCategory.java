package com.example.production.domain.model.item;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ItemCategory {
    PRODUCT("製品"),
    SEMI_PRODUCT("半製品"),
    INTERMEDIATE("中間品"),
    PART("部品"),
    MATERIAL("材料"),
    RAW_MATERIAL("原料"),
    SUPPLY("資材");

    private final String displayName;

    public static ItemCategory fromDisplayName(String displayName) {
        for (ItemCategory category : values()) {
            if (category.displayName.equals(displayName)) {
                return category;
            }
        }
        throw new IllegalArgumentException("不正な品目区分: " + displayName);
    }
}
