package com.example.production.domain.model.calendar;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum DateType {
    WORKING("稼働日"),
    HOLIDAY("休日"),
    HALF_DAY("半日稼働");

    private final String displayName;

    public static DateType fromDisplayName(String displayName) {
        for (DateType type : values()) {
            if (type.displayName.equals(displayName)) {
                return type;
            }
        }
        throw new IllegalArgumentException("不正な日付区分: " + displayName);
    }
}
