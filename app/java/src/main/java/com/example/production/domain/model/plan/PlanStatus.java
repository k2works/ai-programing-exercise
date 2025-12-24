package com.example.production.domain.model.plan;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum PlanStatus {
    DRAFT("草案"),
    CONFIRMED("確定"),
    EXPANDED("展開済"),
    CANCELLED("取消");

    private final String displayName;

    public static PlanStatus fromDisplayName(String displayName) {
        for (PlanStatus status : values()) {
            if (status.displayName.equals(displayName)) {
                return status;
            }
        }
        throw new IllegalArgumentException("Unknown plan status: " + displayName);
    }
}
