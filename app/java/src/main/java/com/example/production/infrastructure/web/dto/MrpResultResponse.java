package com.example.production.infrastructure.web.dto;

import lombok.Builder;
import lombok.Value;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * MRP実行結果レスポンス
 */
@Value
@Builder
public class MrpResultResponse {
    LocalDateTime executionTime;
    LocalDate periodStart;
    LocalDate periodEnd;
    List<RequirementResponse> requirements;

    @Value
    @Builder
    public static class RequirementResponse {
        String requirementNumber;
        String itemCode;
        LocalDate dueDate;
        BigDecimal requiredQuantity;
        BigDecimal allocatedQuantity;
        BigDecimal shortageQuantity;
        String locationCode;
    }
}
