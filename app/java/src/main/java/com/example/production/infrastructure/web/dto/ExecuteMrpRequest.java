package com.example.production.infrastructure.web.dto;

import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.time.LocalDate;

/**
 * MRP実行リクエスト
 */
@Data
public class ExecuteMrpRequest {

    @NotNull(message = "開始日は必須です")
    private LocalDate startDate;

    @NotNull(message = "終了日は必須です")
    private LocalDate endDate;
}
