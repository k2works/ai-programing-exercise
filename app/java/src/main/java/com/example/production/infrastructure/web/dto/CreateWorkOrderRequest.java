package com.example.production.infrastructure.web.dto;

import com.example.production.application.port.in.command.WorkOrderCreateCommand;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.time.LocalDate;

/**
 * 作業指示作成リクエスト
 */
@Data
public class CreateWorkOrderRequest {

    @NotBlank(message = "オーダ番号は必須です")
    private String orderNumber;

    @NotNull(message = "作業指示日は必須です")
    private LocalDate workOrderDate;

    @NotBlank(message = "場所コードは必須です")
    private String locationCode;

    @NotNull(message = "開始予定日は必須です")
    private LocalDate plannedStartDate;

    @NotNull(message = "終了予定日は必須です")
    private LocalDate plannedEndDate;

    private String remarks;

    public WorkOrderCreateCommand toCommand() {
        return WorkOrderCreateCommand.builder()
                .orderNumber(orderNumber)
                .workOrderDate(workOrderDate)
                .locationCode(locationCode)
                .plannedStartDate(plannedStartDate)
                .plannedEndDate(plannedEndDate)
                .remarks(remarks)
                .build();
    }
}
