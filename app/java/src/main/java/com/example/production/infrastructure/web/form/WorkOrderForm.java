package com.example.production.infrastructure.web.form;

import com.example.production.application.port.in.command.WorkOrderCreateCommand;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.time.LocalDate;

/**
 * 作業指示登録フォーム
 */
@Data
public class WorkOrderForm {

    @NotBlank(message = "オーダ番号は必須です")
    private String orderNumber;

    @NotNull(message = "作業指示日は必須です")
    private LocalDate workOrderDate;

    @NotBlank(message = "場所コードは必須です")
    private String locationCode;

    @NotNull(message = "予定開始日は必須です")
    private LocalDate plannedStartDate;

    @NotNull(message = "予定終了日は必須です")
    private LocalDate plannedEndDate;

    private String remarks;

    /**
     * フォームをコマンドに変換
     */
    public WorkOrderCreateCommand toCommand() {
        return WorkOrderCreateCommand.builder()
                .orderNumber(this.orderNumber)
                .workOrderDate(this.workOrderDate)
                .locationCode(this.locationCode)
                .plannedStartDate(this.plannedStartDate)
                .plannedEndDate(this.plannedEndDate)
                .remarks(this.remarks)
                .build();
    }
}
