package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

/**
 * 作業指示作成コマンド
 */
@Data
@Builder
public class WorkOrderCreateCommand {
    private String orderNumber;
    private LocalDate workOrderDate;
    private String locationCode;
    private LocalDate plannedStartDate;
    private LocalDate plannedEndDate;
    private String remarks;
}
