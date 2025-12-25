package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 工数実績登録コマンド
 */
@Data
@Builder
public class LaborHoursCreateCommand {
    private String workOrderNumber;
    private Integer sequence;
    private LocalDate workDate;
    private String employeeCode;
    private String departmentCode;
    private BigDecimal hours;
    private String remarks;
    private String createdBy;
}
