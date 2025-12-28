package com.example.production.infrastructure.in.web.form;

import com.example.production.application.port.in.command.LaborHoursCreateCommand;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 工数実績登録フォーム
 */
@Data
public class LaborHoursForm {

    @NotBlank(message = "作業指示番号は必須です")
    private String workOrderNumber;

    @NotNull(message = "工順は必須です")
    private Integer sequence;

    @NotNull(message = "作業日は必須です")
    private LocalDate workDate;

    private String employeeCode;

    private String departmentCode;

    @NotNull(message = "工数は必須です")
    @Positive(message = "工数は0より大きい値で入力してください")
    private BigDecimal hours;

    private String remarks;

    /**
     * フォームをコマンドに変換
     */
    public LaborHoursCreateCommand toCommand() {
        return LaborHoursCreateCommand.builder()
                .workOrderNumber(this.workOrderNumber)
                .sequence(this.sequence)
                .workDate(this.workDate)
                .employeeCode(this.employeeCode)
                .departmentCode(this.departmentCode)
                .hours(this.hours)
                .remarks(this.remarks)
                .build();
    }
}
