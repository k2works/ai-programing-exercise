package com.example.production.infrastructure.web.form;

import com.example.production.application.port.in.command.CompletionResultCreateCommand;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 完成実績登録フォーム
 */
@Data
public class CompletionForm {

    @NotBlank(message = "作業指示番号は必須です")
    private String workOrderNumber;

    @NotNull(message = "完成日は必須です")
    private LocalDate completionDate;

    @NotNull(message = "完成数量は必須です")
    @PositiveOrZero(message = "完成数量は0以上で入力してください")
    private BigDecimal completedQuantity;

    @NotNull(message = "良品数は必須です")
    @PositiveOrZero(message = "良品数は0以上で入力してください")
    private BigDecimal goodQuantity;

    @PositiveOrZero(message = "不良品数は0以上で入力してください")
    private BigDecimal defectQuantity;

    private String remarks;

    /**
     * フォームをコマンドに変換
     */
    public CompletionResultCreateCommand toCommand() {
        return CompletionResultCreateCommand.builder()
                .workOrderNumber(this.workOrderNumber)
                .completionDate(this.completionDate)
                .completedQuantity(this.completedQuantity)
                .goodQuantity(this.goodQuantity)
                .defectQuantity(this.defectQuantity != null ? this.defectQuantity : BigDecimal.ZERO)
                .remarks(this.remarks)
                .build();
    }
}
