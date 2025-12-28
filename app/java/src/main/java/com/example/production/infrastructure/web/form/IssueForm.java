package com.example.production.infrastructure.web.form;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 払出登録フォーム
 */
@Data
public class IssueForm {

    @NotBlank(message = "作業指示番号は必須です")
    private String workOrderNumber;

    @NotNull(message = "工順は必須です")
    private Integer routingSequence;

    @NotBlank(message = "場所コードは必須です")
    private String locationCode;

    @NotNull(message = "払出日は必須です")
    private LocalDate issueDate;

    private String issuerCode;

    @NotBlank(message = "品目コードは必須です")
    private String itemCode;

    @NotNull(message = "払出数は必須です")
    @Positive(message = "払出数は正の値を入力してください")
    private BigDecimal issueQuantity;
}
