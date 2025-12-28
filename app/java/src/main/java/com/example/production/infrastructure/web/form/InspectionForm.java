package com.example.production.infrastructure.web.form;

import com.example.production.domain.model.purchase.Inspection;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 検収登録フォーム
 */
@Data
public class InspectionForm {

    @NotBlank(message = "入荷番号は必須です")
    private String receivingNumber;

    @NotBlank(message = "発注番号は必須です")
    private String purchaseOrderNumber;

    @NotNull(message = "発注行番号は必須です")
    private Integer lineNumber;

    @NotNull(message = "検収日は必須です")
    private LocalDate inspectionDate;

    private String inspectorCode;

    @NotBlank(message = "品目コードは必須です")
    private String itemCode;

    @NotNull(message = "良品数は必須です")
    @PositiveOrZero(message = "良品数は0以上で入力してください")
    private BigDecimal goodQuantity;

    @PositiveOrZero(message = "不良品数は0以上で入力してください")
    private BigDecimal defectQuantity;

    private String remarks;

    /**
     * フォームをエンティティに変換
     */
    public Inspection toEntity() {
        return Inspection.builder()
                .receivingNumber(this.receivingNumber)
                .purchaseOrderNumber(this.purchaseOrderNumber)
                .lineNumber(this.lineNumber)
                .inspectionDate(this.inspectionDate)
                .inspectorCode(this.inspectorCode)
                .itemCode(this.itemCode)
                .miscellaneousItemFlag(false)
                .goodQuantity(this.goodQuantity)
                .defectQuantity(this.defectQuantity != null ? this.defectQuantity : BigDecimal.ZERO)
                .remarks(this.remarks)
                .build();
    }
}
