package com.example.production.infrastructure.web.form;

import com.example.production.domain.model.purchase.Receiving;
import com.example.production.domain.model.purchase.ReceivingType;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 入荷登録フォーム
 */
@Data
public class ReceivingForm {

    @NotBlank(message = "発注番号は必須です")
    private String purchaseOrderNumber;

    @NotNull(message = "発注行番号は必須です")
    private Integer lineNumber;

    @NotNull(message = "入荷日は必須です")
    private LocalDate receivingDate;

    private String receiverCode;

    @NotNull(message = "入荷区分は必須です")
    private ReceivingType receivingType;

    @NotBlank(message = "品目コードは必須です")
    private String itemCode;

    @NotNull(message = "入荷数量は必須です")
    @Positive(message = "入荷数量は1以上で入力してください")
    private BigDecimal receivingQuantity;

    private String remarks;

    /**
     * フォームをエンティティに変換
     */
    public Receiving toEntity() {
        return Receiving.builder()
                .purchaseOrderNumber(this.purchaseOrderNumber)
                .lineNumber(this.lineNumber)
                .receivingDate(this.receivingDate)
                .receiverCode(this.receiverCode)
                .receivingType(this.receivingType)
                .itemCode(this.itemCode)
                .miscellaneousItemFlag(false)
                .receivingQuantity(this.receivingQuantity)
                .remarks(this.remarks)
                .build();
    }
}
