package com.example.production.infrastructure.web.form;

import com.example.production.application.port.in.command.CreatePurchaseOrderCommand;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * 発注登録・編集フォーム
 */
@Data
public class PurchaseOrderForm {

    @NotBlank(message = "取引先は必須です")
    private String supplierCode;

    private LocalDate orderDate;

    private String ordererCode;

    private String departmentCode;

    private String remarks;

    @Valid
    private List<DetailForm> details = new ArrayList<>();

    /**
     * 明細フォーム
     */
    @Data
    public static class DetailForm {
        @NotBlank(message = "品目は必須です")
        private String itemCode;

        @NotNull(message = "発注数量は必須です")
        @Positive(message = "発注数量は1以上で入力してください")
        private BigDecimal orderQuantity;

        @Positive(message = "単価は0より大きい値で入力してください")
        private BigDecimal unitPrice;

        @NotNull(message = "納期は必須です")
        private LocalDate deliveryDate;
    }

    /**
     * フォームをコマンドに変換
     */
    public CreatePurchaseOrderCommand toCommand() {
        List<CreatePurchaseOrderCommand.PurchaseOrderDetailCommand> detailCommands = new ArrayList<>();
        if (this.details != null) {
            for (DetailForm detail : this.details) {
                detailCommands.add(CreatePurchaseOrderCommand.PurchaseOrderDetailCommand.builder()
                        .itemCode(detail.getItemCode())
                        .orderQuantity(detail.getOrderQuantity())
                        .unitPrice(detail.getUnitPrice())
                        .deliveryDate(detail.getDeliveryDate())
                        .build());
            }
        }

        return CreatePurchaseOrderCommand.builder()
                .supplierCode(this.supplierCode)
                .ordererCode(this.ordererCode)
                .departmentCode(this.departmentCode)
                .remarks(this.remarks)
                .details(detailCommands)
                .build();
    }
}
