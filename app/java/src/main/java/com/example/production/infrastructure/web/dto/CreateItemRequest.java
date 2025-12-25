package com.example.production.infrastructure.web.dto;

import com.example.production.domain.model.item.ItemCategory;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 品目登録リクエスト
 */
@Data
public class CreateItemRequest {

    @NotBlank(message = "品目コードは必須です")
    @Size(max = 20, message = "品目コードは20文字以内で入力してください")
    private String itemCode;

    @NotBlank(message = "品名は必須です")
    @Size(max = 100, message = "品名は100文字以内で入力してください")
    private String itemName;

    @NotNull(message = "品目区分は必須です")
    private ItemCategory category;

    private String unitCode;
    private Integer leadTime;
    private Integer safetyLeadTime;
    private BigDecimal safetyStock;
    private BigDecimal yieldRate;
    private BigDecimal minLotSize;
    private BigDecimal lotIncrement;
    private BigDecimal maxLotSize;
    private Integer shelfLife;
}
