package com.example.production.infrastructure.web.form;

import com.example.production.application.port.in.command.CreateItemCommand;
import com.example.production.application.port.in.command.UpdateItemCommand;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;
import jakarta.validation.constraints.Size;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 品目登録・編集フォーム
 */
@Data
public class ItemForm {

    @NotBlank(message = "品目コードは必須です")
    @Size(max = 20, message = "品目コードは20文字以内で入力してください")
    private String itemCode;

    @NotBlank(message = "品名は必須です")
    @Size(max = 100, message = "品名は100文字以内で入力してください")
    private String itemName;

    @NotNull(message = "品目区分は必須です")
    private ItemCategory category;

    private String unitCode;

    @PositiveOrZero(message = "リードタイムは0以上で入力してください")
    private Integer leadTime;

    @PositiveOrZero(message = "安全リードタイムは0以上で入力してください")
    private Integer safetyLeadTime;

    @PositiveOrZero(message = "安全在庫は0以上で入力してください")
    private BigDecimal safetyStock;

    @PositiveOrZero(message = "歩留まり率は0以上で入力してください")
    private BigDecimal yieldRate;

    @PositiveOrZero(message = "最小ロットサイズは0以上で入力してください")
    private BigDecimal minLotSize;

    @PositiveOrZero(message = "ロット増分は0以上で入力してください")
    private BigDecimal lotIncrement;

    @PositiveOrZero(message = "最大ロットサイズは0以上で入力してください")
    private BigDecimal maxLotSize;

    @PositiveOrZero(message = "保管期限は0以上で入力してください")
    private Integer shelfLife;

    /**
     * フォームを登録コマンドに変換
     */
    public CreateItemCommand toCreateCommand() {
        return CreateItemCommand.builder()
                .itemCode(this.itemCode)
                .itemName(this.itemName)
                .category(this.category)
                .unitCode(this.unitCode)
                .leadTime(this.leadTime != null ? this.leadTime : 0)
                .safetyLeadTime(this.safetyLeadTime != null ? this.safetyLeadTime : 0)
                .safetyStock(this.safetyStock)
                .yieldRate(this.yieldRate)
                .minLotSize(this.minLotSize)
                .lotIncrement(this.lotIncrement)
                .maxLotSize(this.maxLotSize)
                .shelfLife(this.shelfLife)
                .build();
    }

    /**
     * フォームを更新コマンドに変換
     */
    public UpdateItemCommand toUpdateCommand() {
        return UpdateItemCommand.builder()
                .itemCode(this.itemCode)
                .itemName(this.itemName)
                .category(this.category)
                .leadTime(this.leadTime)
                .safetyLeadTime(this.safetyLeadTime)
                .safetyStock(this.safetyStock)
                .yieldRate(this.yieldRate)
                .minLotSize(this.minLotSize)
                .lotIncrement(this.lotIncrement)
                .maxLotSize(this.maxLotSize)
                .shelfLife(this.shelfLife)
                .build();
    }

    /**
     * エンティティからフォームを生成
     */
    public static ItemForm from(Item item) {
        ItemForm form = new ItemForm();
        form.setItemCode(item.getItemCode());
        form.setItemName(item.getItemName());
        form.setCategory(item.getItemCategory());
        form.setUnitCode(item.getUnitCode());
        form.setLeadTime(item.getLeadTime());
        form.setSafetyLeadTime(item.getSafetyLeadTime());
        form.setSafetyStock(item.getSafetyStock());
        form.setYieldRate(item.getYieldRate());
        form.setMinLotSize(item.getMinLotSize());
        form.setLotIncrement(item.getLotIncrement());
        form.setMaxLotSize(item.getMaxLotSize());
        form.setShelfLife(item.getShelfLife());
        return form;
    }
}
