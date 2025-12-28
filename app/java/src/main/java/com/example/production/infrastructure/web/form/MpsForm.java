package com.example.production.infrastructure.web.form;

import com.example.production.domain.model.plan.MasterProductionSchedule;
import com.example.production.domain.model.plan.PlanStatus;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 基準生産計画登録・編集フォーム
 */
@Data
public class MpsForm {

    private Integer id;

    private String mpsNumber;

    @NotNull(message = "計画日は必須です")
    private LocalDate planDate;

    @NotBlank(message = "品目は必須です")
    private String itemCode;

    @NotNull(message = "計画数量は必須です")
    @Positive(message = "計画数量は1以上で入力してください")
    private BigDecimal planQuantity;

    @NotNull(message = "納期は必須です")
    private LocalDate dueDate;

    private String locationCode;

    private String remarks;

    /**
     * フォームをエンティティに変換（新規登録用）
     */
    public MasterProductionSchedule toEntity() {
        return MasterProductionSchedule.builder()
                .mpsNumber(generateMpsNumber())
                .planDate(this.planDate)
                .itemCode(this.itemCode)
                .planQuantity(this.planQuantity)
                .dueDate(this.dueDate)
                .status(PlanStatus.DRAFT)
                .locationCode(this.locationCode)
                .remarks(this.remarks)
                .build();
    }

    /**
     * フォームをエンティティに変換（更新用）
     */
    public MasterProductionSchedule toEntity(MasterProductionSchedule existing) {
        existing.setPlanDate(this.planDate);
        existing.setItemCode(this.itemCode);
        existing.setPlanQuantity(this.planQuantity);
        existing.setDueDate(this.dueDate);
        existing.setLocationCode(this.locationCode);
        existing.setRemarks(this.remarks);
        return existing;
    }

    /**
     * エンティティからフォームを生成
     */
    public static MpsForm from(MasterProductionSchedule mps) {
        MpsForm form = new MpsForm();
        form.setId(mps.getId());
        form.setMpsNumber(mps.getMpsNumber());
        form.setPlanDate(mps.getPlanDate());
        form.setItemCode(mps.getItemCode());
        form.setPlanQuantity(mps.getPlanQuantity());
        form.setDueDate(mps.getDueDate());
        form.setLocationCode(mps.getLocationCode());
        form.setRemarks(mps.getRemarks());
        return form;
    }

    /**
     * MPS番号を生成する
     */
    private String generateMpsNumber() {
        return "MPS-" + System.currentTimeMillis();
    }
}
