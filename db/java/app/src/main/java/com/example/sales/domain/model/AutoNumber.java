package com.example.sales.domain.model;

import java.time.LocalDateTime;

/**
 * 自動採番マスタのEntityクラス
 */
public class AutoNumber {
    private String slipType;
    private LocalDateTime yearMonth;
    private Integer lastSlipNo;

    // デフォルトコンストラクタ
    public AutoNumber() {}

    // コンストラクタ（複合主キー用）
    public AutoNumber(String slipType, LocalDateTime yearMonth) {
        this.slipType = slipType;
        this.yearMonth = yearMonth;
    }

    // Getter/Setter（全フィールド）
    public String getSlipType() { return slipType; }
    public void setSlipType(String slipType) {
        this.slipType = slipType;
    }

    public LocalDateTime getYearMonth() { return yearMonth; }
    public void setYearMonth(LocalDateTime yearMonth) {
        this.yearMonth = yearMonth;
    }

    public Integer getLastSlipNo() { return lastSlipNo; }
    public void setLastSlipNo(Integer lastSlipNo) {
        this.lastSlipNo = lastSlipNo;
    }
}
