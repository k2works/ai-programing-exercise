package com.example.production.domain.model.item;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * 単位（Unit）
 *
 * 品目の数量を計測するための単位を表すドメインモデル
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Unit {
    /** 単位コード */
    private String unitCode;

    /** 単位記号 */
    private String unitSymbol;

    /** 単位名 */
    private String unitName;

    /** 作成日時 */
    private LocalDateTime createdAt;

    /** 更新日時 */
    private LocalDateTime updatedAt;
}
