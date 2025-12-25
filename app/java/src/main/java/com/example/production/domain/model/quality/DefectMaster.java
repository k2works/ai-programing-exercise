package com.example.production.domain.model.quality;

import lombok.*;
import java.time.LocalDateTime;

/**
 * 欠点マスタ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DefectMaster {
    private Integer id;
    private String defectCode;
    private String defectDescription;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
}
