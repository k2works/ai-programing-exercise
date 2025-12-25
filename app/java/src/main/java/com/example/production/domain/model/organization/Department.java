package com.example.production.domain.model.organization;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * 部門マスタ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Department {
    private String departmentCode;
    private String departmentName;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
}
