package com.example.production.domain.model.organization;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * 担当者マスタ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Employee {
    private String employeeCode;
    private String employeeName;
    private String departmentCode;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // リレーション
    private Department department;
}
