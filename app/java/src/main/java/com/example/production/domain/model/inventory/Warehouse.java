package com.example.production.domain.model.inventory;

import com.example.production.domain.model.organization.Department;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * 倉庫マスタ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Warehouse {
    private String warehouseCode;
    private String warehouseCategory;
    private String warehouseName;
    private String departmentCode;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    // リレーション
    private Department department;
}
