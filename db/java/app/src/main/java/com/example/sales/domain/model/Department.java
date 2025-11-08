package com.example.sales.domain.model;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 部門マスタのEntityクラス
 */
public class Department {
    private String departmentCode;
    private LocalDate startDate;
    private LocalDate endDate;
    private String departmentName;
    private Integer organizationLevel;
    private String departmentPath;
    private Integer lowestLevelFlag;
    private Integer slipInputFlag;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // デフォルトコンストラクタ
    public Department() {}

    // Getter/Setter
    public String getDepartmentCode() { return departmentCode; }
    public void setDepartmentCode(String departmentCode) { this.departmentCode = departmentCode; }

    public LocalDate getStartDate() { return startDate; }
    public void setStartDate(LocalDate startDate) { this.startDate = startDate; }

    public LocalDate getEndDate() { return endDate; }
    public void setEndDate(LocalDate endDate) { this.endDate = endDate; }

    public String getDepartmentName() { return departmentName; }
    public void setDepartmentName(String departmentName) { this.departmentName = departmentName; }

    public Integer getOrganizationLevel() { return organizationLevel; }
    public void setOrganizationLevel(Integer organizationLevel) { this.organizationLevel = organizationLevel; }

    public String getDepartmentPath() { return departmentPath; }
    public void setDepartmentPath(String departmentPath) { this.departmentPath = departmentPath; }

    public Integer getLowestLevelFlag() { return lowestLevelFlag; }
    public void setLowestLevelFlag(Integer lowestLevelFlag) { this.lowestLevelFlag = lowestLevelFlag; }

    public Integer getSlipInputFlag() { return slipInputFlag; }
    public void setSlipInputFlag(Integer slipInputFlag) { this.slipInputFlag = slipInputFlag; }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public String getCreatedBy() { return createdBy; }
    public void setCreatedBy(String createdBy) { this.createdBy = createdBy; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }

    public String getUpdatedBy() { return updatedBy; }
    public void setUpdatedBy(String updatedBy) { this.updatedBy = updatedBy; }
}
