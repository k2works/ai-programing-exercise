package com.example.production.application.port.out;

import com.example.production.domain.model.organization.Department;

import java.util.Optional;

/**
 * 部門リポジトリ（Output Port）
 */
public interface DepartmentRepository {
    void save(Department department);
    Optional<Department> findByDepartmentCode(String departmentCode);
    void deleteAll();
}
