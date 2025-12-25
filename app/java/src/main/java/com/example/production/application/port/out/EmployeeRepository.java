package com.example.production.application.port.out;

import com.example.production.domain.model.organization.Employee;

import java.util.List;
import java.util.Optional;

/**
 * 担当者リポジトリ（Output Port）
 */
public interface EmployeeRepository {
    void save(Employee employee);
    Optional<Employee> findByEmployeeCode(String employeeCode);
    List<Employee> findByDepartmentCode(String departmentCode);
    List<Employee> findAll();
    void deleteAll();
}
