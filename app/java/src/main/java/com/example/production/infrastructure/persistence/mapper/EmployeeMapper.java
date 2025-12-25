package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.organization.Employee;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;
import java.util.Optional;

@Mapper
public interface EmployeeMapper {
    void insert(Employee employee);
    Optional<Employee> findByEmployeeCode(String employeeCode);
    List<Employee> findByDepartmentCode(String departmentCode);
    List<Employee> findAll();
    void deleteAll();
}
