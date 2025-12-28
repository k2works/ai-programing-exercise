package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.organization.Department;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;
import java.util.Optional;

@Mapper
public interface DepartmentMapper {
    void insert(Department department);
    Optional<Department> findByDepartmentCode(String departmentCode);
    List<Department> findAll();
    void deleteAll();
}
