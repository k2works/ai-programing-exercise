package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.DepartmentRepository;
import com.example.production.domain.model.organization.Department;
import com.example.production.infrastructure.persistence.mapper.DepartmentMapper;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * 部門リポジトリ実装
 */
@Repository
public class DepartmentRepositoryImpl implements DepartmentRepository {

    private final DepartmentMapper departmentMapper;

    public DepartmentRepositoryImpl(DepartmentMapper departmentMapper) {
        this.departmentMapper = departmentMapper;
    }

    @Override
    public void save(Department department) {
        departmentMapper.insert(department);
    }

    @Override
    public Optional<Department> findByDepartmentCode(String departmentCode) {
        return departmentMapper.findByDepartmentCode(departmentCode);
    }

    @Override
    public void deleteAll() {
        departmentMapper.deleteAll();
    }
}
