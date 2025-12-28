package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.EmployeeRepository;
import com.example.production.domain.model.organization.Employee;
import com.example.production.infrastructure.out.mapper.EmployeeMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 担当者リポジトリ実装
 */
@Repository
public class EmployeeRepositoryImpl implements EmployeeRepository {

    private final EmployeeMapper employeeMapper;

    public EmployeeRepositoryImpl(EmployeeMapper employeeMapper) {
        this.employeeMapper = employeeMapper;
    }

    @Override
    public void save(Employee employee) {
        employeeMapper.insert(employee);
    }

    @Override
    public Optional<Employee> findByEmployeeCode(String employeeCode) {
        return employeeMapper.findByEmployeeCode(employeeCode);
    }

    @Override
    public List<Employee> findByDepartmentCode(String departmentCode) {
        return employeeMapper.findByDepartmentCode(departmentCode);
    }

    @Override
    public List<Employee> findAll() {
        return employeeMapper.findAll();
    }

    @Override
    public void deleteAll() {
        employeeMapper.deleteAll();
    }
}
