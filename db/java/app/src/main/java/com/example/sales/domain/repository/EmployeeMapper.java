package com.example.sales.domain.repository;

import com.example.sales.domain.model.Employee;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 社員マスタのMapperインターフェース
 */
@Mapper
public interface EmployeeMapper {
    /**
     * 社員を登録する
     * @param employee 社員
     * @return 登録件数
     */
    int insert(Employee employee);

    /**
     * 社員を更新する
     * @param employee 社員
     * @return 更新件数
     */
    int update(Employee employee);

    /**
     * 社員を削除する
     * @param employeeCode 社員コード
     * @return 削除件数
     */
    int delete(@Param("employeeCode") String employeeCode);

    /**
     * 社員コードで社員を取得する
     * @param employeeCode 社員コード
     * @return 社員
     */
    Optional<Employee> findById(@Param("employeeCode") String employeeCode);

    /**
     * すべての社員を取得する
     * @return 社員のリスト
     */
    List<Employee> findAll();

    /**
     * 部門コードで社員を取得する
     * @param departmentCode 部門コード
     * @return 社員のリスト
     */
    List<Employee> findByDepartmentCode(@Param("departmentCode") String departmentCode);
}
