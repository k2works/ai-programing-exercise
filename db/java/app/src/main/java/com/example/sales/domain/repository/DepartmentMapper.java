package com.example.sales.domain.repository;

import com.example.sales.domain.model.Department;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 部門マスタのMapperインターフェース
 */
@Mapper
public interface DepartmentMapper {
    /**
     * 部門を登録する
     * @param department 部門
     * @return 登録件数
     */
    int insert(Department department);

    /**
     * 部門を更新する
     * @param department 部門
     * @return 更新件数
     */
    int update(Department department);

    /**
     * 部門を削除する
     * @param departmentCode 部門コード
     * @param startDate 開始日
     * @return 削除件数
     */
    int delete(@Param("departmentCode") String departmentCode, @Param("startDate") LocalDate startDate);

    /**
     * 部門コードと開始日で部門を取得する
     * @param departmentCode 部門コード
     * @param startDate 開始日
     * @return 部門
     */
    Optional<Department> findById(@Param("departmentCode") String departmentCode, @Param("startDate") LocalDate startDate);

    /**
     * すべての部門を取得する
     * @return 部門のリスト
     */
    List<Department> findAll();

    /**
     * 部門コードで部門を取得する（有効期間を考慮）
     * @param departmentCode 部門コード
     * @param targetDate 基準日
     * @return 部門
     */
    Optional<Department> findByDepartmentCode(@Param("departmentCode") String departmentCode, @Param("targetDate") LocalDate targetDate);
}
