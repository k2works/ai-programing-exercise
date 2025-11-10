package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.Department;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 部門マスタMapperのテストクラス
 */
class DepartmentMapperTest extends AbstractDatabaseTest {

    @Autowired
    private DepartmentMapper departmentMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        // テストデータをクリア（外部キー制約により、参照元を先に削除）
        // 第7章のテーブル
        jdbcTemplate.execute("DELETE FROM 与信残高データ");
        jdbcTemplate.execute("DELETE FROM 自動採番マスタ");

        // 第6章のテーブル
        jdbcTemplate.execute("DELETE FROM 請求データ明細");
        jdbcTemplate.execute("DELETE FROM 請求データ");
        jdbcTemplate.execute("DELETE FROM 入金データ");
        jdbcTemplate.execute("DELETE FROM 支払データ");
        jdbcTemplate.execute("DELETE FROM 入金口座マスタ");

        // 第5章のテーブル
        jdbcTemplate.execute("DELETE FROM 在庫データ");
        jdbcTemplate.execute("DELETE FROM 仕入データ明細");
        jdbcTemplate.execute("DELETE FROM 仕入データ");
        jdbcTemplate.execute("DELETE FROM 発注データ明細");
        jdbcTemplate.execute("DELETE FROM 発注データ");

        // 第4章のテーブル
        jdbcTemplate.execute("DELETE FROM 売上データ明細");
        jdbcTemplate.execute("DELETE FROM 売上データ");
        jdbcTemplate.execute("DELETE FROM 受注データ明細");
        jdbcTemplate.execute("DELETE FROM 受注データ");

        // 第3章以前のテーブル
        jdbcTemplate.execute("DELETE FROM 社員マスタ");
        jdbcTemplate.execute("DELETE FROM 部門マスタ");
    }

    @Test
    void 部門を登録できる() {
        // Given
        Department department = createTestDepartment("10000", LocalDate.of(2024, 1, 1));

        // When
        int result = departmentMapper.insert(department);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Department> saved = departmentMapper.findById("10000", LocalDate.of(2024, 1, 1));
        assertThat(saved).isPresent();
        assertThat(saved.get().getDepartmentCode()).isEqualTo("10000");
        assertThat(saved.get().getDepartmentName()).isEqualTo("本社");
    }

    @Test
    void 部門を更新できる() {
        // Given
        Department department = createTestDepartment("10000", LocalDate.of(2024, 1, 1));
        departmentMapper.insert(department);

        // When
        department.setDepartmentName("本社（更新後）");
        department.setUpdatedBy("updater");
        int result = departmentMapper.update(department);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Department> updated = departmentMapper.findById("10000", LocalDate.of(2024, 1, 1));
        assertThat(updated).isPresent();
        assertThat(updated.get().getDepartmentName()).isEqualTo("本社（更新後）");
        assertThat(updated.get().getUpdatedBy()).isEqualTo("updater");
    }

    @Test
    void 部門を削除できる() {
        // Given
        Department department = createTestDepartment("10000", LocalDate.of(2024, 1, 1));
        departmentMapper.insert(department);

        // When
        int result = departmentMapper.delete("10000", LocalDate.of(2024, 1, 1));

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Department> deleted = departmentMapper.findById("10000", LocalDate.of(2024, 1, 1));
        assertThat(deleted).isEmpty();
    }

    @Test
    void 部門コードと開始日で部門を取得できる() {
        // Given
        Department department = createTestDepartment("10000", LocalDate.of(2024, 1, 1));
        departmentMapper.insert(department);

        // When
        Optional<Department> found = departmentMapper.findById("10000", LocalDate.of(2024, 1, 1));

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getDepartmentCode()).isEqualTo("10000");
        assertThat(found.get().getStartDate()).isEqualTo(LocalDate.of(2024, 1, 1));
    }

    @Test
    void すべての部門を取得できる() {
        // Given
        departmentMapper.insert(createTestDepartment("10000", LocalDate.of(2024, 1, 1)));
        departmentMapper.insert(createTestDepartment("20000", LocalDate.of(2024, 1, 1)));

        // When
        List<Department> departments = departmentMapper.findAll();

        // Then
        assertThat(departments).hasSize(2);
        assertThat(departments)
            .extracting(Department::getDepartmentCode)
            .containsExactly("10000", "20000");
    }

    @Test
    void 部門コードで有効な部門を取得できる() {
        // Given
        Department dept1 = createTestDepartment("10000", LocalDate.of(2024, 1, 1));
        dept1.setEndDate(LocalDate.of(2024, 6, 30));
        departmentMapper.insert(dept1);

        Department dept2 = createTestDepartment("10000", LocalDate.of(2024, 7, 1));
        dept2.setEndDate(LocalDate.of(2024, 12, 31));
        departmentMapper.insert(dept2);

        // When - 2024年5月1日時点で有効な部門を取得
        Optional<Department> found = departmentMapper.findByDepartmentCode(
            "10000",
            LocalDate.of(2024, 5, 1)
        );

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getStartDate()).isEqualTo(LocalDate.of(2024, 1, 1));
        assertThat(found.get().getEndDate()).isEqualTo(LocalDate.of(2024, 6, 30));
    }

    private Department createTestDepartment(String code, LocalDate startDate) {
        Department department = new Department();
        department.setDepartmentCode(code);
        department.setStartDate(startDate);
        department.setEndDate(LocalDate.of(9999, 12, 31));
        department.setDepartmentName("本社");
        department.setOrganizationLevel(1);
        department.setDepartmentPath(code);
        department.setLowestLevelFlag(1);
        department.setSlipInputFlag(0);
        department.setCreatedBy("tester");
        department.setUpdatedBy("tester");
        return department;
    }
}
