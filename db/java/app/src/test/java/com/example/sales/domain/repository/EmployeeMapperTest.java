package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.Department;
import com.example.sales.domain.model.Employee;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 社員マスタMapperのテストクラス
 */
class EmployeeMapperTest extends AbstractDatabaseTest {

    @Autowired
    private EmployeeMapper employeeMapper;

    @Autowired
    private DepartmentMapper departmentMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        // テストデータをクリア
        jdbcTemplate.execute("DELETE FROM 社員マスタ");
        jdbcTemplate.execute("DELETE FROM 部門マスタ");

        // 部門マスタにテストデータを登録
        Department department = new Department();
        department.setDepartmentCode("10000");
        department.setStartDate(LocalDate.of(2024, 1, 1));
        department.setEndDate(LocalDate.of(9999, 12, 31));
        department.setDepartmentName("本社");
        department.setOrganizationLevel(1);
        department.setDepartmentPath("10000");
        department.setLowestLevelFlag(1);
        department.setSlipInputFlag(0);
        department.setCreatedBy("tester");
        department.setUpdatedBy("tester");
        departmentMapper.insert(department);
    }

    @Test
    void 社員を登録できる() {
        // Given
        Employee employee = createTestEmployee("E0001", "10000");

        // When
        int result = employeeMapper.insert(employee);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Employee> saved = employeeMapper.findById("E0001");
        assertThat(saved).isPresent();
        assertThat(saved.get().getEmployeeCode()).isEqualTo("E0001");
        assertThat(saved.get().getEmployeeName()).isEqualTo("山田太郎");
    }

    @Test
    void 社員を更新できる() {
        // Given
        Employee employee = createTestEmployee("E0001", "10000");
        employeeMapper.insert(employee);

        // When
        employee.setEmployeeName("山田花子");
        employee.setUpdatedBy("updater");
        int result = employeeMapper.update(employee);

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Employee> updated = employeeMapper.findById("E0001");
        assertThat(updated).isPresent();
        assertThat(updated.get().getEmployeeName()).isEqualTo("山田花子");
        assertThat(updated.get().getUpdatedBy()).isEqualTo("updater");
    }

    @Test
    void 社員を削除できる() {
        // Given
        Employee employee = createTestEmployee("E0001", "10000");
        employeeMapper.insert(employee);

        // When
        int result = employeeMapper.delete("E0001");

        // Then
        assertThat(result).isEqualTo(1);

        Optional<Employee> deleted = employeeMapper.findById("E0001");
        assertThat(deleted).isEmpty();
    }

    @Test
    void 社員コードで社員を取得できる() {
        // Given
        Employee employee = createTestEmployee("E0001", "10000");
        employeeMapper.insert(employee);

        // When
        Optional<Employee> found = employeeMapper.findById("E0001");

        // Then
        assertThat(found).isPresent();
        assertThat(found.get().getEmployeeCode()).isEqualTo("E0001");
        assertThat(found.get().getEmployeeName()).isEqualTo("山田太郎");
    }

    @Test
    void すべての社員を取得できる() {
        // Given
        employeeMapper.insert(createTestEmployee("E0001", "10000"));
        employeeMapper.insert(createTestEmployee("E0002", "10000"));

        // When
        List<Employee> employees = employeeMapper.findAll();

        // Then
        assertThat(employees).hasSize(2);
        assertThat(employees)
            .extracting(Employee::getEmployeeCode)
            .containsExactly("E0001", "E0002");
    }

    @Test
    void 部門コードで社員を取得できる() {
        // Given
        employeeMapper.insert(createTestEmployee("E0001", "10000"));
        employeeMapper.insert(createTestEmployee("E0002", "10000"));

        // When
        List<Employee> employees = employeeMapper.findByDepartmentCode("10000");

        // Then
        assertThat(employees).hasSize(2);
        assertThat(employees)
            .extracting(Employee::getDepartmentCode)
            .containsOnly("10000");
    }

    private Employee createTestEmployee(String code, String departmentCode) {
        Employee employee = new Employee();
        employee.setEmployeeCode(code);
        employee.setEmployeeName("山田太郎");
        employee.setEmployeeNameKana("ヤマダタロウ");
        employee.setGender("M");
        employee.setBirthDate(LocalDate.of(1990, 1, 1));
        employee.setJoinDate(LocalDate.of(2020, 4, 1));
        employee.setDepartmentCode(departmentCode);
        employee.setPositionCode("P001");
        employee.setCreatedBy("tester");
        employee.setUpdatedBy("tester");
        return employee;
    }
}
