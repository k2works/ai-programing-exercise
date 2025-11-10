package com.example.sales.domain.repository;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.domain.model.Department;
import com.example.sales.domain.model.Employee;
import com.example.sales.domain.model.Warehouse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 倉庫マスタMapperのテストクラス
 */
class WarehouseMapperTest extends AbstractDatabaseTest {

    @Autowired
    private WarehouseMapper warehouseMapper;

    @Autowired
    private DepartmentMapper departmentMapper;

    @Autowired
    private EmployeeMapper employeeMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        // テストデータをクリア（外部キー制約を考慮した順序）
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
        jdbcTemplate.execute("DELETE FROM 売上データ明細");
        jdbcTemplate.execute("DELETE FROM 売上データ");
        jdbcTemplate.execute("DELETE FROM 受注データ明細");
        jdbcTemplate.execute("DELETE FROM 受注データ");
        jdbcTemplate.execute("DELETE FROM 顧客別販売単価");
        jdbcTemplate.execute("DELETE FROM 代替商品");
        jdbcTemplate.execute("DELETE FROM 商品マスタ");
        jdbcTemplate.execute("DELETE FROM 商品分類マスタ");
        jdbcTemplate.execute("DELETE FROM 倉庫マスタ");
        jdbcTemplate.execute("DELETE FROM 仕入先マスタ");
        jdbcTemplate.execute("DELETE FROM 顧客マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先分類所属マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先マスタ");
        jdbcTemplate.execute("DELETE FROM 取引先グループマスタ");
        jdbcTemplate.execute("DELETE FROM 社員マスタ");
        jdbcTemplate.execute("DELETE FROM 部門マスタ");

        // 部門を作成
        Department department = new Department();
        department.setDepartmentCode("10000");
        department.setStartDate(LocalDate.of(2024, 1, 1));
        department.setEndDate(LocalDate.of(9999, 12, 31));
        department.setDepartmentName("物流部");
        department.setOrganizationLevel(1);
        department.setDepartmentPath("10000");
        department.setLowestLevelFlag(1);
        department.setSlipInputFlag(0);
        department.setCreatedBy("tester");
        department.setUpdatedBy("tester");
        departmentMapper.insert(department);

        // 社員を作成
        Employee employee = new Employee();
        employee.setEmployeeCode("0000000001");
        employee.setDepartmentCode("10000");
        employee.setEmployeeName("テスト社員");
        employee.setEmployeeNameKana("テストシャイン");
        employee.setGender("M");
        employee.setBirthDate(LocalDate.of(1990, 1, 1));
        employee.setJoinDate(LocalDate.of(2020, 4, 1));
        employee.setCreatedBy("tester");
        employee.setUpdatedBy("tester");
        employeeMapper.insert(employee);
    }

    @Test
    void testInsertAndFindById() {
        // 倉庫を作成
        Warehouse warehouse = new Warehouse();
        warehouse.setWarehouseCode("001");
        warehouse.setWarehouseName("東京倉庫");
        warehouse.setWarehouseType(1);
        warehouse.setAddress("東京都千代田区1-1-1");
        warehouse.setPhoneNumber("03-1234-5678");
        warehouse.setManagerCode("0000000001");
        warehouse.setCreatedBy("tester");
        warehouse.setUpdatedBy("tester");

        // 挿入
        int result = warehouseMapper.insert(warehouse);
        assertThat(result).isEqualTo(1);

        // 取得
        Optional<Warehouse> found = warehouseMapper.findById("001");
        assertThat(found).isPresent();
        assertThat(found.get().getWarehouseCode()).isEqualTo("001");
        assertThat(found.get().getWarehouseName()).isEqualTo("東京倉庫");
        assertThat(found.get().getWarehouseType()).isEqualTo(1);
    }

    @Test
    void testUpdate() {
        // 倉庫を作成
        Warehouse warehouse = new Warehouse();
        warehouse.setWarehouseCode("001");
        warehouse.setWarehouseName("東京倉庫");
        warehouse.setWarehouseType(1);
        warehouse.setAddress("東京都千代田区1-1-1");
        warehouse.setManagerCode("0000000001");
        warehouse.setCreatedBy("tester");
        warehouse.setUpdatedBy("tester");
        warehouseMapper.insert(warehouse);

        // 更新
        warehouse.setWarehouseName("東京第一倉庫");
        warehouse.setWarehouseType(2);
        warehouse.setUpdatedBy("updater");
        int result = warehouseMapper.update(warehouse);
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Warehouse> found = warehouseMapper.findById("001");
        assertThat(found).isPresent();
        assertThat(found.get().getWarehouseName()).isEqualTo("東京第一倉庫");
        assertThat(found.get().getWarehouseType()).isEqualTo(2);
    }

    @Test
    void testDelete() {
        // 倉庫を作成
        Warehouse warehouse = new Warehouse();
        warehouse.setWarehouseCode("001");
        warehouse.setWarehouseName("東京倉庫");
        warehouse.setWarehouseType(1);
        warehouse.setManagerCode("0000000001");
        warehouse.setCreatedBy("tester");
        warehouse.setUpdatedBy("tester");
        warehouseMapper.insert(warehouse);

        // 削除
        int result = warehouseMapper.delete("001");
        assertThat(result).isEqualTo(1);

        // 確認
        Optional<Warehouse> found = warehouseMapper.findById("001");
        assertThat(found).isEmpty();
    }

    @Test
    void testFindAll() {
        // 倉庫1を作成
        Warehouse warehouse1 = new Warehouse();
        warehouse1.setWarehouseCode("001");
        warehouse1.setWarehouseName("東京倉庫");
        warehouse1.setWarehouseType(1);
        warehouse1.setManagerCode("0000000001");
        warehouse1.setCreatedBy("tester");
        warehouse1.setUpdatedBy("tester");
        warehouseMapper.insert(warehouse1);

        // 倉庫2を作成
        Warehouse warehouse2 = new Warehouse();
        warehouse2.setWarehouseCode("002");
        warehouse2.setWarehouseName("大阪倉庫");
        warehouse2.setWarehouseType(1);
        warehouse2.setManagerCode("0000000001");
        warehouse2.setCreatedBy("tester");
        warehouse2.setUpdatedBy("tester");
        warehouseMapper.insert(warehouse2);

        // 全件取得
        List<Warehouse> warehouses = warehouseMapper.findAll();
        assertThat(warehouses).hasSize(2);
    }
}
