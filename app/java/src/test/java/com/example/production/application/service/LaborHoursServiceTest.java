package com.example.production.application.service;

import com.example.production.application.port.in.command.LaborHoursCreateCommand;
import com.example.production.application.port.in.command.WorkOrderCreateCommand;
import com.example.production.application.port.out.*;
import com.example.production.infrastructure.dto.LaborHoursSummary;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import com.example.production.domain.model.organization.Department;
import com.example.production.domain.model.organization.Employee;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.domain.model.process.Process;
import com.example.production.domain.model.process.Routing;
import com.example.production.domain.model.process.LaborHours;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("工数実績サービス")
class LaborHoursServiceTest extends BaseIntegrationTest {

    @Autowired
    private LaborHoursService laborHoursService;

    @Autowired
    private WorkOrderService workOrderService;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private ProcessRepository processRepository;

    @Autowired
    private RoutingRepository routingRepository;

    @Autowired
    private LocationRepository locationRepository;

    @Autowired
    private OrderRepository orderRepository;

    @Autowired
    private WorkOrderRepository workOrderRepository;

    @Autowired
    private WorkOrderDetailRepository workOrderDetailRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private EmployeeRepository employeeRepository;

    @Autowired
    private LaborHoursRepository laborHoursRepository;

    private WorkOrder testWorkOrder;

    @BeforeEach
    void setUp() {
        // クリーンアップ（依存関係の順序に注意）
        laborHoursRepository.deleteAll();
        employeeRepository.deleteAll();
        departmentRepository.deleteAll();
        workOrderDetailRepository.deleteAll();
        workOrderRepository.deleteAll();
        routingRepository.deleteAll();
        orderRepository.deleteAll();
        processRepository.deleteAll();
        locationRepository.deleteAll();
        itemRepository.deleteAll();

        setupMasterData();
        setupWorkOrder();
    }

    void setupMasterData() {
        // 品目マスタ
        itemRepository.save(Item.builder()
                .itemCode("PROD-001")
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .itemName("製品A")
                .itemCategory(ItemCategory.PRODUCT)
                .build());

        // 工程マスタ
        processRepository.save(Process.builder()
                .processCode("PRESS")
                .processName("プレス加工")
                .build());
        processRepository.save(Process.builder()
                .processCode("ASSEMBLY")
                .processName("組立")
                .build());
        processRepository.save(Process.builder()
                .processCode("INSPECT")
                .processName("検査")
                .build());

        // 工程表
        routingRepository.save(Routing.builder()
                .itemCode("PROD-001")
                .sequence(1)
                .processCode("PRESS")
                .build());
        routingRepository.save(Routing.builder()
                .itemCode("PROD-001")
                .sequence(2)
                .processCode("ASSEMBLY")
                .build());
        routingRepository.save(Routing.builder()
                .itemCode("PROD-001")
                .sequence(3)
                .processCode("INSPECT")
                .build());

        // 場所マスタ
        locationRepository.save(Location.builder()
                .locationCode("LINE001")
                .locationName("製造ライン1")
                .locationType(LocationType.MANUFACTURING)
                .build());

        // 部門マスタ
        departmentRepository.save(Department.builder()
                .departmentCode("DEPT001")
                .departmentName("製造部")
                .build());

        // 担当者マスタ
        employeeRepository.save(Employee.builder()
                .employeeCode("EMP001")
                .employeeName("山田太郎")
                .departmentCode("DEPT001")
                .build());
        employeeRepository.save(Employee.builder()
                .employeeCode("EMP002")
                .employeeName("鈴木花子")
                .departmentCode("DEPT001")
                .build());

        // オーダ情報
        orderRepository.save(Order.builder()
                .orderNumber("MO-2025-001")
                .orderType(OrderType.MANUFACTURING)
                .itemCode("PROD-001")
                .startDate(LocalDate.of(2025, 1, 21))
                .dueDate(LocalDate.of(2025, 1, 25))
                .planQuantity(new BigDecimal("100"))
                .locationCode("LINE001")
                .status(PlanStatus.CONFIRMED)
                .build());
    }

    void setupWorkOrder() {
        // 作業指示を作成して開始状態にする
        testWorkOrder = workOrderService.createWorkOrder(WorkOrderCreateCommand.builder()
                .orderNumber("MO-2025-001")
                .workOrderDate(LocalDate.of(2025, 1, 20))
                .locationCode("LINE001")
                .plannedStartDate(LocalDate.of(2025, 1, 21))
                .plannedEndDate(LocalDate.of(2025, 1, 25))
                .build());
        testWorkOrder = workOrderService.startWork(testWorkOrder.getWorkOrderNumber());
    }

    @Nested
    @DisplayName("工数実績の登録")
    class LaborHoursCreation {

        @Test
        @DisplayName("工数実績を登録できる")
        void canCreateLaborHours() {
            // Arrange
            LaborHoursCreateCommand command = LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(1)
                    .workDate(LocalDate.of(2025, 1, 21))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP001")
                    .hours(new BigDecimal("2.5"))
                    .remarks("午前作業")
                    .createdBy("EMP001")
                    .build();

            // Act
            LaborHours result = laborHoursService.createLaborHours(command);

            // Assert
            assertThat(result).isNotNull();
            assertThat(result.getLaborHoursNumber()).startsWith("LH-");
            assertThat(result.getWorkOrderNumber()).isEqualTo(testWorkOrder.getWorkOrderNumber());
            assertThat(result.getSequence()).isEqualTo(1);
            assertThat(result.getProcessCode()).isEqualTo("PRESS");
            assertThat(result.getHours()).isEqualByComparingTo(new BigDecimal("2.5"));
        }

        @Test
        @DisplayName("存在しない工順ではエラーになる")
        void throwsErrorForNonExistentSequence() {
            // Arrange
            LaborHoursCreateCommand command = LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(99)
                    .workDate(LocalDate.of(2025, 1, 21))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP001")
                    .hours(new BigDecimal("2.5"))
                    .createdBy("EMP001")
                    .build();

            // Act & Assert
            assertThatThrownBy(() -> laborHoursService.createLaborHours(command))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Work order detail not found");
        }

        @Test
        @DisplayName("未着手の作業指示には工数を登録できない")
        void cannotCreateForNotStartedWorkOrder() {
            // Arrange: 新しい作業指示を作成（開始しない）
            orderRepository.save(Order.builder()
                    .orderNumber("MO-2025-002")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("PROD-001")
                    .startDate(LocalDate.of(2025, 1, 26))
                    .dueDate(LocalDate.of(2025, 1, 30))
                    .planQuantity(new BigDecimal("100"))
                    .locationCode("LINE001")
                    .status(PlanStatus.CONFIRMED)
                    .build());

            WorkOrder notStartedWorkOrder = workOrderService.createWorkOrder(WorkOrderCreateCommand.builder()
                    .orderNumber("MO-2025-002")
                    .workOrderDate(LocalDate.of(2025, 1, 25))
                    .locationCode("LINE001")
                    .plannedStartDate(LocalDate.of(2025, 1, 26))
                    .plannedEndDate(LocalDate.of(2025, 1, 30))
                    .build());

            LaborHoursCreateCommand command = LaborHoursCreateCommand.builder()
                    .workOrderNumber(notStartedWorkOrder.getWorkOrderNumber())
                    .sequence(1)
                    .workDate(LocalDate.of(2025, 1, 26))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP001")
                    .hours(new BigDecimal("2.5"))
                    .createdBy("EMP001")
                    .build();

            // Act & Assert
            assertThatThrownBy(() -> laborHoursService.createLaborHours(command))
                    .isInstanceOf(IllegalStateException.class)
                    .hasMessageContaining("Only IN_PROGRESS work orders can record labor hours");
        }
    }

    @Nested
    @DisplayName("工数集計")
    class LaborHoursAggregation {

        @Test
        @DisplayName("工順別の工数合計を取得できる")
        void canGetTotalHoursBySequence() {
            // Arrange
            laborHoursService.createLaborHours(LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(1)
                    .workDate(LocalDate.of(2025, 1, 21))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP001")
                    .hours(new BigDecimal("2.5"))
                    .createdBy("EMP001")
                    .build());
            laborHoursService.createLaborHours(LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(1)
                    .workDate(LocalDate.of(2025, 1, 21))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP002")
                    .hours(new BigDecimal("1.5"))
                    .createdBy("EMP002")
                    .build());

            // Act
            BigDecimal totalHours = laborHoursService.getTotalHoursBySequence(
                    testWorkOrder.getWorkOrderNumber(), 1);

            // Assert
            assertThat(totalHours).isEqualByComparingTo(new BigDecimal("4.0"));
        }

        @Test
        @DisplayName("工数サマリを取得できる")
        void canGetSummary() {
            // Arrange
            laborHoursService.createLaborHours(LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(1)
                    .workDate(LocalDate.of(2025, 1, 21))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP001")
                    .hours(new BigDecimal("2.0"))
                    .createdBy("EMP001")
                    .build());
            laborHoursService.createLaborHours(LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(2)
                    .workDate(LocalDate.of(2025, 1, 22))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP001")
                    .hours(new BigDecimal("3.0"))
                    .createdBy("EMP001")
                    .build());
            laborHoursService.createLaborHours(LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(3)
                    .workDate(LocalDate.of(2025, 1, 23))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP001")
                    .hours(new BigDecimal("1.0"))
                    .createdBy("EMP001")
                    .build());

            // Act
            LaborHoursSummary summary = laborHoursService.getSummary(testWorkOrder.getWorkOrderNumber());

            // Assert
            assertThat(summary.getTotalHours()).isEqualByComparingTo(new BigDecimal("6.0"));
            assertThat(summary.getProcessHours()).hasSize(3);

            // 工程別の確認
            assertThat(summary.getProcessHours().get(0).getProcessCode()).isEqualTo("PRESS");
            assertThat(summary.getProcessHours().get(0).getHours()).isEqualByComparingTo(new BigDecimal("2.0"));
            assertThat(summary.getProcessHours().get(1).getProcessCode()).isEqualTo("ASSEMBLY");
            assertThat(summary.getProcessHours().get(1).getHours()).isEqualByComparingTo(new BigDecimal("3.0"));
            assertThat(summary.getProcessHours().get(2).getProcessCode()).isEqualTo("INSPECT");
            assertThat(summary.getProcessHours().get(2).getHours()).isEqualByComparingTo(new BigDecimal("1.0"));
        }

        @Test
        @DisplayName("担当者別の工数を取得できる")
        void canGetTotalHoursByEmployee() {
            // Arrange
            laborHoursService.createLaborHours(LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(1)
                    .workDate(LocalDate.of(2025, 1, 21))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP001")
                    .hours(new BigDecimal("2.0"))
                    .createdBy("EMP001")
                    .build());
            laborHoursService.createLaborHours(LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(2)
                    .workDate(LocalDate.of(2025, 1, 22))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP001")
                    .hours(new BigDecimal("3.0"))
                    .createdBy("EMP001")
                    .build());

            // Act
            BigDecimal totalHours = laborHoursService.getTotalHoursByEmployee(
                    "EMP001", LocalDate.of(2025, 1, 1), LocalDate.of(2025, 1, 31));

            // Assert
            assertThat(totalHours).isEqualByComparingTo(new BigDecimal("5.0"));
        }
    }

    @Nested
    @DisplayName("工数実績番号の採番")
    class LaborHoursNumberGeneration {

        @Test
        @DisplayName("工数実績番号は日付ベースで連番になる")
        void generatesSequentialLaborHoursNumbers() {
            // Arrange & Act
            LaborHours result1 = laborHoursService.createLaborHours(LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(1)
                    .workDate(LocalDate.of(2025, 1, 21))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP001")
                    .hours(new BigDecimal("2.0"))
                    .createdBy("EMP001")
                    .build());

            LaborHours result2 = laborHoursService.createLaborHours(LaborHoursCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .sequence(1)
                    .workDate(LocalDate.of(2025, 1, 21))
                    .departmentCode("DEPT001")
                    .employeeCode("EMP002")
                    .hours(new BigDecimal("1.5"))
                    .createdBy("EMP002")
                    .build());

            // Assert
            assertThat(result1.getLaborHoursNumber()).startsWith("LH-202501-");
            assertThat(result2.getLaborHoursNumber()).startsWith("LH-202501-");

            // 連番であることを確認
            String num1 = result1.getLaborHoursNumber().substring(result1.getLaborHoursNumber().length() - 4);
            String num2 = result2.getLaborHoursNumber().substring(result2.getLaborHoursNumber().length() - 4);
            assertThat(Integer.parseInt(num2)).isEqualTo(Integer.parseInt(num1) + 1);
        }
    }
}
