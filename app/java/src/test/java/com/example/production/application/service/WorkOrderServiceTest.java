package com.example.production.application.service;

import com.example.production.application.port.in.command.WorkOrderCreateCommand;
import com.example.production.application.port.out.*;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.domain.model.process.Process;
import com.example.production.domain.model.process.Routing;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderDetail;
import com.example.production.domain.model.process.WorkOrderStatus;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("作業指示サービス")
class WorkOrderServiceTest extends BaseIntegrationTest {

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

    @BeforeEach
    void setUp() {
        workOrderDetailRepository.deleteAll();
        workOrderRepository.deleteAll();
        routingRepository.deleteAll();
        orderRepository.deleteAll();
        processRepository.deleteAll();
        locationRepository.deleteAll();
        itemRepository.deleteAll();

        setupMasterData();
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

    @Nested
    @DisplayName("作業指示の作成")
    class WorkOrderCreation {

        @Test
        @DisplayName("オーダ情報から作業指示を作成できる")
        void canCreateWorkOrderFromOrder() {
            // Arrange
            WorkOrderCreateCommand command = WorkOrderCreateCommand.builder()
                    .orderNumber("MO-2025-001")
                    .workOrderDate(LocalDate.of(2025, 1, 20))
                    .locationCode("LINE001")
                    .plannedStartDate(LocalDate.of(2025, 1, 21))
                    .plannedEndDate(LocalDate.of(2025, 1, 25))
                    .build();

            // Act
            WorkOrder workOrder = workOrderService.createWorkOrder(command);

            // Assert
            assertThat(workOrder).isNotNull();
            assertThat(workOrder.getWorkOrderNumber()).startsWith("WO-");
            assertThat(workOrder.getItemCode()).isEqualTo("PROD-001");
            assertThat(workOrder.getOrderQuantity()).isEqualByComparingTo(new BigDecimal("100"));
            assertThat(workOrder.getDetails()).hasSize(3);
        }

        @Test
        @DisplayName("工程表の工順に従って明細が作成される")
        void detailsAreCreatedAccordingToRouting() {
            // Arrange
            WorkOrderCreateCommand command = WorkOrderCreateCommand.builder()
                    .orderNumber("MO-2025-001")
                    .workOrderDate(LocalDate.of(2025, 1, 20))
                    .locationCode("LINE001")
                    .plannedStartDate(LocalDate.of(2025, 1, 21))
                    .plannedEndDate(LocalDate.of(2025, 1, 25))
                    .build();

            // Act
            WorkOrder workOrder = workOrderService.createWorkOrder(command);

            // Assert: 工順の順序を確認
            List<Integer> sequences = workOrder.getDetails().stream()
                    .map(WorkOrderDetail::getSequence)
                    .sorted()
                    .toList();
            assertThat(sequences).containsExactly(1, 2, 3);

            // 各工程の確認
            List<String> processCodes = workOrder.getDetails().stream()
                    .map(WorkOrderDetail::getProcessCode)
                    .toList();
            assertThat(processCodes).containsExactlyInAnyOrder("PRESS", "ASSEMBLY", "INSPECT");
        }

        @Test
        @DisplayName("存在しないオーダ番号でエラーになる")
        void throwsErrorForNonExistentOrder() {
            // Arrange
            WorkOrderCreateCommand command = WorkOrderCreateCommand.builder()
                    .orderNumber("NON-EXISTENT")
                    .workOrderDate(LocalDate.of(2025, 1, 20))
                    .locationCode("LINE001")
                    .plannedStartDate(LocalDate.of(2025, 1, 21))
                    .plannedEndDate(LocalDate.of(2025, 1, 25))
                    .build();

            // Act & Assert
            assertThatThrownBy(() -> workOrderService.createWorkOrder(command))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Order not found");
        }
    }

    @Nested
    @DisplayName("作業指示のステータス管理")
    class WorkOrderStatusManagement {

        @Test
        @DisplayName("作業指示を開始できる")
        void canStartWorkOrder() {
            // Arrange
            WorkOrder workOrder = workOrderService.createWorkOrder(WorkOrderCreateCommand.builder()
                    .orderNumber("MO-2025-001")
                    .workOrderDate(LocalDate.of(2025, 1, 20))
                    .locationCode("LINE001")
                    .plannedStartDate(LocalDate.of(2025, 1, 21))
                    .plannedEndDate(LocalDate.of(2025, 1, 25))
                    .build());

            // Act
            WorkOrder updated = workOrderService.startWork(workOrder.getWorkOrderNumber());

            // Assert
            assertThat(updated.getStatus()).isEqualTo(WorkOrderStatus.IN_PROGRESS);
            assertThat(updated.getActualStartDate()).isNotNull();
        }

        @Test
        @DisplayName("作業指示を完了できる")
        void canCompleteWorkOrder() {
            // Arrange
            WorkOrder workOrder = workOrderService.createWorkOrder(WorkOrderCreateCommand.builder()
                    .orderNumber("MO-2025-001")
                    .workOrderDate(LocalDate.of(2025, 1, 20))
                    .locationCode("LINE001")
                    .plannedStartDate(LocalDate.of(2025, 1, 21))
                    .plannedEndDate(LocalDate.of(2025, 1, 25))
                    .build());
            workOrderService.startWork(workOrder.getWorkOrderNumber());

            // Act
            WorkOrder updated = workOrderService.completeWork(workOrder.getWorkOrderNumber());

            // Assert
            assertThat(updated.getStatus()).isEqualTo(WorkOrderStatus.COMPLETED);
            assertThat(updated.getCompletedFlag()).isTrue();
            assertThat(updated.getActualEndDate()).isNotNull();
        }

        @Test
        @DisplayName("未着手状態から直接完了にはできない")
        void cannotCompleteFromNotStarted() {
            // Arrange
            WorkOrder workOrder = workOrderService.createWorkOrder(WorkOrderCreateCommand.builder()
                    .orderNumber("MO-2025-001")
                    .workOrderDate(LocalDate.of(2025, 1, 20))
                    .locationCode("LINE001")
                    .plannedStartDate(LocalDate.of(2025, 1, 21))
                    .plannedEndDate(LocalDate.of(2025, 1, 25))
                    .build());

            // Act & Assert
            assertThatThrownBy(() -> workOrderService.completeWork(workOrder.getWorkOrderNumber()))
                    .isInstanceOf(IllegalStateException.class)
                    .hasMessageContaining("Only IN_PROGRESS work orders can be completed");
        }
    }

    @Nested
    @DisplayName("作業指示番号の採番")
    class WorkOrderNumberGeneration {

        @Test
        @DisplayName("作業指示番号は日付ベースで連番になる")
        void generatesSequentialWorkOrderNumbers() {
            // Arrange & Act
            WorkOrder workOrder1 = workOrderService.createWorkOrder(WorkOrderCreateCommand.builder()
                    .orderNumber("MO-2025-001")
                    .workOrderDate(LocalDate.of(2025, 1, 20))
                    .locationCode("LINE001")
                    .plannedStartDate(LocalDate.of(2025, 1, 21))
                    .plannedEndDate(LocalDate.of(2025, 1, 25))
                    .build());

            WorkOrder workOrder2 = workOrderService.createWorkOrder(WorkOrderCreateCommand.builder()
                    .orderNumber("MO-2025-001")
                    .workOrderDate(LocalDate.of(2025, 1, 20))
                    .locationCode("LINE001")
                    .plannedStartDate(LocalDate.of(2025, 1, 21))
                    .plannedEndDate(LocalDate.of(2025, 1, 25))
                    .build());

            // Assert
            assertThat(workOrder1.getWorkOrderNumber()).startsWith("WO-202501-");
            assertThat(workOrder2.getWorkOrderNumber()).startsWith("WO-202501-");

            // 連番であることを確認
            String num1 = workOrder1.getWorkOrderNumber().substring(workOrder1.getWorkOrderNumber().length() - 4);
            String num2 = workOrder2.getWorkOrderNumber().substring(workOrder2.getWorkOrderNumber().length() - 4);
            assertThat(Integer.parseInt(num2)).isEqualTo(Integer.parseInt(num1) + 1);
        }
    }
}
