package com.example.production.application.service;

import com.example.production.application.port.in.command.CompletionResultCreateCommand;
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
import com.example.production.domain.model.process.CompletionResult;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;
import com.example.production.domain.model.purchase.Defect;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("完成実績サービス")
class CompletionResultServiceTest extends BaseIntegrationTest {

    @Autowired
    private CompletionResultService completionResultService;

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
    private CompletionResultRepository completionResultRepository;

    @Autowired
    private CompletionInspectionResultRepository completionInspectionResultRepository;

    @Autowired
    private DefectRepository defectRepository;

    private WorkOrder testWorkOrder;

    @BeforeEach
    void setUp() {
        // クリーンアップ（依存関係の順序に注意）
        completionInspectionResultRepository.deleteAll();
        completionResultRepository.deleteAll();
        workOrderDetailRepository.deleteAll();
        workOrderRepository.deleteAll();
        routingRepository.deleteAll();
        orderRepository.deleteAll();
        defectRepository.deleteAll();
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

        // 欠点マスタ
        defectRepository.save(Defect.builder()
                .defectCode("DEF001")
                .defectDescription("キズ")
                .build());
        defectRepository.save(Defect.builder()
                .defectCode("DEF002")
                .defectDescription("汚れ")
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
    @DisplayName("完成実績の登録")
    class CompletionResultCreation {

        @Test
        @DisplayName("完成実績を登録できる")
        void canCreateCompletionResult() {
            // Arrange
            CompletionResultCreateCommand command = CompletionResultCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .completionDate(LocalDate.of(2025, 1, 22))
                    .completedQuantity(new BigDecimal("50"))
                    .goodQuantity(new BigDecimal("48"))
                    .defectQuantity(new BigDecimal("2"))
                    .remarks("午前の生産分")
                    .createdBy("operator1")
                    .build();

            // Act
            CompletionResult result = completionResultService.createCompletionResult(command);

            // Assert
            assertThat(result).isNotNull();
            assertThat(result.getCompletionResultNumber()).startsWith("CR-");
            assertThat(result.getWorkOrderNumber()).isEqualTo(testWorkOrder.getWorkOrderNumber());
            assertThat(result.getItemCode()).isEqualTo("PROD-001");
            assertThat(result.getCompletedQuantity()).isEqualByComparingTo(new BigDecimal("50"));
            assertThat(result.getGoodQuantity()).isEqualByComparingTo(new BigDecimal("48"));
            assertThat(result.getDefectQuantity()).isEqualByComparingTo(new BigDecimal("2"));
        }

        @Test
        @DisplayName("検査結果を含む完成実績を登録できる")
        void canCreateCompletionResultWithInspectionResults() {
            // Arrange
            CompletionResultCreateCommand command = CompletionResultCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .completionDate(LocalDate.of(2025, 1, 22))
                    .completedQuantity(new BigDecimal("50"))
                    .goodQuantity(new BigDecimal("48"))
                    .defectQuantity(new BigDecimal("2"))
                    .createdBy("operator1")
                    .inspectionResults(List.of(
                            CompletionResultCreateCommand.InspectionResultCommand.builder()
                                    .defectCode("DEF001")
                                    .quantity(new BigDecimal("1"))
                                    .build(),
                            CompletionResultCreateCommand.InspectionResultCommand.builder()
                                    .defectCode("DEF002")
                                    .quantity(new BigDecimal("1"))
                                    .build()
                    ))
                    .build();

            // Act
            CompletionResult result = completionResultService.createCompletionResult(command);

            // Assert
            assertThat(result.getInspectionResults()).hasSize(2);
        }

        @Test
        @DisplayName("完成実績登録時に作業指示の数量が更新される")
        void workOrderQuantitiesAreUpdated() {
            // Arrange
            CompletionResultCreateCommand command = CompletionResultCreateCommand.builder()
                    .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .completionDate(LocalDate.of(2025, 1, 22))
                    .completedQuantity(new BigDecimal("50"))
                    .goodQuantity(new BigDecimal("48"))
                    .defectQuantity(new BigDecimal("2"))
                    .createdBy("operator1")
                    .build();

            // Act
            completionResultService.createCompletionResult(command);

            // Assert
            WorkOrder updatedWorkOrder = workOrderRepository.findByWorkOrderNumber(testWorkOrder.getWorkOrderNumber())
                    .orElseThrow();
            assertThat(updatedWorkOrder.getCompletedQuantity()).isEqualByComparingTo(new BigDecimal("50"));
            assertThat(updatedWorkOrder.getTotalGoodQuantity()).isEqualByComparingTo(new BigDecimal("48"));
            assertThat(updatedWorkOrder.getTotalDefectQuantity()).isEqualByComparingTo(new BigDecimal("2"));
        }

        @Test
        @DisplayName("未着手の作業指示には完成実績を登録できない")
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

            CompletionResultCreateCommand command = CompletionResultCreateCommand.builder()
                    .workOrderNumber(notStartedWorkOrder.getWorkOrderNumber())
                    .completionDate(LocalDate.of(2025, 1, 26))
                    .completedQuantity(new BigDecimal("50"))
                    .goodQuantity(new BigDecimal("50"))
                    .defectQuantity(BigDecimal.ZERO)
                    .createdBy("operator1")
                    .build();

            // Act & Assert
            assertThatThrownBy(() -> completionResultService.createCompletionResult(command))
                    .isInstanceOf(IllegalStateException.class)
                    .hasMessageContaining("Only IN_PROGRESS work orders can record completion results");
        }
    }

    @Nested
    @DisplayName("完成実績の検索")
    class CompletionResultSearch {

        @Test
        @DisplayName("完成実績番号で検索できる")
        void canFindByCompletionResultNumber() {
            // Arrange
            CompletionResult created = completionResultService.createCompletionResult(
                    CompletionResultCreateCommand.builder()
                            .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                            .completionDate(LocalDate.of(2025, 1, 22))
                            .completedQuantity(new BigDecimal("50"))
                            .goodQuantity(new BigDecimal("48"))
                            .defectQuantity(new BigDecimal("2"))
                            .createdBy("operator1")
                            .build());

            // Act
            CompletionResult found = completionResultService.findByCompletionResultNumber(created.getCompletionResultNumber());

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getCompletionResultNumber()).isEqualTo(created.getCompletionResultNumber());
        }

        @Test
        @DisplayName("作業指示番号で完成実績を検索できる")
        void canFindByWorkOrderNumber() {
            // Arrange
            completionResultService.createCompletionResult(
                    CompletionResultCreateCommand.builder()
                            .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                            .completionDate(LocalDate.of(2025, 1, 22))
                            .completedQuantity(new BigDecimal("30"))
                            .goodQuantity(new BigDecimal("28"))
                            .defectQuantity(new BigDecimal("2"))
                            .createdBy("operator1")
                            .build());
            completionResultService.createCompletionResult(
                    CompletionResultCreateCommand.builder()
                            .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                            .completionDate(LocalDate.of(2025, 1, 22))
                            .completedQuantity(new BigDecimal("20"))
                            .goodQuantity(new BigDecimal("20"))
                            .defectQuantity(BigDecimal.ZERO)
                            .createdBy("operator1")
                            .build());

            // Act
            List<CompletionResult> results = completionResultService.findByWorkOrderNumber(testWorkOrder.getWorkOrderNumber());

            // Assert
            assertThat(results).hasSize(2);
        }
    }

    @Nested
    @DisplayName("完成実績番号の採番")
    class CompletionResultNumberGeneration {

        @Test
        @DisplayName("完成実績番号は日付ベースで連番になる")
        void generatesSequentialCompletionResultNumbers() {
            // Arrange & Act
            CompletionResult result1 = completionResultService.createCompletionResult(
                    CompletionResultCreateCommand.builder()
                            .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                            .completionDate(LocalDate.of(2025, 1, 22))
                            .completedQuantity(new BigDecimal("30"))
                            .goodQuantity(new BigDecimal("30"))
                            .defectQuantity(BigDecimal.ZERO)
                            .createdBy("operator1")
                            .build());

            CompletionResult result2 = completionResultService.createCompletionResult(
                    CompletionResultCreateCommand.builder()
                            .workOrderNumber(testWorkOrder.getWorkOrderNumber())
                            .completionDate(LocalDate.of(2025, 1, 22))
                            .completedQuantity(new BigDecimal("20"))
                            .goodQuantity(new BigDecimal("20"))
                            .defectQuantity(BigDecimal.ZERO)
                            .createdBy("operator1")
                            .build());

            // Assert
            assertThat(result1.getCompletionResultNumber()).startsWith("CR-202501-");
            assertThat(result2.getCompletionResultNumber()).startsWith("CR-202501-");

            // 連番であることを確認
            String num1 = result1.getCompletionResultNumber().substring(result1.getCompletionResultNumber().length() - 4);
            String num2 = result2.getCompletionResultNumber().substring(result2.getCompletionResultNumber().length() - 4);
            assertThat(Integer.parseInt(num2)).isEqualTo(Integer.parseInt(num1) + 1);
        }
    }
}
