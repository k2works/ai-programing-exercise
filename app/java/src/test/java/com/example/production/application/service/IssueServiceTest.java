package com.example.production.application.service;

import com.example.production.application.port.in.command.*;
import com.example.production.application.port.out.*;
import com.example.production.domain.model.inventory.*;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("払出サービス")
class IssueServiceTest extends BaseIntegrationTest {

    @Autowired
    private IssueService issueService;

    @Autowired
    private InventoryService inventoryService;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private LocationRepository locationRepository;

    @Autowired
    private OrderRepository orderRepository;

    @Autowired
    private StockRepository stockRepository;

    @Autowired
    private IssueInstructionRepository issueInstructionRepository;

    @Autowired
    private IssueRepository issueRepository;

    @BeforeEach
    void setUp() {
        // クリーンアップ（依存関係の順序に注意）
        issueRepository.deleteAll();
        issueInstructionRepository.deleteAll();
        stockRepository.deleteAll();
        orderRepository.deleteAll();
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
        itemRepository.save(Item.builder()
                .itemCode("MAT-001")
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .itemName("材料A")
                .itemCategory(ItemCategory.MATERIAL)
                .build());
        itemRepository.save(Item.builder()
                .itemCode("MAT-002")
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .itemName("材料B")
                .itemCategory(ItemCategory.MATERIAL)
                .build());

        // 場所マスタ
        locationRepository.save(Location.builder()
                .locationCode("WH001")
                .locationName("資材倉庫")
                .locationType(LocationType.WAREHOUSE)
                .build());
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

        // 在庫データ
        stockRepository.save(Stock.builder()
                .locationCode("WH001")
                .itemCode("MAT-001")
                .stockQuantity(new BigDecimal("1000"))
                .passedQuantity(new BigDecimal("1000"))
                .defectiveQuantity(BigDecimal.ZERO)
                .uninspectedQuantity(BigDecimal.ZERO)
                .build());
        stockRepository.save(Stock.builder()
                .locationCode("WH001")
                .itemCode("MAT-002")
                .stockQuantity(new BigDecimal("500"))
                .passedQuantity(new BigDecimal("500"))
                .defectiveQuantity(BigDecimal.ZERO)
                .uninspectedQuantity(BigDecimal.ZERO)
                .build());
    }

    @Nested
    @DisplayName("払出指示")
    class IssueInstructionTest {

        @Test
        @DisplayName("オーダ情報から払出指示を作成できる")
        void canCreateIssueInstructionFromOrder() {
            // Arrange
            IssueInstructionCreateCommand command = IssueInstructionCreateCommand.builder()
                    .orderNumber("MO-2025-001")
                    .instructionDate(LocalDate.of(2025, 1, 20))
                    .locationCode("WH001")
                    .details(List.of(
                            IssueInstructionDetailCommand.builder()
                                    .itemCode("MAT-001")
                                    .routingSequence(1)
                                    .issueQuantity(new BigDecimal("100"))
                                    .build(),
                            IssueInstructionDetailCommand.builder()
                                    .itemCode("MAT-002")
                                    .routingSequence(1)
                                    .issueQuantity(new BigDecimal("200"))
                                    .build()
                    ))
                    .build();

            // Act
            IssueInstruction instruction = issueService.createIssueInstruction(command);

            // Assert
            assertThat(instruction).isNotNull();
            assertThat(instruction.getInstructionNumber()).matches("IS-\\d{4}-\\d{4}");
            assertThat(instruction.getOrderNumber()).isEqualTo("MO-2025-001");
            assertThat(instruction.getLocationCode()).isEqualTo("WH001");
            assertThat(instruction.getDetails()).hasSize(2);
        }

        @Test
        @DisplayName("払出指示を取得できる")
        void canFindIssueInstruction() {
            // Arrange
            IssueInstruction created = issueService.createIssueInstruction(
                    IssueInstructionCreateCommand.builder()
                            .orderNumber("MO-2025-001")
                            .instructionDate(LocalDate.of(2025, 1, 20))
                            .locationCode("WH001")
                            .details(List.of(
                                    IssueInstructionDetailCommand.builder()
                                            .itemCode("MAT-001")
                                            .routingSequence(1)
                                            .issueQuantity(new BigDecimal("100"))
                                            .build()
                            ))
                            .build());

            // Act
            IssueInstruction found = issueService.findByInstructionNumber(created.getInstructionNumber());

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getInstructionNumber()).isEqualTo(created.getInstructionNumber());
            assertThat(found.getDetails()).hasSize(1);
        }
    }

    @Nested
    @DisplayName("払出実行")
    class IssueExecutionTest {

        @Test
        @DisplayName("払出を実行できる")
        void canExecuteIssue() {
            // Arrange
            IssueExecuteCommand command = IssueExecuteCommand.builder()
                    .workOrderNumber("WO-2025-0001")
                    .routingSequence(1)
                    .locationCode("WH001")
                    .issueDate(LocalDate.of(2025, 1, 21))
                    .issuerCode("EMP001")
                    .details(List.of(
                            IssueDetailCommand.builder()
                                    .itemCode("MAT-001")
                                    .issueQuantity(new BigDecimal("50"))
                                    .build()
                    ))
                    .build();

            // Act
            Issue issue = issueService.executeIssue(command);

            // Assert
            assertThat(issue).isNotNull();
            assertThat(issue.getIssueNumber()).matches("PO-\\d{4}-\\d{4}");
            assertThat(issue.getWorkOrderNumber()).isEqualTo("WO-2025-0001");
            assertThat(issue.getDetails()).hasSize(1);

            // 在庫が減少していることを確認
            Stock stock = inventoryService.getStock("WH001", "MAT-001");
            assertThat(stock.getStockQuantity()).isEqualByComparingTo(new BigDecimal("950")); // 1000 - 50
            assertThat(stock.getPassedQuantity()).isEqualByComparingTo(new BigDecimal("950"));
        }

        @Test
        @DisplayName("複数品目を同時に払出できる")
        void canExecuteIssueWithMultipleItems() {
            // Arrange
            IssueExecuteCommand command = IssueExecuteCommand.builder()
                    .workOrderNumber("WO-2025-0001")
                    .routingSequence(1)
                    .locationCode("WH001")
                    .issueDate(LocalDate.of(2025, 1, 21))
                    .issuerCode("EMP001")
                    .details(List.of(
                            IssueDetailCommand.builder()
                                    .itemCode("MAT-001")
                                    .issueQuantity(new BigDecimal("100"))
                                    .build(),
                            IssueDetailCommand.builder()
                                    .itemCode("MAT-002")
                                    .issueQuantity(new BigDecimal("200"))
                                    .build()
                    ))
                    .build();

            // Act
            Issue issue = issueService.executeIssue(command);

            // Assert
            assertThat(issue.getDetails()).hasSize(2);

            Stock stock1 = inventoryService.getStock("WH001", "MAT-001");
            assertThat(stock1.getPassedQuantity()).isEqualByComparingTo(new BigDecimal("900")); // 1000 - 100

            Stock stock2 = inventoryService.getStock("WH001", "MAT-002");
            assertThat(stock2.getPassedQuantity()).isEqualByComparingTo(new BigDecimal("300")); // 500 - 200
        }

        @Test
        @DisplayName("在庫不足の場合はエラーになる")
        void throwsExceptionWhenStockInsufficient() {
            // Arrange
            IssueExecuteCommand command = IssueExecuteCommand.builder()
                    .workOrderNumber("WO-2025-0001")
                    .routingSequence(1)
                    .locationCode("WH001")
                    .issueDate(LocalDate.of(2025, 1, 21))
                    .issuerCode("EMP001")
                    .details(List.of(
                            IssueDetailCommand.builder()
                                    .itemCode("MAT-001")
                                    .issueQuantity(new BigDecimal("2000")) // 在庫 1000 を超える
                                    .build()
                    ))
                    .build();

            // Act & Assert
            assertThatThrownBy(() -> issueService.executeIssue(command))
                    .isInstanceOf(InsufficientStockException.class)
                    .hasMessageContaining("在庫が不足しています");
        }

        @Test
        @DisplayName("払出を取得できる")
        void canFindIssue() {
            // Arrange
            Issue created = issueService.executeIssue(IssueExecuteCommand.builder()
                    .workOrderNumber("WO-2025-0001")
                    .routingSequence(1)
                    .locationCode("WH001")
                    .issueDate(LocalDate.of(2025, 1, 21))
                    .issuerCode("EMP001")
                    .details(List.of(
                            IssueDetailCommand.builder()
                                    .itemCode("MAT-001")
                                    .issueQuantity(new BigDecimal("50"))
                                    .build()
                    ))
                    .build());

            // Act
            Issue found = issueService.findByIssueNumber(created.getIssueNumber());

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getIssueNumber()).isEqualTo(created.getIssueNumber());
            assertThat(found.getDetails()).hasSize(1);
        }

        @Test
        @DisplayName("作業指示番号で払出を検索できる")
        void canFindByWorkOrderNumber() {
            // Arrange
            issueService.executeIssue(IssueExecuteCommand.builder()
                    .workOrderNumber("WO-2025-0001")
                    .routingSequence(1)
                    .locationCode("WH001")
                    .issueDate(LocalDate.of(2025, 1, 21))
                    .issuerCode("EMP001")
                    .details(List.of(
                            IssueDetailCommand.builder()
                                    .itemCode("MAT-001")
                                    .issueQuantity(new BigDecimal("50"))
                                    .build()
                    ))
                    .build());
            issueService.executeIssue(IssueExecuteCommand.builder()
                    .workOrderNumber("WO-2025-0001")
                    .routingSequence(2)
                    .locationCode("WH001")
                    .issueDate(LocalDate.of(2025, 1, 22))
                    .issuerCode("EMP001")
                    .details(List.of(
                            IssueDetailCommand.builder()
                                    .itemCode("MAT-002")
                                    .issueQuantity(new BigDecimal("30"))
                                    .build()
                    ))
                    .build());

            // Act
            List<Issue> issues = issueService.findByWorkOrderNumber("WO-2025-0001");

            // Assert
            assertThat(issues).hasSize(2);
        }
    }

    @Nested
    @DisplayName("払出番号の採番")
    class IssueNumberGenerationTest {

        @Test
        @DisplayName("払出指示番号は連番で採番される")
        void issueInstructionNumbersAreSequential() {
            // Act
            IssueInstruction instruction1 = issueService.createIssueInstruction(
                    IssueInstructionCreateCommand.builder()
                            .orderNumber("MO-2025-001")
                            .instructionDate(LocalDate.of(2025, 1, 20))
                            .locationCode("WH001")
                            .details(List.of(
                                    IssueInstructionDetailCommand.builder()
                                            .itemCode("MAT-001")
                                            .routingSequence(1)
                                            .issueQuantity(new BigDecimal("100"))
                                            .build()
                            ))
                            .build());

            IssueInstruction instruction2 = issueService.createIssueInstruction(
                    IssueInstructionCreateCommand.builder()
                            .orderNumber("MO-2025-001")
                            .instructionDate(LocalDate.of(2025, 1, 21))
                            .locationCode("WH001")
                            .details(List.of(
                                    IssueInstructionDetailCommand.builder()
                                            .itemCode("MAT-002")
                                            .routingSequence(1)
                                            .issueQuantity(new BigDecimal("50"))
                                            .build()
                            ))
                            .build());

            // Assert
            assertThat(instruction1.getInstructionNumber()).matches("IS-\\d{4}-0001");
            assertThat(instruction2.getInstructionNumber()).matches("IS-\\d{4}-0002");
        }

        @Test
        @DisplayName("払出番号は連番で採番される")
        void issueNumbersAreSequential() {
            // Act
            Issue issue1 = issueService.executeIssue(IssueExecuteCommand.builder()
                    .workOrderNumber("WO-2025-0001")
                    .routingSequence(1)
                    .locationCode("WH001")
                    .issueDate(LocalDate.of(2025, 1, 21))
                    .issuerCode("EMP001")
                    .details(List.of(
                            IssueDetailCommand.builder()
                                    .itemCode("MAT-001")
                                    .issueQuantity(new BigDecimal("50"))
                                    .build()
                    ))
                    .build());

            Issue issue2 = issueService.executeIssue(IssueExecuteCommand.builder()
                    .workOrderNumber("WO-2025-0002")
                    .routingSequence(1)
                    .locationCode("WH001")
                    .issueDate(LocalDate.of(2025, 1, 22))
                    .issuerCode("EMP001")
                    .details(List.of(
                            IssueDetailCommand.builder()
                                    .itemCode("MAT-002")
                                    .issueQuantity(new BigDecimal("30"))
                                    .build()
                    ))
                    .build());

            // Assert
            assertThat(issue1.getIssueNumber()).matches("PO-\\d{4}-0001");
            assertThat(issue2.getIssueNumber()).matches("PO-\\d{4}-0002");
        }
    }
}
