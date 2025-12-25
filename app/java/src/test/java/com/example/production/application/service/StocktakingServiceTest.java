package com.example.production.application.service;

import com.example.production.application.port.in.command.*;
import com.example.production.application.port.out.*;
import com.example.production.domain.model.inventory.*;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("棚卸サービス")
class StocktakingServiceTest extends BaseIntegrationTest {

    @Autowired
    private StocktakingService stocktakingService;

    @Autowired
    private InventoryService inventoryService;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private LocationRepository locationRepository;

    @Autowired
    private StockRepository stockRepository;

    @Autowired
    private StocktakingRepository stocktakingRepository;

    @Autowired
    private StockAdjustmentRepository stockAdjustmentRepository;

    @BeforeEach
    void setUp() {
        // クリーンアップ（依存関係の順序に注意）
        stockAdjustmentRepository.deleteAll();
        stocktakingRepository.deleteAll();
        stockRepository.deleteAll();
        locationRepository.deleteAll();
        itemRepository.deleteAll();

        setupMasterData();
    }

    void setupMasterData() {
        // 品目マスタ
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

        // 在庫データ
        stockRepository.save(Stock.builder()
                .locationCode("WH001")
                .itemCode("MAT-001")
                .stockQuantity(new BigDecimal("100"))
                .passedQuantity(new BigDecimal("100"))
                .defectiveQuantity(BigDecimal.ZERO)
                .uninspectedQuantity(BigDecimal.ZERO)
                .build());
        stockRepository.save(Stock.builder()
                .locationCode("WH001")
                .itemCode("MAT-002")
                .stockQuantity(new BigDecimal("50"))
                .passedQuantity(new BigDecimal("50"))
                .defectiveQuantity(BigDecimal.ZERO)
                .uninspectedQuantity(BigDecimal.ZERO)
                .build());
    }

    @Nested
    @DisplayName("棚卸表発行")
    class StocktakingSheetTest {

        @Test
        @DisplayName("場所を指定して棚卸表を発行できる")
        void shouldIssueStocktakingSheet() {
            // Act
            Stocktaking stocktaking = stocktakingService.issueStocktakingSheet(
                    StocktakingIssueCommand.builder()
                            .locationCode("WH001")
                            .stocktakingDate(LocalDate.of(2025, 1, 31))
                            .build());

            // Assert
            assertThat(stocktaking).isNotNull();
            assertThat(stocktaking.getStocktakingNumber()).matches("ST-\\d{4}-\\d{4}");
            assertThat(stocktaking.getLocationCode()).isEqualTo("WH001");
            assertThat(stocktaking.getStatus()).isEqualTo(StocktakingStatus.ISSUED);
            assertThat(stocktaking.getDetails()).hasSize(2);
            assertThat(stocktaking.getDetails().get(0).getBookQuantity()).isNotNull();
        }

        @Test
        @DisplayName("棚卸データを取得できる")
        void canFindStocktaking() {
            // Arrange
            Stocktaking created = stocktakingService.issueStocktakingSheet(
                    StocktakingIssueCommand.builder()
                            .locationCode("WH001")
                            .stocktakingDate(LocalDate.of(2025, 1, 31))
                            .build());

            // Act
            Stocktaking found = stocktakingService.findByStocktakingNumber(created.getStocktakingNumber());

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getStocktakingNumber()).isEqualTo(created.getStocktakingNumber());
            assertThat(found.getDetails()).hasSize(2);
        }
    }

    @Nested
    @DisplayName("実棚入力")
    class ActualCountInputTest {

        @Test
        @DisplayName("実棚数量を入力できる")
        void shouldInputActualCount() {
            // Arrange
            Stocktaking stocktaking = stocktakingService.issueStocktakingSheet(
                    StocktakingIssueCommand.builder()
                            .locationCode("WH001")
                            .stocktakingDate(LocalDate.of(2025, 1, 31))
                            .build());

            // Act
            Stocktaking updated = stocktakingService.inputActualCount(
                    ActualCountInputCommand.builder()
                            .stocktakingNumber(stocktaking.getStocktakingNumber())
                            .details(List.of(
                                    ActualCountDetailCommand.builder()
                                            .itemCode("MAT-001")
                                            .actualQuantity(new BigDecimal("98"))
                                            .build(),
                                    ActualCountDetailCommand.builder()
                                            .itemCode("MAT-002")
                                            .actualQuantity(new BigDecimal("50"))
                                            .build()
                            ))
                            .build());

            // Assert
            assertThat(updated.getStatus()).isEqualTo(StocktakingStatus.ENTERED);

            StocktakingDetail detail1 = updated.getDetails().stream()
                    .filter(d -> d.getItemCode().equals("MAT-001"))
                    .findFirst().orElseThrow();
            assertThat(detail1.getActualQuantity()).isEqualByComparingTo(new BigDecimal("98"));
            assertThat(detail1.getDifferenceQuantity()).isEqualByComparingTo(new BigDecimal("-2")); // 98 - 100

            StocktakingDetail detail2 = updated.getDetails().stream()
                    .filter(d -> d.getItemCode().equals("MAT-002"))
                    .findFirst().orElseThrow();
            assertThat(detail2.getDifferenceQuantity()).isEqualByComparingTo(BigDecimal.ZERO); // 50 - 50
        }
    }

    @Nested
    @DisplayName("棚卸確定")
    class StocktakingConfirmTest {

        @Test
        @DisplayName("差異に基づいて在庫を調整できる")
        void shouldAdjustStockBasedOnDifference() {
            // Arrange
            Stocktaking stocktaking = stocktakingService.issueStocktakingSheet(
                    StocktakingIssueCommand.builder()
                            .locationCode("WH001")
                            .stocktakingDate(LocalDate.of(2025, 1, 31))
                            .build());

            stocktakingService.inputActualCount(
                    ActualCountInputCommand.builder()
                            .stocktakingNumber(stocktaking.getStocktakingNumber())
                            .details(List.of(
                                    ActualCountDetailCommand.builder()
                                            .itemCode("MAT-001")
                                            .actualQuantity(new BigDecimal("98"))
                                            .build(),
                                    ActualCountDetailCommand.builder()
                                            .itemCode("MAT-002")
                                            .actualQuantity(new BigDecimal("50"))
                                            .build()
                            ))
                            .build());

            // Act
            Stocktaking confirmed = stocktakingService.confirmStocktaking(
                    StocktakingConfirmCommand.builder()
                            .stocktakingNumber(stocktaking.getStocktakingNumber())
                            .adjustmentReasonCode("ADJ001")
                            .adjusterCode("EMP001")
                            .build());

            // Assert
            assertThat(confirmed.getStatus()).isEqualTo(StocktakingStatus.CONFIRMED);

            // 在庫が調整されていることを確認
            Stock stock = inventoryService.getStock("WH001", "MAT-001");
            assertThat(stock.getStockQuantity()).isEqualByComparingTo(new BigDecimal("98")); // 100 → 98
            assertThat(stock.getPassedQuantity()).isEqualByComparingTo(new BigDecimal("98"));
        }

        @Test
        @DisplayName("在庫調整履歴が記録される")
        void shouldRecordAdjustmentHistory() {
            // Arrange
            Stocktaking stocktaking = stocktakingService.issueStocktakingSheet(
                    StocktakingIssueCommand.builder()
                            .locationCode("WH001")
                            .stocktakingDate(LocalDate.of(2025, 1, 31))
                            .build());

            stocktakingService.inputActualCount(
                    ActualCountInputCommand.builder()
                            .stocktakingNumber(stocktaking.getStocktakingNumber())
                            .details(List.of(
                                    ActualCountDetailCommand.builder()
                                            .itemCode("MAT-001")
                                            .actualQuantity(new BigDecimal("98"))
                                            .build()
                            ))
                            .build());

            // Act
            stocktakingService.confirmStocktaking(
                    StocktakingConfirmCommand.builder()
                            .stocktakingNumber(stocktaking.getStocktakingNumber())
                            .adjustmentReasonCode("ADJ001")
                            .adjusterCode("EMP001")
                            .build());

            // Assert
            List<StockAdjustment> adjustments = stocktakingService.findAdjustmentsByStocktakingNumber(
                    stocktaking.getStocktakingNumber());
            assertThat(adjustments).hasSize(1);
            assertThat(adjustments.get(0).getAdjustmentQuantity()).isEqualByComparingTo(new BigDecimal("-2"));
            assertThat(adjustments.get(0).getReasonCode()).isEqualTo("ADJ001");
            assertThat(adjustments.get(0).getAdjusterCode()).isEqualTo("EMP001");
        }

        @Test
        @DisplayName("差異がない場合は在庫調整されない")
        void shouldNotAdjustWhenNoDifference() {
            // Arrange
            Stocktaking stocktaking = stocktakingService.issueStocktakingSheet(
                    StocktakingIssueCommand.builder()
                            .locationCode("WH001")
                            .stocktakingDate(LocalDate.of(2025, 1, 31))
                            .build());

            stocktakingService.inputActualCount(
                    ActualCountInputCommand.builder()
                            .stocktakingNumber(stocktaking.getStocktakingNumber())
                            .details(List.of(
                                    ActualCountDetailCommand.builder()
                                            .itemCode("MAT-001")
                                            .actualQuantity(new BigDecimal("100")) // 帳簿と同じ
                                            .build(),
                                    ActualCountDetailCommand.builder()
                                            .itemCode("MAT-002")
                                            .actualQuantity(new BigDecimal("50")) // 帳簿と同じ
                                            .build()
                            ))
                            .build());

            // Act
            stocktakingService.confirmStocktaking(
                    StocktakingConfirmCommand.builder()
                            .stocktakingNumber(stocktaking.getStocktakingNumber())
                            .adjustmentReasonCode("ADJ001")
                            .adjusterCode("EMP001")
                            .build());

            // Assert
            List<StockAdjustment> adjustments = stocktakingService.findAdjustmentsByStocktakingNumber(
                    stocktaking.getStocktakingNumber());
            assertThat(adjustments).isEmpty();

            // 在庫は変わっていないことを確認
            Stock stock1 = inventoryService.getStock("WH001", "MAT-001");
            assertThat(stock1.getStockQuantity()).isEqualByComparingTo(new BigDecimal("100"));

            Stock stock2 = inventoryService.getStock("WH001", "MAT-002");
            assertThat(stock2.getStockQuantity()).isEqualByComparingTo(new BigDecimal("50"));
        }
    }

    @Nested
    @DisplayName("棚卸番号の採番")
    class StocktakingNumberGenerationTest {

        @Test
        @DisplayName("棚卸番号は連番で採番される")
        void stocktakingNumbersAreSequential() {
            // Act
            Stocktaking stocktaking1 = stocktakingService.issueStocktakingSheet(
                    StocktakingIssueCommand.builder()
                            .locationCode("WH001")
                            .stocktakingDate(LocalDate.of(2025, 1, 31))
                            .build());

            Stocktaking stocktaking2 = stocktakingService.issueStocktakingSheet(
                    StocktakingIssueCommand.builder()
                            .locationCode("WH001")
                            .stocktakingDate(LocalDate.of(2025, 2, 28))
                            .build());

            // Assert
            assertThat(stocktaking1.getStocktakingNumber()).matches("ST-\\d{4}-0001");
            assertThat(stocktaking2.getStocktakingNumber()).matches("ST-\\d{4}-0002");
        }
    }
}
