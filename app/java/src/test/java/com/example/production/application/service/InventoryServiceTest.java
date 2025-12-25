package com.example.production.application.service;

import com.example.production.application.port.in.command.StockChangeCommand;
import com.example.production.application.port.in.command.StockStatusChangeCommand;
import com.example.production.application.port.out.*;
import com.example.production.domain.model.inventory.InsufficientStockException;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.domain.model.inventory.StockStatus;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.*;

@DisplayName("在庫管理サービス")
class InventoryServiceTest extends BaseIntegrationTest {

    @Autowired
    private InventoryService inventoryService;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private LocationRepository locationRepository;

    @Autowired
    private StockRepository stockRepository;

    @BeforeEach
    void setUp() {
        // クリーンアップ
        stockRepository.deleteAll();
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
    }

    @Nested
    @DisplayName("在庫の参照")
    class StockReference {

        @Test
        @DisplayName("場所と品目で在庫を取得できる")
        void canGetStockByLocationAndItem() {
            // Arrange
            stockRepository.save(Stock.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .stockQuantity(new BigDecimal("100"))
                    .passedQuantity(new BigDecimal("95"))
                    .defectiveQuantity(new BigDecimal("3"))
                    .uninspectedQuantity(new BigDecimal("2"))
                    .build());

            // Act
            Stock stock = inventoryService.getStock("WH001", "MAT-001");

            // Assert
            assertThat(stock).isNotNull();
            assertThat(stock.getStockQuantity()).isEqualByComparingTo(new BigDecimal("100"));
            assertThat(stock.getPassedQuantity()).isEqualByComparingTo(new BigDecimal("95"));
            assertThat(stock.getDefectiveQuantity()).isEqualByComparingTo(new BigDecimal("3"));
            assertThat(stock.getUninspectedQuantity()).isEqualByComparingTo(new BigDecimal("2"));
        }

        @Test
        @DisplayName("存在しない在庫は0として返す")
        void returnsZeroForNonExistentStock() {
            // Act
            Stock stock = inventoryService.getStock("WH001", "NONEXISTENT");

            // Assert
            assertThat(stock.getStockQuantity()).isEqualByComparingTo(BigDecimal.ZERO);
            assertThat(stock.getPassedQuantity()).isEqualByComparingTo(BigDecimal.ZERO);
            assertThat(stock.getDefectiveQuantity()).isEqualByComparingTo(BigDecimal.ZERO);
            assertThat(stock.getUninspectedQuantity()).isEqualByComparingTo(BigDecimal.ZERO);
        }
    }

    @Nested
    @DisplayName("在庫の増減")
    class StockChange {

        @Test
        @DisplayName("新規在庫を登録できる")
        void canCreateNewStock() {
            // Arrange
            StockChangeCommand command = StockChangeCommand.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .quantity(new BigDecimal("50"))
                    .stockStatus(StockStatus.PASSED)
                    .build();

            // Act
            inventoryService.increaseStock(command);

            // Assert
            Stock stock = inventoryService.getStock("WH001", "MAT-001");
            assertThat(stock.getStockQuantity()).isEqualByComparingTo(new BigDecimal("50"));
            assertThat(stock.getPassedQuantity()).isEqualByComparingTo(new BigDecimal("50"));
        }

        @Test
        @DisplayName("既存在庫を増加できる")
        void canIncreaseExistingStock() {
            // Arrange
            stockRepository.save(Stock.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .stockQuantity(new BigDecimal("100"))
                    .passedQuantity(new BigDecimal("100"))
                    .defectiveQuantity(BigDecimal.ZERO)
                    .uninspectedQuantity(BigDecimal.ZERO)
                    .build());

            StockChangeCommand command = StockChangeCommand.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .quantity(new BigDecimal("50"))
                    .stockStatus(StockStatus.PASSED)
                    .build();

            // Act
            inventoryService.increaseStock(command);

            // Assert
            Stock stock = inventoryService.getStock("WH001", "MAT-001");
            assertThat(stock.getStockQuantity()).isEqualByComparingTo(new BigDecimal("150"));
            assertThat(stock.getPassedQuantity()).isEqualByComparingTo(new BigDecimal("150"));
        }

        @Test
        @DisplayName("在庫を減少できる")
        void canDecreaseStock() {
            // Arrange
            stockRepository.save(Stock.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .stockQuantity(new BigDecimal("100"))
                    .passedQuantity(new BigDecimal("100"))
                    .defectiveQuantity(BigDecimal.ZERO)
                    .uninspectedQuantity(BigDecimal.ZERO)
                    .build());

            StockChangeCommand command = StockChangeCommand.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .quantity(new BigDecimal("30"))
                    .stockStatus(StockStatus.PASSED)
                    .build();

            // Act
            inventoryService.decreaseStock(command);

            // Assert
            Stock stock = inventoryService.getStock("WH001", "MAT-001");
            assertThat(stock.getStockQuantity()).isEqualByComparingTo(new BigDecimal("70"));
            assertThat(stock.getPassedQuantity()).isEqualByComparingTo(new BigDecimal("70"));
        }

        @Test
        @DisplayName("在庫以上の数量を減少しようとするとエラー")
        void throwsExceptionWhenDecreasingMoreThanStock() {
            // Arrange
            stockRepository.save(Stock.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .stockQuantity(new BigDecimal("100"))
                    .passedQuantity(new BigDecimal("100"))
                    .defectiveQuantity(BigDecimal.ZERO)
                    .uninspectedQuantity(BigDecimal.ZERO)
                    .build());

            StockChangeCommand command = StockChangeCommand.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .quantity(new BigDecimal("200"))
                    .stockStatus(StockStatus.PASSED)
                    .build();

            // Act & Assert
            assertThatThrownBy(() -> inventoryService.decreaseStock(command))
                    .isInstanceOf(InsufficientStockException.class)
                    .hasMessageContaining("在庫が不足しています");
        }
    }

    @Nested
    @DisplayName("在庫状態の変更")
    class StockStatusChange {

        @Test
        @DisplayName("未検査から合格への状態変更ができる")
        void canChangeStatusFromUninspectedToPassed() {
            // Arrange
            stockRepository.save(Stock.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .stockQuantity(new BigDecimal("100"))
                    .passedQuantity(new BigDecimal("90"))
                    .defectiveQuantity(new BigDecimal("5"))
                    .uninspectedQuantity(new BigDecimal("5"))
                    .build());

            StockStatusChangeCommand command = StockStatusChangeCommand.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .quantity(new BigDecimal("3"))
                    .fromStatus(StockStatus.UNINSPECTED)
                    .toStatus(StockStatus.PASSED)
                    .build();

            // Act
            inventoryService.changeStockStatus(command);

            // Assert
            Stock stock = inventoryService.getStock("WH001", "MAT-001");
            assertThat(stock.getStockQuantity()).isEqualByComparingTo(new BigDecimal("100")); // 総数は変わらない
            assertThat(stock.getPassedQuantity()).isEqualByComparingTo(new BigDecimal("93")); // 90 + 3
            assertThat(stock.getUninspectedQuantity()).isEqualByComparingTo(new BigDecimal("2")); // 5 - 3
        }

        @Test
        @DisplayName("未検査から不良への状態変更ができる")
        void canChangeStatusFromUninspectedToDefective() {
            // Arrange
            stockRepository.save(Stock.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .stockQuantity(new BigDecimal("100"))
                    .passedQuantity(new BigDecimal("90"))
                    .defectiveQuantity(new BigDecimal("5"))
                    .uninspectedQuantity(new BigDecimal("5"))
                    .build());

            StockStatusChangeCommand command = StockStatusChangeCommand.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .quantity(new BigDecimal("2"))
                    .fromStatus(StockStatus.UNINSPECTED)
                    .toStatus(StockStatus.DEFECTIVE)
                    .build();

            // Act
            inventoryService.changeStockStatus(command);

            // Assert
            Stock stock = inventoryService.getStock("WH001", "MAT-001");
            assertThat(stock.getStockQuantity()).isEqualByComparingTo(new BigDecimal("100")); // 総数は変わらない
            assertThat(stock.getDefectiveQuantity()).isEqualByComparingTo(new BigDecimal("7")); // 5 + 2
            assertThat(stock.getUninspectedQuantity()).isEqualByComparingTo(new BigDecimal("3")); // 5 - 2
        }

        @Test
        @DisplayName("元の状態の数量以上を変更しようとするとエラー")
        void throwsExceptionWhenChangingMoreThanFromStatus() {
            // Arrange
            stockRepository.save(Stock.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .stockQuantity(new BigDecimal("100"))
                    .passedQuantity(new BigDecimal("90"))
                    .defectiveQuantity(new BigDecimal("5"))
                    .uninspectedQuantity(new BigDecimal("5"))
                    .build());

            StockStatusChangeCommand command = StockStatusChangeCommand.builder()
                    .locationCode("WH001")
                    .itemCode("MAT-001")
                    .quantity(new BigDecimal("10"))
                    .fromStatus(StockStatus.UNINSPECTED)
                    .toStatus(StockStatus.PASSED)
                    .build();

            // Act & Assert
            assertThatThrownBy(() -> inventoryService.changeStockStatus(command))
                    .isInstanceOf(InsufficientStockException.class)
                    .hasMessageContaining("未検査の在庫が不足しています");
        }
    }
}
