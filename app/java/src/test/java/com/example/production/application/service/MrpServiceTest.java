package com.example.production.application.service;

import com.example.production.application.port.out.*;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.plan.Allocation;
import com.example.production.domain.model.plan.AllocationType;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.domain.model.plan.Requirement;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.*;

@DisplayName("所要量展開（MRP）")
class MrpServiceTest extends BaseIntegrationTest {

    @Autowired
    private MrpService mrpService;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private BomRepository bomRepository;

    @Autowired
    private OrderRepository orderRepository;

    @Autowired
    private RequirementRepository requirementRepository;

    @Autowired
    private AllocationRepository allocationRepository;

    @BeforeEach
    void setUp() {
        allocationRepository.deleteAll();
        requirementRepository.deleteAll();
        orderRepository.deleteAll();
        bomRepository.deleteAll();
        itemRepository.deleteAll();
    }

    @Nested
    @DisplayName("単純な所要量計算")
    class SimpleRequirementCalculation {

        @Test
        @DisplayName("製品オーダから子部品の所要量を計算できる")
        void canCalculateRequirementsFromProductOrder() {
            // Arrange: 製品と部品を登録
            var product = Item.builder()
                    .itemCode("PROD-001")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("製品A")
                    .itemCategory(ItemCategory.PRODUCT)
                    .leadTime(5)
                    .build();
            itemRepository.save(product);

            var part = Item.builder()
                    .itemCode("PART-001")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("部品B")
                    .itemCategory(ItemCategory.PART)
                    .leadTime(3)
                    .build();
            itemRepository.save(part);

            // BOM登録（製品1個に部品2個必要）
            var bom = Bom.builder()
                    .parentItemCode("PROD-001")
                    .childItemCode("PART-001")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .baseQuantity(BigDecimal.ONE)
                    .requiredQuantity(new BigDecimal("2"))
                    .defectRate(BigDecimal.ZERO)
                    .build();
            bomRepository.save(bom);

            // 製品オーダを作成
            var productOrder = Order.builder()
                    .orderNumber("MO-2025-001")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("PROD-001")
                    .startDate(LocalDate.of(2025, 1, 15))
                    .dueDate(LocalDate.of(2025, 1, 20))
                    .planQuantity(new BigDecimal("100"))
                    .locationCode("WH-001")
                    .status(PlanStatus.CONFIRMED)
                    .build();
            orderRepository.save(productOrder);

            // Act: MRP実行
            var requirements = mrpService.explodeRequirements(productOrder.getId());

            // Assert: 部品の所要量が200個（100 × 2）になる
            assertThat(requirements).hasSize(1);
            assertThat(requirements.get(0).getItemCode()).isEqualTo("PART-001");
            assertThat(requirements.get(0).getRequiredQuantity())
                    .isEqualByComparingTo(new BigDecimal("200"));
        }

        @Test
        @DisplayName("不良率を考慮して所要量を計算できる")
        void canCalculateRequirementsWithDefectRate() {
            // Arrange
            var product = Item.builder()
                    .itemCode("PROD-002")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("製品B")
                    .itemCategory(ItemCategory.PRODUCT)
                    .leadTime(5)
                    .build();
            itemRepository.save(product);

            var part = Item.builder()
                    .itemCode("PART-002")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("部品C")
                    .itemCategory(ItemCategory.PART)
                    .leadTime(3)
                    .build();
            itemRepository.save(part);

            // 不良率5%のBOM
            var bom = Bom.builder()
                    .parentItemCode("PROD-002")
                    .childItemCode("PART-002")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .baseQuantity(BigDecimal.ONE)
                    .requiredQuantity(new BigDecimal("1"))
                    .defectRate(new BigDecimal("5"))
                    .build();
            bomRepository.save(bom);

            var productOrder = Order.builder()
                    .orderNumber("MO-2025-002")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("PROD-002")
                    .startDate(LocalDate.of(2025, 1, 15))
                    .dueDate(LocalDate.of(2025, 1, 20))
                    .planQuantity(new BigDecimal("100"))
                    .locationCode("WH-001")
                    .status(PlanStatus.CONFIRMED)
                    .build();
            orderRepository.save(productOrder);

            // Act
            var requirements = mrpService.explodeRequirements(productOrder.getId());

            // Assert: 100 × 1 × 1.05 = 105
            assertThat(requirements).hasSize(1);
            assertThat(requirements.get(0).getRequiredQuantity())
                    .isEqualByComparingTo(new BigDecimal("105"));
        }
    }

    @Nested
    @DisplayName("在庫引当")
    class InventoryAllocation {

        @Test
        @DisplayName("在庫がある場合は引当される")
        void allocatesFromInventory() {
            // Arrange
            var part = Item.builder()
                    .itemCode("PART-003")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("部品D")
                    .itemCategory(ItemCategory.PART)
                    .build();
            itemRepository.save(part);

            var order = Order.builder()
                    .orderNumber("MO-2025-003")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("PART-003")
                    .startDate(LocalDate.of(2025, 1, 10))
                    .dueDate(LocalDate.of(2025, 1, 15))
                    .planQuantity(new BigDecimal("100"))
                    .locationCode("WH-001")
                    .status(PlanStatus.CONFIRMED)
                    .build();
            orderRepository.save(order);

            int currentInventory = 50;

            var requirement = Requirement.builder()
                    .requirementNumber("REQ-2025-001")
                    .orderId(order.getId())
                    .itemCode("PART-003")
                    .dueDate(LocalDate.of(2025, 1, 15))
                    .requiredQuantity(new BigDecimal("100"))
                    .allocatedQuantity(BigDecimal.ZERO)
                    .shortageQuantity(new BigDecimal("100"))
                    .locationCode("WH-001")
                    .build();
            requirementRepository.save(requirement);

            // Act: 在庫引当を実行
            var allocation = mrpService.allocateFromInventory(
                    requirement.getId(), currentInventory);

            // Assert: 50個が引当され、50個が不足
            assertThat(allocation.getAllocatedQuantity())
                    .isEqualByComparingTo(new BigDecimal("50"));

            var updated = requirementRepository.findById(requirement.getId());
            assertThat(updated).isPresent();
            assertThat(updated.get().getAllocatedQuantity())
                    .isEqualByComparingTo(new BigDecimal("50"));
            assertThat(updated.get().getShortageQuantity())
                    .isEqualByComparingTo(new BigDecimal("50"));
        }

        @Test
        @DisplayName("在庫が所要量を上回る場合は全量引当される")
        void allocatesFullQuantityWhenInventorySufficient() {
            // Arrange
            var part = Item.builder()
                    .itemCode("PART-004")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("部品E")
                    .itemCategory(ItemCategory.PART)
                    .build();
            itemRepository.save(part);

            var order = Order.builder()
                    .orderNumber("MO-2025-004")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("PART-004")
                    .startDate(LocalDate.of(2025, 1, 10))
                    .dueDate(LocalDate.of(2025, 1, 15))
                    .planQuantity(new BigDecimal("100"))
                    .locationCode("WH-001")
                    .status(PlanStatus.CONFIRMED)
                    .build();
            orderRepository.save(order);

            var requirement = Requirement.builder()
                    .requirementNumber("REQ-2025-002")
                    .orderId(order.getId())
                    .itemCode("PART-004")
                    .dueDate(LocalDate.of(2025, 1, 15))
                    .requiredQuantity(new BigDecimal("100"))
                    .allocatedQuantity(BigDecimal.ZERO)
                    .shortageQuantity(new BigDecimal("100"))
                    .locationCode("WH-001")
                    .build();
            requirementRepository.save(requirement);

            // Act
            var allocation = mrpService.allocateFromInventory(requirement.getId(), 200);

            // Assert
            assertThat(allocation.getAllocatedQuantity())
                    .isEqualByComparingTo(new BigDecimal("100"));

            var updated = requirementRepository.findById(requirement.getId());
            assertThat(updated).isPresent();
            assertThat(updated.get().getShortageQuantity())
                    .isEqualByComparingTo(BigDecimal.ZERO);
        }
    }

    @Nested
    @DisplayName("ロットサイズ計算")
    class LotSizeCalculation {

        @Test
        @DisplayName("最小ロットサイズを考慮してオーダ数量を計算する")
        void calculatesWithMinimumLotSize() {
            // Act: 必要数量75個に対するオーダ数量を計算
            BigDecimal orderQuantity = mrpService.calculateOrderQuantity(
                    new BigDecimal("75"),
                    new BigDecimal("100"),  // 最小ロット数
                    new BigDecimal("50"),   // 刻みロット数
                    null                     // 最大ロット数
            );

            // Assert: 最小ロット100個になる
            assertThat(orderQuantity).isEqualByComparingTo(new BigDecimal("100"));
        }

        @Test
        @DisplayName("刻みロットサイズを考慮してオーダ数量を計算する")
        void calculatesWithIncrementLotSize() {
            // Act: 必要数量130個に対するオーダ数量を計算
            BigDecimal orderQuantity = mrpService.calculateOrderQuantity(
                    new BigDecimal("130"),
                    new BigDecimal("100"),  // 最小ロット数
                    new BigDecimal("50"),   // 刻みロット数
                    null                     // 最大ロット数
            );

            // Assert: 100 + 50 = 150個になる
            assertThat(orderQuantity).isEqualByComparingTo(new BigDecimal("150"));
        }

        @Test
        @DisplayName("最大ロットサイズを超えないようにする")
        void respectsMaximumLotSize() {
            // Act: 必要数量300個に対するオーダ数量を計算
            BigDecimal orderQuantity = mrpService.calculateOrderQuantity(
                    new BigDecimal("300"),
                    new BigDecimal("100"),  // 最小ロット数
                    new BigDecimal("50"),   // 刻みロット数
                    new BigDecimal("250")   // 最大ロット数
            );

            // Assert: 最大250個になる
            assertThat(orderQuantity).isEqualByComparingTo(new BigDecimal("250"));
        }

        @Test
        @DisplayName("デフォルト値で計算できる")
        void calculatesWithDefaultValues() {
            BigDecimal orderQuantity = mrpService.calculateOrderQuantity(
                    new BigDecimal("75"),
                    null,  // 最小ロット数（デフォルト1）
                    null,  // 刻みロット数（デフォルト1）
                    null   // 最大ロット数（なし）
            );

            assertThat(orderQuantity).isEqualByComparingTo(new BigDecimal("75"));
        }
    }

    @Nested
    @DisplayName("リードタイム計算")
    class LeadTimeCalculation {

        @Test
        @DisplayName("納期からリードタイムを逆算して着手日を計算する")
        void calculatesStartDateFromDueDate() {
            // Arrange
            LocalDate dueDate = LocalDate.of(2025, 1, 20);
            int leadTime = 5;

            // Act
            LocalDate startDate = mrpService.calculateStartDate(dueDate, leadTime, 0);

            // Assert: 1/20 - 5日 = 1/15
            assertThat(startDate).isEqualTo(LocalDate.of(2025, 1, 15));
        }

        @Test
        @DisplayName("安全リードタイムも考慮して着手日を計算する")
        void calculatesStartDateWithSafetyLeadTime() {
            // Arrange
            LocalDate dueDate = LocalDate.of(2025, 1, 20);
            int leadTime = 5;
            int safetyLeadTime = 2;

            // Act
            LocalDate startDate = mrpService.calculateStartDate(dueDate, leadTime, safetyLeadTime);

            // Assert: 1/20 - 5日 - 2日 = 1/13
            assertThat(startDate).isEqualTo(LocalDate.of(2025, 1, 13));
        }
    }

    @Nested
    @DisplayName("新規オーダ生成")
    class NewOrderGeneration {

        @Test
        @DisplayName("不足分に対して新規オーダを生成できる")
        void canGenerateNewOrderForShortage() {
            // Arrange
            var part = Item.builder()
                    .itemCode("PART-005")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("部品F")
                    .itemCategory(ItemCategory.PART)
                    .leadTime(3)
                    .minLotSize(new BigDecimal("100"))
                    .build();
            itemRepository.save(part);

            var shortageQuantity = new BigDecimal("75");
            var dueDate = LocalDate.of(2025, 1, 20);

            // Act
            var newOrder = mrpService.createShortageOrder(
                    "PART-005", shortageQuantity, dueDate, "WH-001", OrderType.PURCHASE
            );

            // Assert
            assertThat(newOrder).isNotNull();
            assertThat(newOrder.getItemCode()).isEqualTo("PART-005");
            assertThat(newOrder.getPlanQuantity())
                    .isEqualByComparingTo(new BigDecimal("100")); // 最小ロット適用
            assertThat(newOrder.getStartDate()).isEqualTo(LocalDate.of(2025, 1, 17)); // 3日前
            assertThat(newOrder.getOrderType()).isEqualTo(OrderType.PURCHASE);
        }

        @Test
        @DisplayName("製造オーダを生成できる")
        void canGenerateManufacturingOrder() {
            // Arrange
            var semiProduct = Item.builder()
                    .itemCode("SEMI-001")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("半製品A")
                    .itemCategory(ItemCategory.SEMI_PRODUCT)
                    .leadTime(5)
                    .build();
            itemRepository.save(semiProduct);

            // Act
            var newOrder = mrpService.createShortageOrder(
                    "SEMI-001", new BigDecimal("50"), LocalDate.of(2025, 1, 25), "WH-001", OrderType.MANUFACTURING
            );

            // Assert
            assertThat(newOrder.getOrderType()).isEqualTo(OrderType.MANUFACTURING);
            assertThat(newOrder.getStartDate()).isEqualTo(LocalDate.of(2025, 1, 20)); // 5日前
        }
    }

    @Nested
    @DisplayName("歩留率と不良率を考慮した計算")
    class YieldAndDefectCalculation {

        @Test
        @DisplayName("歩留率と不良率を考慮した所要量を計算できる")
        void calculatesWithYieldAndDefectRate() {
            // 親数量100、基準数量1、必要数量2、不良率5%、歩留率90%
            BigDecimal result = mrpService.calculateRequiredQuantity(
                    new BigDecimal("100"),
                    BigDecimal.ONE,
                    new BigDecimal("2"),
                    new BigDecimal("5"),
                    new BigDecimal("90")
            );

            // 100 × 2 / 1 × 1.05 / 0.9 = 233.33... → 234（切り上げ）
            assertThat(result).isEqualByComparingTo(new BigDecimal("234"));
        }
    }
}
