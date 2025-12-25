package com.example.production.application.service;

import com.example.production.application.port.out.*;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import com.example.production.domain.model.purchase.*;
import com.example.production.domain.model.subcontract.*;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("外注委託ワークフローサービス")
class SubcontractingWorkflowServiceTest extends BaseIntegrationTest {

    @Autowired
    private SubcontractingWorkflowService workflowService;

    @Autowired
    private SupplyService supplyService;

    @Autowired
    private ConsumptionService consumptionService;

    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;

    @Autowired
    private PurchaseOrderDetailRepository purchaseOrderDetailRepository;

    @Autowired
    private SupplyRepository supplyRepository;

    @Autowired
    private SupplyDetailRepository supplyDetailRepository;

    @Autowired
    private ConsumptionRepository consumptionRepository;

    @Autowired
    private ConsumptionDetailRepository consumptionDetailRepository;

    @Autowired
    private ReceivingRepository receivingRepository;

    @Autowired
    private AcceptanceRepository acceptanceRepository;

    @Autowired
    private InspectionRepository inspectionRepository;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private SupplierRepository supplierRepository;

    @Autowired
    private UnitPriceRepository unitPriceRepository;

    @BeforeEach
    void setUp() {
        consumptionDetailRepository.deleteAll();
        consumptionRepository.deleteAll();
        supplyDetailRepository.deleteAll();
        supplyRepository.deleteAll();
        acceptanceRepository.deleteAll();
        inspectionRepository.deleteAll();
        receivingRepository.deleteAll();
        purchaseOrderDetailRepository.deleteAll();
        purchaseOrderRepository.deleteAll();
        unitPriceRepository.deleteAll();
        supplierRepository.deleteAll();
        itemRepository.deleteAll();

        // マスタデータの準備
        var supplier = Supplier.builder()
                .supplierCode("SUB-001")
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .supplierName("株式会社メッキ工業")
                .supplierType(SupplierType.SUBCONTRACTOR)
                .build();
        supplierRepository.save(supplier);

        var parentItem = Item.builder()
                .itemCode("PLATED-001")
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .itemName("メッキ加工品")
                .itemCategory(ItemCategory.SEMI_PRODUCT)
                .build();
        itemRepository.save(parentItem);

        var supplyItem = Item.builder()
                .itemCode("PRESS-001")
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .itemName("プレス部品")
                .itemCategory(ItemCategory.PART)
                .build();
        itemRepository.save(supplyItem);

        // 単価マスタの登録
        var unitPrice = UnitPrice.builder()
                .itemCode("PLATED-001")
                .supplierCode("SUB-001")
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .unitPrice(new BigDecimal("500"))
                .build();
        unitPriceRepository.save(unitPrice);
    }

    @Nested
    @DisplayName("外注発注作成")
    class CreateSubcontractOrder {

        @Test
        @DisplayName("外注発注を作成できる")
        void canCreateSubcontractOrder() {
            // Arrange
            SubcontractOrderInput input = SubcontractOrderInput.builder()
                    .supplierCode("SUB-001")
                    .deliveryDate(LocalDate.of(2025, 2, 15))
                    .itemCode("PLATED-001")
                    .quantity(new BigDecimal("100"))
                    .build();

            // Act
            PurchaseOrder result = workflowService.createSubcontractOrder(input);

            // Assert
            assertThat(result).isNotNull();
            assertThat(result.getPurchaseOrderNumber()).startsWith("PO-");
            assertThat(result.getStatus()).isEqualTo(PurchaseOrderStatus.ORDERED);
            assertThat(result.getSupplierCode()).isEqualTo("SUB-001");
        }

        @Test
        @DisplayName("単価マスタから単価を取得して発注金額を計算する")
        void calculatesOrderAmountFromUnitPrice() {
            // Arrange
            SubcontractOrderInput input = SubcontractOrderInput.builder()
                    .supplierCode("SUB-001")
                    .deliveryDate(LocalDate.of(2025, 2, 15))
                    .itemCode("PLATED-001")
                    .quantity(new BigDecimal("100"))
                    .build();

            // Act
            PurchaseOrder result = workflowService.createSubcontractOrder(input);

            // Assert
            var details = purchaseOrderDetailRepository.findByPurchaseOrderNumber(result.getPurchaseOrderNumber());
            assertThat(details).hasSize(1);
            assertThat(details.get(0).getOrderUnitPrice()).isEqualByComparingTo(new BigDecimal("500"));
            assertThat(details.get(0).getOrderAmount()).isEqualByComparingTo(new BigDecimal("50000"));
        }

        @Test
        @DisplayName("指定した単価で発注を作成できる")
        void canCreateWithSpecifiedUnitPrice() {
            // Arrange
            SubcontractOrderInput input = SubcontractOrderInput.builder()
                    .supplierCode("SUB-001")
                    .deliveryDate(LocalDate.of(2025, 2, 15))
                    .itemCode("PLATED-001")
                    .quantity(new BigDecimal("100"))
                    .unitPrice(new BigDecimal("600"))
                    .build();

            // Act
            PurchaseOrder result = workflowService.createSubcontractOrder(input);

            // Assert
            var details = purchaseOrderDetailRepository.findByPurchaseOrderNumber(result.getPurchaseOrderNumber());
            assertThat(details.get(0).getOrderUnitPrice()).isEqualByComparingTo(new BigDecimal("600"));
            assertThat(details.get(0).getOrderAmount()).isEqualByComparingTo(new BigDecimal("60000"));
        }
    }

    @Nested
    @DisplayName("外注委託状況取得")
    class GetSubcontractStatus {

        @Test
        @DisplayName("発注直後の状況を取得できる")
        void canGetStatusAfterOrdering() {
            // Arrange
            SubcontractOrderInput input = SubcontractOrderInput.builder()
                    .supplierCode("SUB-001")
                    .deliveryDate(LocalDate.of(2025, 2, 15))
                    .itemCode("PLATED-001")
                    .quantity(new BigDecimal("100"))
                    .build();
            PurchaseOrder order = workflowService.createSubcontractOrder(input);

            // Act
            SubcontractStatus status = workflowService.getSubcontractStatus(order.getPurchaseOrderNumber());

            // Assert
            assertThat(status.getPurchaseOrderNumber()).isEqualTo(order.getPurchaseOrderNumber());
            assertThat(status.getStatus()).isEqualTo(PurchaseOrderStatus.ORDERED);
            assertThat(status.getSuppliedQuantity()).isEqualByComparingTo(BigDecimal.ZERO);
            assertThat(status.getConsumedQuantity()).isEqualByComparingTo(BigDecimal.ZERO);
            assertThat(status.getAcceptedQuantity()).isEqualByComparingTo(BigDecimal.ZERO);
            assertThat(status.getYieldRate()).isEqualByComparingTo(BigDecimal.ZERO);
        }

        @Test
        @DisplayName("支給後の状況を取得できる")
        void canGetStatusAfterSupply() {
            // Arrange
            SubcontractOrderInput orderInput = SubcontractOrderInput.builder()
                    .supplierCode("SUB-001")
                    .deliveryDate(LocalDate.of(2025, 2, 15))
                    .itemCode("PLATED-001")
                    .quantity(new BigDecimal("100"))
                    .build();
            PurchaseOrder order = workflowService.createSubcontractOrder(orderInput);

            // 支給を実行
            SupplyCreateInput supplyInput = SupplyCreateInput.builder()
                    .purchaseOrderNumber(order.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 20))
                    .supplierPersonCode("EMP001")
                    .supplyType(SupplyType.FREE)
                    .details(List.of(
                            SupplyDetailInput.builder()
                                    .itemCode("PRESS-001")
                                    .quantity(new BigDecimal("100"))
                                    .unitPrice(new BigDecimal("200"))
                                    .build()
                    ))
                    .build();
            supplyService.createSupply(supplyInput);

            // Act
            SubcontractStatus status = workflowService.getSubcontractStatus(order.getPurchaseOrderNumber());

            // Assert
            assertThat(status.getSuppliedQuantity()).isEqualByComparingTo(new BigDecimal("100"));
            assertThat(status.getConsumedQuantity()).isEqualByComparingTo(BigDecimal.ZERO);
            assertThat(status.getYieldRate()).isEqualByComparingTo(BigDecimal.ZERO);
        }

        @Test
        @DisplayName("消費後の歩留り率を計算できる")
        void canCalculateYieldRateAfterConsumption() {
            // Arrange
            SubcontractOrderInput orderInput = SubcontractOrderInput.builder()
                    .supplierCode("SUB-001")
                    .deliveryDate(LocalDate.of(2025, 2, 15))
                    .itemCode("PLATED-001")
                    .quantity(new BigDecimal("100"))
                    .build();
            PurchaseOrder order = workflowService.createSubcontractOrder(orderInput);

            // 支給を実行
            SupplyCreateInput supplyInput = SupplyCreateInput.builder()
                    .purchaseOrderNumber(order.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 20))
                    .supplierPersonCode("EMP001")
                    .supplyType(SupplyType.FREE)
                    .details(List.of(
                            SupplyDetailInput.builder()
                                    .itemCode("PRESS-001")
                                    .quantity(new BigDecimal("100"))
                                    .unitPrice(new BigDecimal("200"))
                                    .build()
                    ))
                    .build();
            supplyService.createSupply(supplyInput);

            // 入荷を登録
            var receiving = Receiving.builder()
                    .receivingNumber("RCV-TEST-001")
                    .purchaseOrderNumber(order.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .receivingDate(LocalDate.of(2025, 2, 10))
                    .receivingType(ReceivingType.NORMAL)
                    .itemCode("PLATED-001")
                    .receivingQuantity(new BigDecimal("100"))
                    .miscellaneousItemFlag(false)
                    .build();
            receivingRepository.save(receiving);

            // 消費を記録
            ConsumptionCreateInput consumptionInput = ConsumptionCreateInput.builder()
                    .receivingNumber("RCV-TEST-001")
                    .consumptionDate(LocalDate.of(2025, 2, 10))
                    .supplierCode("SUB-001")
                    .details(List.of(
                            ConsumptionDetailInput.builder()
                                    .itemCode("PRESS-001")
                                    .quantity(new BigDecimal("95"))
                                    .build()
                    ))
                    .build();
            consumptionService.createConsumption(consumptionInput);

            // Act
            SubcontractStatus status = workflowService.getSubcontractStatus(order.getPurchaseOrderNumber());

            // Assert: 95/100 = 0.95
            assertThat(status.getSuppliedQuantity()).isEqualByComparingTo(new BigDecimal("100"));
            assertThat(status.getConsumedQuantity()).isEqualByComparingTo(new BigDecimal("95"));
            assertThat(status.getYieldRate()).isEqualByComparingTo(new BigDecimal("0.95"));
        }

        @Test
        @DisplayName("存在しない発注番号でエラーになる")
        void throwsErrorForNonExistentOrder() {
            // Act & Assert
            assertThatThrownBy(() -> workflowService.getSubcontractStatus("NON-EXISTENT"))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Purchase order not found");
        }
    }
}
