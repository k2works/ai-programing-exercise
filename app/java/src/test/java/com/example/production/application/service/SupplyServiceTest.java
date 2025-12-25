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

@DisplayName("支給業務サービス")
class SupplyServiceTest extends BaseIntegrationTest {

    @Autowired
    private SupplyService supplyService;

    @Autowired
    private SupplyRepository supplyRepository;

    @Autowired
    private SupplyDetailRepository supplyDetailRepository;

    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;

    @Autowired
    private PurchaseOrderDetailRepository purchaseOrderDetailRepository;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private SupplierRepository supplierRepository;

    private PurchaseOrder testPurchaseOrder;

    @BeforeEach
    void setUp() {
        supplyDetailRepository.deleteAll();
        supplyRepository.deleteAll();
        purchaseOrderDetailRepository.deleteAll();
        purchaseOrderRepository.deleteAll();
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

        // テスト用発注を作成
        var purchaseOrder = PurchaseOrder.builder()
                .purchaseOrderNumber("PO-2025-001")
                .orderDate(LocalDate.of(2025, 1, 15))
                .supplierCode("SUB-001")
                .status(PurchaseOrderStatus.ORDERED)
                .build();
        purchaseOrderRepository.save(purchaseOrder);
        testPurchaseOrder = purchaseOrder;

        var purchaseOrderDetail = PurchaseOrderDetail.builder()
                .purchaseOrderNumber("PO-2025-001")
                .lineNumber(1)
                .itemCode("PLATED-001")
                .orderQuantity(new BigDecimal("100"))
                .orderUnitPrice(new BigDecimal("500"))
                .orderAmount(new BigDecimal("50000"))
                .expectedReceivingDate(LocalDate.of(2025, 1, 25))
                .build();
        purchaseOrderDetailRepository.save(purchaseOrderDetail);
    }

    @Nested
    @DisplayName("支給データ作成")
    class SupplyCreation {

        @Test
        @DisplayName("発注に紐づく支給データを作成できる")
        void canCreateSupplyFromPurchaseOrder() {
            // Arrange
            SupplyCreateInput input = SupplyCreateInput.builder()
                    .purchaseOrderNumber(testPurchaseOrder.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 16))
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

            // Act
            Supply supply = supplyService.createSupply(input);

            // Assert
            assertThat(supply).isNotNull();
            assertThat(supply.getSupplyNumber()).startsWith("SUP-");
            assertThat(supply.getSupplierCode()).isEqualTo("SUB-001");
            assertThat(supply.getSupplyType()).isEqualTo(SupplyType.FREE);
            assertThat(supply.getDetails()).hasSize(1);
            assertThat(supply.getDetails().get(0).getQuantity())
                    .isEqualByComparingTo(new BigDecimal("100"));
        }

        @Test
        @DisplayName("有償支給を作成できる")
        void canCreatePaidSupply() {
            // Arrange
            SupplyCreateInput input = SupplyCreateInput.builder()
                    .purchaseOrderNumber(testPurchaseOrder.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 16))
                    .supplierPersonCode("EMP001")
                    .supplyType(SupplyType.PAID)
                    .details(List.of(
                            SupplyDetailInput.builder()
                                    .itemCode("PRESS-001")
                                    .quantity(new BigDecimal("100"))
                                    .unitPrice(new BigDecimal("200"))
                                    .build()
                    ))
                    .build();

            // Act
            Supply supply = supplyService.createSupply(input);

            // Assert
            assertThat(supply.getSupplyType()).isEqualTo(SupplyType.PAID);
            assertThat(supply.getDetails().get(0).getAmount())
                    .isEqualByComparingTo(new BigDecimal("20000"));
        }

        @Test
        @DisplayName("複数明細の支給を作成できる")
        void canCreateSupplyWithMultipleDetails() {
            // Arrange: 追加の品目を登録
            var supplyItem2 = Item.builder()
                    .itemCode("PRESS-002")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("プレス部品2")
                    .itemCategory(ItemCategory.PART)
                    .build();
            itemRepository.save(supplyItem2);

            SupplyCreateInput input = SupplyCreateInput.builder()
                    .purchaseOrderNumber(testPurchaseOrder.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 16))
                    .supplierPersonCode("EMP001")
                    .supplyType(SupplyType.FREE)
                    .details(List.of(
                            SupplyDetailInput.builder()
                                    .itemCode("PRESS-001")
                                    .quantity(new BigDecimal("50"))
                                    .unitPrice(new BigDecimal("200"))
                                    .build(),
                            SupplyDetailInput.builder()
                                    .itemCode("PRESS-002")
                                    .quantity(new BigDecimal("30"))
                                    .unitPrice(new BigDecimal("150"))
                                    .build()
                    ))
                    .build();

            // Act
            Supply supply = supplyService.createSupply(input);

            // Assert
            assertThat(supply.getDetails()).hasSize(2);
            assertThat(supply.getDetails().get(0).getLineNumber()).isEqualTo(1);
            assertThat(supply.getDetails().get(1).getLineNumber()).isEqualTo(2);
        }

        @Test
        @DisplayName("支給区分を指定しない場合は無償支給になる")
        void defaultsToFreeSupplyType() {
            // Arrange
            SupplyCreateInput input = SupplyCreateInput.builder()
                    .purchaseOrderNumber(testPurchaseOrder.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 16))
                    .supplierPersonCode("EMP001")
                    .supplyType(null)  // 明示的にnull
                    .details(List.of(
                            SupplyDetailInput.builder()
                                    .itemCode("PRESS-001")
                                    .quantity(new BigDecimal("100"))
                                    .unitPrice(new BigDecimal("200"))
                                    .build()
                    ))
                    .build();

            // Act
            Supply supply = supplyService.createSupply(input);

            // Assert
            assertThat(supply.getSupplyType()).isEqualTo(SupplyType.FREE);
        }
    }

    @Nested
    @DisplayName("支給データ検索")
    class SupplySearch {

        @Test
        @DisplayName("支給番号で支給データを検索できる")
        void canFindBySupplyNumber() {
            // Arrange: 支給データを作成
            SupplyCreateInput input = SupplyCreateInput.builder()
                    .purchaseOrderNumber(testPurchaseOrder.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 16))
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
            Supply created = supplyService.createSupply(input);

            // Act
            var found = supplyService.findBySupplyNumber(created.getSupplyNumber());

            // Assert
            assertThat(found).isPresent();
            assertThat(found.get().getSupplyNumber()).isEqualTo(created.getSupplyNumber());
            assertThat(found.get().getDetails()).hasSize(1);
        }

        @Test
        @DisplayName("発注明細に紐づく支給データを検索できる")
        void canFindByPurchaseOrderDetail() {
            // Arrange: 支給データを2件作成
            SupplyCreateInput input1 = SupplyCreateInput.builder()
                    .purchaseOrderNumber(testPurchaseOrder.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 16))
                    .supplierPersonCode("EMP001")
                    .supplyType(SupplyType.FREE)
                    .details(List.of(
                            SupplyDetailInput.builder()
                                    .itemCode("PRESS-001")
                                    .quantity(new BigDecimal("50"))
                                    .unitPrice(new BigDecimal("200"))
                                    .build()
                    ))
                    .build();
            supplyService.createSupply(input1);

            SupplyCreateInput input2 = SupplyCreateInput.builder()
                    .purchaseOrderNumber(testPurchaseOrder.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 17))
                    .supplierPersonCode("EMP001")
                    .supplyType(SupplyType.FREE)
                    .details(List.of(
                            SupplyDetailInput.builder()
                                    .itemCode("PRESS-001")
                                    .quantity(new BigDecimal("50"))
                                    .unitPrice(new BigDecimal("200"))
                                    .build()
                    ))
                    .build();
            supplyService.createSupply(input2);

            // Act
            var supplies = supplyService.findByPurchaseOrderDetail(
                    testPurchaseOrder.getPurchaseOrderNumber(), 1);

            // Assert
            assertThat(supplies).hasSize(2);
        }

        @Test
        @DisplayName("存在しない支給番号の場合は空を返す")
        void returnsEmptyForNonExistentSupplyNumber() {
            // Act
            var found = supplyService.findBySupplyNumber("NON-EXISTENT");

            // Assert
            assertThat(found).isEmpty();
        }
    }

    @Nested
    @DisplayName("支給番号採番")
    class SupplyNumberGeneration {

        @Test
        @DisplayName("支給番号は日付ベースで連番になる")
        void generatesSequentialSupplyNumbers() {
            // Arrange & Act: 同じ日に2件作成
            SupplyCreateInput input1 = SupplyCreateInput.builder()
                    .purchaseOrderNumber(testPurchaseOrder.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 16))
                    .supplierPersonCode("EMP001")
                    .supplyType(SupplyType.FREE)
                    .details(List.of(
                            SupplyDetailInput.builder()
                                    .itemCode("PRESS-001")
                                    .quantity(new BigDecimal("50"))
                                    .unitPrice(new BigDecimal("200"))
                                    .build()
                    ))
                    .build();
            Supply supply1 = supplyService.createSupply(input1);

            SupplyCreateInput input2 = SupplyCreateInput.builder()
                    .purchaseOrderNumber(testPurchaseOrder.getPurchaseOrderNumber())
                    .lineNumber(1)
                    .supplierCode("SUB-001")
                    .supplyDate(LocalDate.of(2025, 1, 16))
                    .supplierPersonCode("EMP001")
                    .supplyType(SupplyType.FREE)
                    .details(List.of(
                            SupplyDetailInput.builder()
                                    .itemCode("PRESS-001")
                                    .quantity(new BigDecimal("50"))
                                    .unitPrice(new BigDecimal("200"))
                                    .build()
                    ))
                    .build();
            Supply supply2 = supplyService.createSupply(input2);

            // Assert
            assertThat(supply1.getSupplyNumber()).isEqualTo("SUP-202501-0001");
            assertThat(supply2.getSupplyNumber()).isEqualTo("SUP-202501-0002");
        }
    }
}
