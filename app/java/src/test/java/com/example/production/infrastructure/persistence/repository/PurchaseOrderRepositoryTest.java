package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.ItemRepository;
import com.example.production.application.port.out.PurchaseOrderDetailRepository;
import com.example.production.application.port.out.PurchaseOrderRepository;
import com.example.production.application.port.out.SupplierRepository;
import com.example.production.application.port.out.UnitPriceRepository;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderDetail;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import com.example.production.domain.model.purchase.UnitPrice;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("発注リポジトリ")
class PurchaseOrderRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;

    @Autowired
    private PurchaseOrderDetailRepository detailRepository;

    @Autowired
    private UnitPriceRepository unitPriceRepository;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private SupplierRepository supplierRepository;

    @BeforeEach
    void setUp() {
        detailRepository.deleteAll();
        purchaseOrderRepository.deleteAll();
        unitPriceRepository.deleteAll();
        supplierRepository.deleteAll();
        itemRepository.deleteAll();
    }

    private Item createItem(String itemCode) {
        var item = Item.builder()
                .itemCode(itemCode)
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .itemName("テスト品目")
                .itemCategory(ItemCategory.MATERIAL)
                .build();
        itemRepository.save(item);
        return item;
    }

    private Supplier createSupplier(String supplierCode) {
        var supplier = Supplier.builder()
                .supplierCode(supplierCode)
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .supplierName("テスト取引先")
                .supplierType(SupplierType.VENDOR)
                .build();
        supplierRepository.save(supplier);
        return supplier;
    }

    @Nested
    @DisplayName("単価マスタ")
    class UnitPriceTests {

        @Test
        @DisplayName("単価マスタを登録できる")
        void canRegisterUnitPrice() {
            // Arrange
            createItem("MAT-001");
            createSupplier("SUP-001");

            var unitPrice = UnitPrice.builder()
                    .itemCode("MAT-001")
                    .supplierCode("SUP-001")
                    .lotUnitQuantity(new BigDecimal("100"))
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .unitPrice(new BigDecimal("1500"))
                    .build();

            // Act
            unitPriceRepository.save(unitPrice);

            // Assert
            assertThat(unitPrice.getId()).isNotNull();
        }

        @Test
        @DisplayName("有効な単価を取得できる")
        void canFindEffectiveUnitPrice() {
            // Arrange
            createItem("MAT-002");
            createSupplier("SUP-002");

            var oldPrice = UnitPrice.builder()
                    .itemCode("MAT-002")
                    .supplierCode("SUP-002")
                    .effectiveFrom(LocalDate.of(2024, 1, 1))
                    .effectiveTo(LocalDate.of(2024, 12, 31))
                    .unitPrice(new BigDecimal("1000"))
                    .build();
            unitPriceRepository.save(oldPrice);

            var currentPrice = UnitPrice.builder()
                    .itemCode("MAT-002")
                    .supplierCode("SUP-002")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .unitPrice(new BigDecimal("1200"))
                    .build();
            unitPriceRepository.save(currentPrice);

            // Act
            var result = unitPriceRepository.findEffectiveUnitPrice(
                    "MAT-002", "SUP-002", LocalDate.of(2025, 3, 15));

            // Assert
            assertThat(result).isPresent();
            assertThat(result.get().getUnitPrice()).isEqualByComparingTo(new BigDecimal("1200"));
        }
    }

    @Nested
    @DisplayName("発注データ")
    class PurchaseOrderTests {

        @Test
        @DisplayName("発注を登録できる")
        void canRegisterPurchaseOrder() {
            // Arrange
            createSupplier("SUP-003");

            var po = PurchaseOrder.builder()
                    .purchaseOrderNumber("PO-2025-001")
                    .orderDate(LocalDate.of(2025, 3, 1))
                    .supplierCode("SUP-003")
                    .ordererCode("EMP-001")
                    .departmentCode("DEPT-001")
                    .status(PurchaseOrderStatus.CREATING)
                    .build();

            // Act
            purchaseOrderRepository.save(po);

            // Assert
            assertThat(po.getId()).isNotNull();

            var result = purchaseOrderRepository.findByPurchaseOrderNumber("PO-2025-001");
            assertThat(result).isPresent();
            assertThat(result.get().getSupplierCode()).isEqualTo("SUP-003");
            assertThat(result.get().getStatus()).isEqualTo(PurchaseOrderStatus.CREATING);
        }

        @Test
        @DisplayName("発注ステータスを更新できる")
        void canUpdatePurchaseOrderStatus() {
            // Arrange
            createSupplier("SUP-004");

            var po = PurchaseOrder.builder()
                    .purchaseOrderNumber("PO-2025-002")
                    .orderDate(LocalDate.of(2025, 3, 1))
                    .supplierCode("SUP-004")
                    .status(PurchaseOrderStatus.CREATING)
                    .build();
            purchaseOrderRepository.save(po);

            // Act
            purchaseOrderRepository.updateStatus("PO-2025-002", PurchaseOrderStatus.ORDERED);

            // Assert
            var result = purchaseOrderRepository.findByPurchaseOrderNumber("PO-2025-002");
            assertThat(result).isPresent();
            assertThat(result.get().getStatus()).isEqualTo(PurchaseOrderStatus.ORDERED);
        }

        @Test
        @DisplayName("全てのステータスを設定できる")
        void canSetAllStatuses() {
            createSupplier("SUP-STATUS");

            for (var status : PurchaseOrderStatus.values()) {
                var poNumber = "PO-S-" + status.ordinal();
                var po = PurchaseOrder.builder()
                        .purchaseOrderNumber(poNumber)
                        .orderDate(LocalDate.of(2025, 3, 1))
                        .supplierCode("SUP-STATUS")
                        .status(status)
                        .build();
                purchaseOrderRepository.save(po);

                var result = purchaseOrderRepository.findByPurchaseOrderNumber(poNumber);
                assertThat(result).isPresent();
                assertThat(result.get().getStatus()).isEqualTo(status);
            }
        }

        @Test
        @DisplayName("最新の発注番号を取得できる")
        void canFindLatestPurchaseOrderNumber() {
            // Arrange
            createSupplier("SUP-005");

            var po1 = PurchaseOrder.builder()
                    .purchaseOrderNumber("PO-2025-001")
                    .orderDate(LocalDate.of(2025, 3, 1))
                    .supplierCode("SUP-005")
                    .build();
            purchaseOrderRepository.save(po1);

            var po2 = PurchaseOrder.builder()
                    .purchaseOrderNumber("PO-2025-010")
                    .orderDate(LocalDate.of(2025, 3, 2))
                    .supplierCode("SUP-005")
                    .build();
            purchaseOrderRepository.save(po2);

            // Act
            var result = purchaseOrderRepository.findLatestPurchaseOrderNumber("PO-2025");

            // Assert
            assertThat(result).isPresent();
            assertThat(result.get()).isEqualTo("PO-2025-010");
        }
    }

    @Nested
    @DisplayName("発注明細")
    class PurchaseOrderDetailTests {

        @Test
        @DisplayName("発注明細を登録できる")
        void canRegisterPurchaseOrderDetail() {
            // Arrange
            createItem("MAT-003");
            createSupplier("SUP-006");

            var po = PurchaseOrder.builder()
                    .purchaseOrderNumber("PO-2025-003")
                    .orderDate(LocalDate.of(2025, 3, 1))
                    .supplierCode("SUP-006")
                    .build();
            purchaseOrderRepository.save(po);

            var detail = PurchaseOrderDetail.builder()
                    .purchaseOrderNumber("PO-2025-003")
                    .lineNumber(1)
                    .itemCode("MAT-003")
                    .expectedReceivingDate(LocalDate.of(2025, 3, 15))
                    .orderUnitPrice(new BigDecimal("1500"))
                    .orderQuantity(new BigDecimal("100"))
                    .orderAmount(new BigDecimal("150000"))
                    .build();

            // Act
            detailRepository.save(detail);

            // Assert
            assertThat(detail.getId()).isNotNull();

            var result = detailRepository.findById(detail.getId());
            assertThat(result).isPresent();
            assertThat(result.get().getItemCode()).isEqualTo("MAT-003");
            assertThat(result.get().getOrderQuantity()).isEqualByComparingTo(new BigDecimal("100"));
        }

        @Test
        @DisplayName("発注番号で明細を取得できる")
        void canFindDetailsByPurchaseOrderNumber() {
            // Arrange
            createItem("MAT-004");
            createItem("MAT-005");
            createSupplier("SUP-007");

            var po = PurchaseOrder.builder()
                    .purchaseOrderNumber("PO-2025-004")
                    .orderDate(LocalDate.of(2025, 3, 1))
                    .supplierCode("SUP-007")
                    .build();
            purchaseOrderRepository.save(po);

            var detail1 = PurchaseOrderDetail.builder()
                    .purchaseOrderNumber("PO-2025-004")
                    .lineNumber(1)
                    .itemCode("MAT-004")
                    .expectedReceivingDate(LocalDate.of(2025, 3, 15))
                    .orderUnitPrice(new BigDecimal("1000"))
                    .orderQuantity(new BigDecimal("50"))
                    .orderAmount(new BigDecimal("50000"))
                    .build();
            detailRepository.save(detail1);

            var detail2 = PurchaseOrderDetail.builder()
                    .purchaseOrderNumber("PO-2025-004")
                    .lineNumber(2)
                    .itemCode("MAT-005")
                    .expectedReceivingDate(LocalDate.of(2025, 3, 15))
                    .orderUnitPrice(new BigDecimal("2000"))
                    .orderQuantity(new BigDecimal("30"))
                    .orderAmount(new BigDecimal("60000"))
                    .build();
            detailRepository.save(detail2);

            // Act
            var result = detailRepository.findByPurchaseOrderNumber("PO-2025-004");

            // Assert
            assertThat(result).hasSize(2);
            assertThat(result.get(0).getLineNumber()).isEqualTo(1);
            assertThat(result.get(1).getLineNumber()).isEqualTo(2);
        }

        @Test
        @DisplayName("入荷済数量を更新できる")
        void canUpdateReceivedQuantity() {
            // Arrange
            createItem("MAT-006");
            createSupplier("SUP-008");

            var po = PurchaseOrder.builder()
                    .purchaseOrderNumber("PO-2025-005")
                    .orderDate(LocalDate.of(2025, 3, 1))
                    .supplierCode("SUP-008")
                    .build();
            purchaseOrderRepository.save(po);

            var detail = PurchaseOrderDetail.builder()
                    .purchaseOrderNumber("PO-2025-005")
                    .lineNumber(1)
                    .itemCode("MAT-006")
                    .expectedReceivingDate(LocalDate.of(2025, 3, 15))
                    .orderUnitPrice(new BigDecimal("1500"))
                    .orderQuantity(new BigDecimal("100"))
                    .orderAmount(new BigDecimal("150000"))
                    .build();
            detailRepository.save(detail);

            // Act
            detailRepository.updateReceivedQuantity(detail.getId(), new BigDecimal("50"));

            // Assert
            var result = detailRepository.findById(detail.getId());
            assertThat(result).isPresent();
            assertThat(result.get().getReceivedQuantity()).isEqualByComparingTo(new BigDecimal("50"));
        }

        @Test
        @DisplayName("検収済数量を更新できる")
        void canUpdateAcceptedQuantity() {
            // Arrange
            createItem("MAT-007");
            createSupplier("SUP-009");

            var po = PurchaseOrder.builder()
                    .purchaseOrderNumber("PO-2025-006")
                    .orderDate(LocalDate.of(2025, 3, 1))
                    .supplierCode("SUP-009")
                    .build();
            purchaseOrderRepository.save(po);

            var detail = PurchaseOrderDetail.builder()
                    .purchaseOrderNumber("PO-2025-006")
                    .lineNumber(1)
                    .itemCode("MAT-007")
                    .expectedReceivingDate(LocalDate.of(2025, 3, 15))
                    .orderUnitPrice(new BigDecimal("1500"))
                    .orderQuantity(new BigDecimal("100"))
                    .orderAmount(new BigDecimal("150000"))
                    .build();
            detailRepository.save(detail);

            // Act
            detailRepository.updateAcceptedQuantity(detail.getId(), new BigDecimal("100"));

            // Assert
            var result = detailRepository.findById(detail.getId());
            assertThat(result).isPresent();
            assertThat(result.get().getAcceptedQuantity()).isEqualByComparingTo(new BigDecimal("100"));
        }

        @Test
        @DisplayName("完了フラグを更新できる")
        void canUpdateCompletedFlag() {
            // Arrange
            createItem("MAT-008");
            createSupplier("SUP-010");

            var po = PurchaseOrder.builder()
                    .purchaseOrderNumber("PO-2025-007")
                    .orderDate(LocalDate.of(2025, 3, 1))
                    .supplierCode("SUP-010")
                    .build();
            purchaseOrderRepository.save(po);

            var detail = PurchaseOrderDetail.builder()
                    .purchaseOrderNumber("PO-2025-007")
                    .lineNumber(1)
                    .itemCode("MAT-008")
                    .expectedReceivingDate(LocalDate.of(2025, 3, 15))
                    .orderUnitPrice(new BigDecimal("1500"))
                    .orderQuantity(new BigDecimal("100"))
                    .orderAmount(new BigDecimal("150000"))
                    .build();
            detailRepository.save(detail);

            // Act
            detailRepository.updateCompletedFlag(detail.getId(), true);

            // Assert
            var result = detailRepository.findById(detail.getId());
            assertThat(result).isPresent();
            assertThat(result.get().getCompletedFlag()).isTrue();
        }
    }
}
