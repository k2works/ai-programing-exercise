package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.*;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import com.example.production.domain.model.purchase.*;
import com.example.production.domain.model.subcontract.*;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("支給リポジトリ")
class SupplyRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private SupplyRepository supplyRepository;

    @Autowired
    private SupplyDetailRepository supplyDetailRepository;

    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;

    @Autowired
    private PurchaseOrderDetailRepository detailRepository;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private SupplierRepository supplierRepository;

    @Autowired
    private ConsumptionDetailRepository consumptionDetailRepository;

    @Autowired
    private ConsumptionRepository consumptionRepository;

    @BeforeEach
    void setUp() {
        consumptionDetailRepository.deleteAll();
        consumptionRepository.deleteAll();
        supplyDetailRepository.deleteAll();
        supplyRepository.deleteAll();
        detailRepository.deleteAll();
        purchaseOrderRepository.deleteAll();
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
                .supplierType(SupplierType.SUBCONTRACTOR)
                .build();
        supplierRepository.save(supplier);
        return supplier;
    }

    private PurchaseOrder createPurchaseOrder(String poNumber, String supplierCode) {
        var po = PurchaseOrder.builder()
                .purchaseOrderNumber(poNumber)
                .orderDate(LocalDate.of(2025, 3, 1))
                .supplierCode(supplierCode)
                .status(PurchaseOrderStatus.ORDERED)
                .build();
        purchaseOrderRepository.save(po);
        return po;
    }

    private PurchaseOrderDetail createPurchaseOrderDetail(String poNumber, int lineNumber, String itemCode) {
        var detail = PurchaseOrderDetail.builder()
                .purchaseOrderNumber(poNumber)
                .lineNumber(lineNumber)
                .itemCode(itemCode)
                .expectedReceivingDate(LocalDate.of(2025, 3, 15))
                .orderUnitPrice(new BigDecimal("5000"))
                .orderQuantity(new BigDecimal("100"))
                .orderAmount(new BigDecimal("500000"))
                .build();
        detailRepository.save(detail);
        return detail;
    }

    @Nested
    @DisplayName("支給データ")
    class SupplyTests {

        @Test
        @DisplayName("支給を登録できる")
        void canRegisterSupply() {
            createItem("MAT-001");
            createSupplier("SUP-001");
            createPurchaseOrder("PO-001", "SUP-001");
            createPurchaseOrderDetail("PO-001", 1, "MAT-001");

            var supply = Supply.builder()
                    .supplyNumber("SPL-2025-001")
                    .purchaseOrderNumber("PO-001")
                    .lineNumber(1)
                    .supplierCode("SUP-001")
                    .supplyDate(LocalDate.of(2025, 3, 5))
                    .supplierPersonCode("EMP-001")
                    .supplyType(SupplyType.FREE)
                    .build();

            supplyRepository.save(supply);

            assertThat(supply.getId()).isNotNull();
        }

        @Test
        @DisplayName("支給番号で検索できる")
        void canFindBySupplyNumber() {
            createItem("MAT-002");
            createSupplier("SUP-002");
            createPurchaseOrder("PO-002", "SUP-002");
            createPurchaseOrderDetail("PO-002", 1, "MAT-002");

            var supply = Supply.builder()
                    .supplyNumber("SPL-2025-002")
                    .purchaseOrderNumber("PO-002")
                    .lineNumber(1)
                    .supplierCode("SUP-002")
                    .supplyDate(LocalDate.of(2025, 3, 5))
                    .supplierPersonCode("EMP-002")
                    .supplyType(SupplyType.PAID)
                    .build();
            supplyRepository.save(supply);

            var result = supplyRepository.findBySupplyNumber("SPL-2025-002");

            assertThat(result).isPresent();
            assertThat(result.get().getSupplyType()).isEqualTo(SupplyType.PAID);
        }

        @Test
        @DisplayName("全ての支給区分を設定できる")
        void canSetAllSupplyTypes() {
            createItem("MAT-003");
            createSupplier("SUP-003");
            createPurchaseOrder("PO-003", "SUP-003");
            createPurchaseOrderDetail("PO-003", 1, "MAT-003");
            createPurchaseOrderDetail("PO-003", 2, "MAT-003");

            for (var type : SupplyType.values()) {
                var supply = Supply.builder()
                        .supplyNumber("SPL-T-" + type.ordinal())
                        .purchaseOrderNumber("PO-003")
                        .lineNumber(type.ordinal() + 1)
                        .supplierCode("SUP-003")
                        .supplyDate(LocalDate.of(2025, 3, 5))
                        .supplierPersonCode("EMP-003")
                        .supplyType(type)
                        .build();
                supplyRepository.save(supply);

                var result = supplyRepository.findBySupplyNumber(supply.getSupplyNumber());
                assertThat(result).isPresent();
                assertThat(result.get().getSupplyType()).isEqualTo(type);
            }
        }

        @Test
        @DisplayName("発注明細で支給を検索できる")
        void canFindByPurchaseOrderDetail() {
            createItem("MAT-004");
            createSupplier("SUP-004");
            createPurchaseOrder("PO-004", "SUP-004");
            createPurchaseOrderDetail("PO-004", 1, "MAT-004");

            supplyRepository.save(Supply.builder()
                    .supplyNumber("SPL-2025-004A")
                    .purchaseOrderNumber("PO-004")
                    .lineNumber(1)
                    .supplierCode("SUP-004")
                    .supplyDate(LocalDate.of(2025, 3, 5))
                    .supplierPersonCode("EMP-004")
                    .supplyType(SupplyType.FREE)
                    .build());

            supplyRepository.save(Supply.builder()
                    .supplyNumber("SPL-2025-004B")
                    .purchaseOrderNumber("PO-004")
                    .lineNumber(1)
                    .supplierCode("SUP-004")
                    .supplyDate(LocalDate.of(2025, 3, 6))
                    .supplierPersonCode("EMP-004")
                    .supplyType(SupplyType.FREE)
                    .build());

            var result = supplyRepository.findByPurchaseOrderDetail("PO-004", 1);

            assertThat(result).hasSize(2);
        }
    }

    @Nested
    @DisplayName("支給明細データ")
    class SupplyDetailTests {

        @Test
        @DisplayName("支給明細を登録できる")
        void canRegisterSupplyDetail() {
            createItem("MAT-005");
            createSupplier("SUP-005");
            createPurchaseOrder("PO-005", "SUP-005");
            createPurchaseOrderDetail("PO-005", 1, "MAT-005");

            supplyRepository.save(Supply.builder()
                    .supplyNumber("SPL-2025-005")
                    .purchaseOrderNumber("PO-005")
                    .lineNumber(1)
                    .supplierCode("SUP-005")
                    .supplyDate(LocalDate.of(2025, 3, 5))
                    .supplierPersonCode("EMP-005")
                    .supplyType(SupplyType.FREE)
                    .build());

            var detail = SupplyDetail.builder()
                    .supplyNumber("SPL-2025-005")
                    .lineNumber(1)
                    .itemCode("MAT-005")
                    .quantity(new BigDecimal("50"))
                    .unitPrice(new BigDecimal("100"))
                    .amount(new BigDecimal("5000"))
                    .build();

            supplyDetailRepository.save(detail);

            assertThat(detail.getId()).isNotNull();
        }

        @Test
        @DisplayName("支給番号で明細一覧を取得できる")
        void canFindBySupplyNumber() {
            createItem("MAT-006");
            createItem("MAT-007");
            createSupplier("SUP-006");
            createPurchaseOrder("PO-006", "SUP-006");
            createPurchaseOrderDetail("PO-006", 1, "MAT-006");

            supplyRepository.save(Supply.builder()
                    .supplyNumber("SPL-2025-006")
                    .purchaseOrderNumber("PO-006")
                    .lineNumber(1)
                    .supplierCode("SUP-006")
                    .supplyDate(LocalDate.of(2025, 3, 5))
                    .supplierPersonCode("EMP-006")
                    .supplyType(SupplyType.PAID)
                    .build());

            supplyDetailRepository.save(SupplyDetail.builder()
                    .supplyNumber("SPL-2025-006")
                    .lineNumber(1)
                    .itemCode("MAT-006")
                    .quantity(new BigDecimal("30"))
                    .unitPrice(new BigDecimal("200"))
                    .amount(new BigDecimal("6000"))
                    .build());

            supplyDetailRepository.save(SupplyDetail.builder()
                    .supplyNumber("SPL-2025-006")
                    .lineNumber(2)
                    .itemCode("MAT-007")
                    .quantity(new BigDecimal("20"))
                    .unitPrice(new BigDecimal("150"))
                    .amount(new BigDecimal("3000"))
                    .build());

            var result = supplyDetailRepository.findBySupplyNumber("SPL-2025-006");

            assertThat(result).hasSize(2);
            assertThat(result.get(0).getLineNumber()).isEqualTo(1);
            assertThat(result.get(1).getLineNumber()).isEqualTo(2);
        }

        @Test
        @DisplayName("支給明細の金額を確認できる")
        void canVerifySupplyDetailAmounts() {
            createItem("MAT-008");
            createSupplier("SUP-007");
            createPurchaseOrder("PO-007", "SUP-007");
            createPurchaseOrderDetail("PO-007", 1, "MAT-008");

            supplyRepository.save(Supply.builder()
                    .supplyNumber("SPL-2025-007")
                    .purchaseOrderNumber("PO-007")
                    .lineNumber(1)
                    .supplierCode("SUP-007")
                    .supplyDate(LocalDate.of(2025, 3, 5))
                    .supplierPersonCode("EMP-007")
                    .supplyType(SupplyType.PAID)
                    .build());

            var detail = SupplyDetail.builder()
                    .supplyNumber("SPL-2025-007")
                    .lineNumber(1)
                    .itemCode("MAT-008")
                    .quantity(new BigDecimal("100"))
                    .unitPrice(new BigDecimal("500"))
                    .amount(new BigDecimal("50000"))
                    .build();
            supplyDetailRepository.save(detail);

            var result = supplyDetailRepository.findById(detail.getId());

            assertThat(result).isPresent();
            assertThat(result.get().getQuantity()).isEqualByComparingTo(new BigDecimal("100"));
            assertThat(result.get().getUnitPrice()).isEqualByComparingTo(new BigDecimal("500"));
            assertThat(result.get().getAmount()).isEqualByComparingTo(new BigDecimal("50000"));
        }
    }
}
