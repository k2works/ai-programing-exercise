package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.*;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import com.example.production.domain.model.purchase.*;
import com.example.production.domain.model.purchase.ReceivingType;
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

@DisplayName("消費リポジトリ")
class ConsumptionRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private ConsumptionRepository consumptionRepository;

    @Autowired
    private ConsumptionDetailRepository consumptionDetailRepository;

    @Autowired
    private ReceivingRepository receivingRepository;

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

    @BeforeEach
    void setUp() {
        consumptionDetailRepository.deleteAll();
        consumptionRepository.deleteAll();
        supplyDetailRepository.deleteAll();
        supplyRepository.deleteAll();
        receivingRepository.deleteAll();
        purchaseOrderDetailRepository.deleteAll();
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
                .supplierName("テスト外注先")
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
        purchaseOrderDetailRepository.save(detail);
        return detail;
    }

    private Receiving createReceiving(String receivingNumber, String poNumber, int lineNumber, String itemCode) {
        var receiving = Receiving.builder()
                .receivingNumber(receivingNumber)
                .purchaseOrderNumber(poNumber)
                .lineNumber(lineNumber)
                .receivingDate(LocalDate.of(2025, 3, 15))
                .receivingType(ReceivingType.NORMAL)
                .itemCode(itemCode)
                .receivingQuantity(new BigDecimal("100"))
                .miscellaneousItemFlag(false)
                .build();
        receivingRepository.save(receiving);
        return receiving;
    }

    private Supply createSupply(String supplyNumber, String poNumber, int lineNumber, String supplierCode) {
        var supply = Supply.builder()
                .supplyNumber(supplyNumber)
                .purchaseOrderNumber(poNumber)
                .lineNumber(lineNumber)
                .supplierCode(supplierCode)
                .supplyDate(LocalDate.of(2025, 3, 5))
                .supplierPersonCode("EMP-001")
                .supplyType(SupplyType.FREE)
                .build();
        supplyRepository.save(supply);
        return supply;
    }

    private SupplyDetail createSupplyDetail(String supplyNumber, int lineNumber, String itemCode, BigDecimal quantity) {
        var detail = SupplyDetail.builder()
                .supplyNumber(supplyNumber)
                .lineNumber(lineNumber)
                .itemCode(itemCode)
                .quantity(quantity)
                .unitPrice(new BigDecimal("100"))
                .amount(quantity.multiply(new BigDecimal("100")))
                .build();
        supplyDetailRepository.save(detail);
        return detail;
    }

    @Nested
    @DisplayName("消費データ")
    class ConsumptionTests {

        @Test
        @DisplayName("消費を登録できる")
        void canRegisterConsumption() {
            createItem("MAT-001");
            createItem("MAT-002");
            createSupplier("SUP-001");
            createPurchaseOrder("PO-001", "SUP-001");
            createPurchaseOrderDetail("PO-001", 1, "MAT-001");
            createReceiving("RCV-001", "PO-001", 1, "MAT-001");

            var consumption = Consumption.builder()
                    .consumptionNumber("CON-2025-001")
                    .receivingNumber("RCV-001")
                    .consumptionDate(LocalDate.of(2025, 3, 16))
                    .supplierCode("SUP-001")
                    .build();

            consumptionRepository.save(consumption);

            assertThat(consumption.getId()).isNotNull();
        }

        @Test
        @DisplayName("消費番号で検索できる")
        void canFindByConsumptionNumber() {
            createItem("MAT-003");
            createSupplier("SUP-002");
            createPurchaseOrder("PO-002", "SUP-002");
            createPurchaseOrderDetail("PO-002", 1, "MAT-003");
            createReceiving("RCV-002", "PO-002", 1, "MAT-003");

            var consumption = Consumption.builder()
                    .consumptionNumber("CON-2025-002")
                    .receivingNumber("RCV-002")
                    .consumptionDate(LocalDate.of(2025, 3, 16))
                    .supplierCode("SUP-002")
                    .build();
            consumptionRepository.save(consumption);

            var result = consumptionRepository.findByConsumptionNumber("CON-2025-002");

            assertThat(result).isPresent();
            assertThat(result.get().getReceivingNumber()).isEqualTo("RCV-002");
        }

        @Test
        @DisplayName("入荷番号で検索できる")
        void canFindByReceivingNumber() {
            createItem("MAT-004");
            createSupplier("SUP-003");
            createPurchaseOrder("PO-003", "SUP-003");
            createPurchaseOrderDetail("PO-003", 1, "MAT-004");
            createReceiving("RCV-003", "PO-003", 1, "MAT-004");

            var consumption = Consumption.builder()
                    .consumptionNumber("CON-2025-003")
                    .receivingNumber("RCV-003")
                    .consumptionDate(LocalDate.of(2025, 3, 16))
                    .supplierCode("SUP-003")
                    .build();
            consumptionRepository.save(consumption);

            var result = consumptionRepository.findByReceivingNumber("RCV-003");

            assertThat(result).isPresent();
            assertThat(result.get().getConsumptionNumber()).isEqualTo("CON-2025-003");
        }
    }

    @Nested
    @DisplayName("消費明細データ")
    class ConsumptionDetailTests {

        @Test
        @DisplayName("消費明細を登録できる")
        void canRegisterConsumptionDetail() {
            createItem("MAT-005");
            createSupplier("SUP-004");
            createPurchaseOrder("PO-004", "SUP-004");
            createPurchaseOrderDetail("PO-004", 1, "MAT-005");
            createReceiving("RCV-004", "PO-004", 1, "MAT-005");

            consumptionRepository.save(Consumption.builder()
                    .consumptionNumber("CON-2025-004")
                    .receivingNumber("RCV-004")
                    .consumptionDate(LocalDate.of(2025, 3, 16))
                    .supplierCode("SUP-004")
                    .build());

            var detail = ConsumptionDetail.builder()
                    .consumptionNumber("CON-2025-004")
                    .lineNumber(1)
                    .itemCode("MAT-005")
                    .quantity(new BigDecimal("95"))
                    .build();

            consumptionDetailRepository.save(detail);

            assertThat(detail.getId()).isNotNull();
        }

        @Test
        @DisplayName("消費番号で明細一覧を取得できる")
        void canFindByConsumptionNumber() {
            createItem("MAT-006");
            createItem("MAT-007");
            createSupplier("SUP-005");
            createPurchaseOrder("PO-005", "SUP-005");
            createPurchaseOrderDetail("PO-005", 1, "MAT-006");
            createReceiving("RCV-005", "PO-005", 1, "MAT-006");

            consumptionRepository.save(Consumption.builder()
                    .consumptionNumber("CON-2025-005")
                    .receivingNumber("RCV-005")
                    .consumptionDate(LocalDate.of(2025, 3, 16))
                    .supplierCode("SUP-005")
                    .build());

            consumptionDetailRepository.save(ConsumptionDetail.builder()
                    .consumptionNumber("CON-2025-005")
                    .lineNumber(1)
                    .itemCode("MAT-006")
                    .quantity(new BigDecimal("50"))
                    .build());

            consumptionDetailRepository.save(ConsumptionDetail.builder()
                    .consumptionNumber("CON-2025-005")
                    .lineNumber(2)
                    .itemCode("MAT-007")
                    .quantity(new BigDecimal("30"))
                    .build());

            var result = consumptionDetailRepository.findByConsumptionNumber("CON-2025-005");

            assertThat(result).hasSize(2);
            assertThat(result.get(0).getLineNumber()).isEqualTo(1);
            assertThat(result.get(1).getLineNumber()).isEqualTo(2);
        }

        @Test
        @DisplayName("発注明細と品目で消費数量を集計できる")
        void canSumByPurchaseOrderAndItem() {
            createItem("MAT-008");
            createSupplier("SUP-006");
            createPurchaseOrder("PO-006", "SUP-006");
            createPurchaseOrderDetail("PO-006", 1, "MAT-008");
            createReceiving("RCV-006", "PO-006", 1, "MAT-008");
            createSupply("SPL-006", "PO-006", 1, "SUP-006");
            createSupplyDetail("SPL-006", 1, "MAT-008", new BigDecimal("100"));

            // 複数回の消費を記録
            consumptionRepository.save(Consumption.builder()
                    .consumptionNumber("CON-2025-006A")
                    .receivingNumber("RCV-006")
                    .consumptionDate(LocalDate.of(2025, 3, 16))
                    .supplierCode("SUP-006")
                    .build());
            consumptionDetailRepository.save(ConsumptionDetail.builder()
                    .consumptionNumber("CON-2025-006A")
                    .lineNumber(1)
                    .itemCode("MAT-008")
                    .quantity(new BigDecimal("40"))
                    .build());

            consumptionRepository.save(Consumption.builder()
                    .consumptionNumber("CON-2025-006B")
                    .receivingNumber("RCV-006")
                    .consumptionDate(LocalDate.of(2025, 3, 17))
                    .supplierCode("SUP-006")
                    .build());
            consumptionDetailRepository.save(ConsumptionDetail.builder()
                    .consumptionNumber("CON-2025-006B")
                    .lineNumber(1)
                    .itemCode("MAT-008")
                    .quantity(new BigDecimal("55"))
                    .build());

            var result = consumptionDetailRepository.sumByPurchaseOrderAndItem("PO-006", 1, "MAT-008");

            assertThat(result).isEqualByComparingTo(new BigDecimal("95"));
        }
    }
}
