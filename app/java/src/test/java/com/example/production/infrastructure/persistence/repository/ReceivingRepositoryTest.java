package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.*;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import com.example.production.domain.model.purchase.*;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("入荷・検収リポジトリ")
class ReceivingRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private ReceivingRepository receivingRepository;

    @Autowired
    private InspectionRepository inspectionRepository;

    @Autowired
    private AcceptanceRepository acceptanceRepository;

    @Autowired
    private DefectRepository defectRepository;

    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;

    @Autowired
    private PurchaseOrderDetailRepository detailRepository;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private SupplierRepository supplierRepository;

    @Autowired
    private SupplyDetailRepository supplyDetailRepository;

    @Autowired
    private SupplyRepository supplyRepository;

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
        acceptanceRepository.deleteAll();
        inspectionRepository.deleteAll();
        receivingRepository.deleteAll();
        defectRepository.deleteAll();
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
                .supplierType(SupplierType.VENDOR)
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
                .orderUnitPrice(new BigDecimal("1000"))
                .orderQuantity(new BigDecimal("100"))
                .orderAmount(new BigDecimal("100000"))
                .build();
        detailRepository.save(detail);
        return detail;
    }

    @Nested
    @DisplayName("欠点マスタ")
    class DefectTests {

        @Test
        @DisplayName("欠点マスタを登録できる")
        void canRegisterDefect() {
            var defect = Defect.builder()
                    .defectCode("DEF-001")
                    .defectDescription("キズ")
                    .build();

            defectRepository.save(defect);

            assertThat(defect.getId()).isNotNull();
        }

        @Test
        @DisplayName("欠点コードで検索できる")
        void canFindByDefectCode() {
            var defect = Defect.builder()
                    .defectCode("DEF-002")
                    .defectDescription("汚れ")
                    .build();
            defectRepository.save(defect);

            var result = defectRepository.findByDefectCode("DEF-002");

            assertThat(result).isPresent();
            assertThat(result.get().getDefectDescription()).isEqualTo("汚れ");
        }

        @Test
        @DisplayName("全件取得できる")
        void canFindAll() {
            defectRepository.save(Defect.builder().defectCode("DEF-A").defectDescription("欠点A").build());
            defectRepository.save(Defect.builder().defectCode("DEF-B").defectDescription("欠点B").build());

            var result = defectRepository.findAll();

            assertThat(result).hasSize(2);
        }
    }

    @Nested
    @DisplayName("入荷受入データ")
    class ReceivingTests {

        @Test
        @DisplayName("入荷を登録できる")
        void canRegisterReceiving() {
            createItem("MAT-001");
            createSupplier("SUP-001");
            createPurchaseOrder("PO-001", "SUP-001");
            createPurchaseOrderDetail("PO-001", 1, "MAT-001");

            var receiving = Receiving.builder()
                    .receivingNumber("RCV-2025-001")
                    .purchaseOrderNumber("PO-001")
                    .lineNumber(1)
                    .receivingDate(LocalDate.of(2025, 3, 15))
                    .receivingType(ReceivingType.NORMAL)
                    .itemCode("MAT-001")
                    .miscellaneousItemFlag(false)
                    .receivingQuantity(new BigDecimal("50"))
                    .build();

            receivingRepository.save(receiving);

            assertThat(receiving.getId()).isNotNull();
        }

        @Test
        @DisplayName("入荷番号で検索できる")
        void canFindByReceivingNumber() {
            createItem("MAT-002");
            createSupplier("SUP-002");
            createPurchaseOrder("PO-002", "SUP-002");
            createPurchaseOrderDetail("PO-002", 1, "MAT-002");

            var receiving = Receiving.builder()
                    .receivingNumber("RCV-2025-002")
                    .purchaseOrderNumber("PO-002")
                    .lineNumber(1)
                    .receivingDate(LocalDate.of(2025, 3, 15))
                    .receivingType(ReceivingType.NORMAL)
                    .itemCode("MAT-002")
                    .miscellaneousItemFlag(false)
                    .receivingQuantity(new BigDecimal("100"))
                    .build();
            receivingRepository.save(receiving);

            var result = receivingRepository.findByReceivingNumber("RCV-2025-002");

            assertThat(result).isPresent();
            assertThat(result.get().getReceivingQuantity()).isEqualByComparingTo(new BigDecimal("100"));
        }

        @Test
        @DisplayName("発注番号で入荷一覧を取得できる")
        void canFindByPurchaseOrderNumber() {
            createItem("MAT-003");
            createSupplier("SUP-003");
            createPurchaseOrder("PO-003", "SUP-003");
            createPurchaseOrderDetail("PO-003", 1, "MAT-003");

            receivingRepository.save(Receiving.builder()
                    .receivingNumber("RCV-2025-003A")
                    .purchaseOrderNumber("PO-003")
                    .lineNumber(1)
                    .receivingDate(LocalDate.of(2025, 3, 15))
                    .receivingType(ReceivingType.NORMAL)
                    .itemCode("MAT-003")
                    .miscellaneousItemFlag(false)
                    .receivingQuantity(new BigDecimal("30"))
                    .build());

            receivingRepository.save(Receiving.builder()
                    .receivingNumber("RCV-2025-003B")
                    .purchaseOrderNumber("PO-003")
                    .lineNumber(1)
                    .receivingDate(LocalDate.of(2025, 3, 16))
                    .receivingType(ReceivingType.SPLIT)
                    .itemCode("MAT-003")
                    .miscellaneousItemFlag(false)
                    .receivingQuantity(new BigDecimal("70"))
                    .build());

            var result = receivingRepository.findByPurchaseOrderNumber("PO-003");

            assertThat(result).hasSize(2);
        }
    }

    @Nested
    @DisplayName("受入検査データ")
    class InspectionTests {

        @Test
        @DisplayName("受入検査を登録できる")
        void canRegisterInspection() {
            createItem("MAT-004");
            createSupplier("SUP-004");
            createPurchaseOrder("PO-004", "SUP-004");
            createPurchaseOrderDetail("PO-004", 1, "MAT-004");

            var receiving = Receiving.builder()
                    .receivingNumber("RCV-2025-004")
                    .purchaseOrderNumber("PO-004")
                    .lineNumber(1)
                    .receivingDate(LocalDate.of(2025, 3, 15))
                    .receivingType(ReceivingType.NORMAL)
                    .itemCode("MAT-004")
                    .miscellaneousItemFlag(false)
                    .receivingQuantity(new BigDecimal("100"))
                    .build();
            receivingRepository.save(receiving);

            var inspection = Inspection.builder()
                    .inspectionNumber("INS-2025-001")
                    .receivingNumber("RCV-2025-004")
                    .purchaseOrderNumber("PO-004")
                    .lineNumber(1)
                    .inspectionDate(LocalDate.of(2025, 3, 16))
                    .itemCode("MAT-004")
                    .miscellaneousItemFlag(false)
                    .goodQuantity(new BigDecimal("95"))
                    .defectQuantity(new BigDecimal("5"))
                    .build();

            inspectionRepository.save(inspection);

            assertThat(inspection.getId()).isNotNull();
        }

        @Test
        @DisplayName("検査番号で検索できる")
        void canFindByInspectionNumber() {
            createItem("MAT-005");
            createSupplier("SUP-005");
            createPurchaseOrder("PO-005", "SUP-005");
            createPurchaseOrderDetail("PO-005", 1, "MAT-005");

            receivingRepository.save(Receiving.builder()
                    .receivingNumber("RCV-2025-005")
                    .purchaseOrderNumber("PO-005")
                    .lineNumber(1)
                    .receivingDate(LocalDate.of(2025, 3, 15))
                    .receivingType(ReceivingType.NORMAL)
                    .itemCode("MAT-005")
                    .miscellaneousItemFlag(false)
                    .receivingQuantity(new BigDecimal("100"))
                    .build());

            inspectionRepository.save(Inspection.builder()
                    .inspectionNumber("INS-2025-002")
                    .receivingNumber("RCV-2025-005")
                    .purchaseOrderNumber("PO-005")
                    .lineNumber(1)
                    .inspectionDate(LocalDate.of(2025, 3, 16))
                    .itemCode("MAT-005")
                    .miscellaneousItemFlag(false)
                    .goodQuantity(new BigDecimal("100"))
                    .defectQuantity(BigDecimal.ZERO)
                    .build());

            var result = inspectionRepository.findByInspectionNumber("INS-2025-002");

            assertThat(result).isPresent();
            assertThat(result.get().getGoodQuantity()).isEqualByComparingTo(new BigDecimal("100"));
        }
    }

    @Nested
    @DisplayName("検収データ")
    class AcceptanceTests {

        @Test
        @DisplayName("検収を登録できる")
        void canRegisterAcceptance() {
            createItem("MAT-006");
            createSupplier("SUP-006");
            createPurchaseOrder("PO-006", "SUP-006");
            createPurchaseOrderDetail("PO-006", 1, "MAT-006");

            receivingRepository.save(Receiving.builder()
                    .receivingNumber("RCV-2025-006")
                    .purchaseOrderNumber("PO-006")
                    .lineNumber(1)
                    .receivingDate(LocalDate.of(2025, 3, 15))
                    .receivingType(ReceivingType.NORMAL)
                    .itemCode("MAT-006")
                    .miscellaneousItemFlag(false)
                    .receivingQuantity(new BigDecimal("100"))
                    .build());

            inspectionRepository.save(Inspection.builder()
                    .inspectionNumber("INS-2025-003")
                    .receivingNumber("RCV-2025-006")
                    .purchaseOrderNumber("PO-006")
                    .lineNumber(1)
                    .inspectionDate(LocalDate.of(2025, 3, 16))
                    .itemCode("MAT-006")
                    .miscellaneousItemFlag(false)
                    .goodQuantity(new BigDecimal("100"))
                    .defectQuantity(BigDecimal.ZERO)
                    .build());

            var acceptance = Acceptance.builder()
                    .acceptanceNumber("ACC-2025-001")
                    .inspectionNumber("INS-2025-003")
                    .purchaseOrderNumber("PO-006")
                    .lineNumber(1)
                    .acceptanceDate(LocalDate.of(2025, 3, 17))
                    .supplierCode("SUP-006")
                    .itemCode("MAT-006")
                    .miscellaneousItemFlag(false)
                    .acceptedQuantity(new BigDecimal("100"))
                    .unitPrice(new BigDecimal("1000"))
                    .amount(new BigDecimal("100000"))
                    .taxAmount(new BigDecimal("10000"))
                    .build();

            acceptanceRepository.save(acceptance);

            assertThat(acceptance.getId()).isNotNull();
        }

        @Test
        @DisplayName("検収番号で検索できる")
        void canFindByAcceptanceNumber() {
            createItem("MAT-007");
            createSupplier("SUP-007");
            createPurchaseOrder("PO-007", "SUP-007");
            createPurchaseOrderDetail("PO-007", 1, "MAT-007");

            receivingRepository.save(Receiving.builder()
                    .receivingNumber("RCV-2025-007")
                    .purchaseOrderNumber("PO-007")
                    .lineNumber(1)
                    .receivingDate(LocalDate.of(2025, 3, 15))
                    .receivingType(ReceivingType.NORMAL)
                    .itemCode("MAT-007")
                    .miscellaneousItemFlag(false)
                    .receivingQuantity(new BigDecimal("50"))
                    .build());

            inspectionRepository.save(Inspection.builder()
                    .inspectionNumber("INS-2025-004")
                    .receivingNumber("RCV-2025-007")
                    .purchaseOrderNumber("PO-007")
                    .lineNumber(1)
                    .inspectionDate(LocalDate.of(2025, 3, 16))
                    .itemCode("MAT-007")
                    .miscellaneousItemFlag(false)
                    .goodQuantity(new BigDecimal("50"))
                    .defectQuantity(BigDecimal.ZERO)
                    .build());

            acceptanceRepository.save(Acceptance.builder()
                    .acceptanceNumber("ACC-2025-002")
                    .inspectionNumber("INS-2025-004")
                    .purchaseOrderNumber("PO-007")
                    .lineNumber(1)
                    .acceptanceDate(LocalDate.of(2025, 3, 17))
                    .supplierCode("SUP-007")
                    .itemCode("MAT-007")
                    .miscellaneousItemFlag(false)
                    .acceptedQuantity(new BigDecimal("50"))
                    .unitPrice(new BigDecimal("2000"))
                    .amount(new BigDecimal("100000"))
                    .taxAmount(new BigDecimal("10000"))
                    .build());

            var result = acceptanceRepository.findByAcceptanceNumber("ACC-2025-002");

            assertThat(result).isPresent();
            assertThat(result.get().getAcceptedQuantity()).isEqualByComparingTo(new BigDecimal("50"));
            assertThat(result.get().getUnitPrice()).isEqualByComparingTo(new BigDecimal("2000"));
        }

        @Test
        @DisplayName("発注番号で検収一覧を取得できる")
        void canFindByPurchaseOrderNumber() {
            createItem("MAT-008");
            createSupplier("SUP-008");
            createPurchaseOrder("PO-008", "SUP-008");
            createPurchaseOrderDetail("PO-008", 1, "MAT-008");

            receivingRepository.save(Receiving.builder()
                    .receivingNumber("RCV-2025-008")
                    .purchaseOrderNumber("PO-008")
                    .lineNumber(1)
                    .receivingDate(LocalDate.of(2025, 3, 15))
                    .receivingType(ReceivingType.NORMAL)
                    .itemCode("MAT-008")
                    .miscellaneousItemFlag(false)
                    .receivingQuantity(new BigDecimal("100"))
                    .build());

            inspectionRepository.save(Inspection.builder()
                    .inspectionNumber("INS-2025-005")
                    .receivingNumber("RCV-2025-008")
                    .purchaseOrderNumber("PO-008")
                    .lineNumber(1)
                    .inspectionDate(LocalDate.of(2025, 3, 16))
                    .itemCode("MAT-008")
                    .miscellaneousItemFlag(false)
                    .goodQuantity(new BigDecimal("100"))
                    .defectQuantity(BigDecimal.ZERO)
                    .build());

            acceptanceRepository.save(Acceptance.builder()
                    .acceptanceNumber("ACC-2025-003")
                    .inspectionNumber("INS-2025-005")
                    .purchaseOrderNumber("PO-008")
                    .lineNumber(1)
                    .acceptanceDate(LocalDate.of(2025, 3, 17))
                    .supplierCode("SUP-008")
                    .itemCode("MAT-008")
                    .miscellaneousItemFlag(false)
                    .acceptedQuantity(new BigDecimal("100"))
                    .unitPrice(new BigDecimal("1000"))
                    .amount(new BigDecimal("100000"))
                    .taxAmount(new BigDecimal("10000"))
                    .build());

            var result = acceptanceRepository.findByPurchaseOrderNumber("PO-008");

            assertThat(result).hasSize(1);
            assertThat(result.get(0).getAcceptanceNumber()).isEqualTo("ACC-2025-003");
        }
    }
}
