package com.example.production.application.service;

import com.example.production.application.port.in.command.*;
import com.example.production.application.port.out.*;
import com.example.production.domain.model.quality.*;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("品質管理サービス")
class QualityServiceTest extends BaseIntegrationTest {

    @Autowired
    private QualityService qualityService;

    @Autowired
    private DefectMasterRepository defectMasterRepository;

    @Autowired
    private ShipmentInspectionRepository shipmentInspectionRepository;

    @Autowired
    private LotMasterRepository lotMasterRepository;

    @Autowired
    private LotCompositionRepository lotCompositionRepository;

    @BeforeEach
    void setUp() {
        // クリーンアップ（依存関係の順序に注意）
        lotCompositionRepository.deleteAll();
        lotMasterRepository.deleteAll();
        shipmentInspectionRepository.deleteAllResults();
        shipmentInspectionRepository.deleteAll();
        defectMasterRepository.deleteAll();
    }

    @Nested
    @DisplayName("欠点マスタ管理")
    class DefectMasterTest {

        @Test
        @DisplayName("欠点マスタを登録できる")
        void shouldRegisterDefect() {
            // Act
            DefectMaster defect = qualityService.registerDefect(
                    DefectMasterCreateCommand.builder()
                            .defectCode("DEF001")
                            .defectDescription("キズ")
                            .build());

            // Assert
            assertThat(defect).isNotNull();
            assertThat(defect.getDefectCode()).isEqualTo("DEF001");
            assertThat(defect.getDefectDescription()).isEqualTo("キズ");
        }

        @Test
        @DisplayName("欠点マスタを取得できる")
        void shouldFindDefect() {
            // Arrange
            qualityService.registerDefect(
                    DefectMasterCreateCommand.builder()
                            .defectCode("DEF001")
                            .defectDescription("キズ")
                            .build());

            // Act
            DefectMaster found = qualityService.findDefectByCode("DEF001");

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getDefectCode()).isEqualTo("DEF001");
        }

        @Test
        @DisplayName("全ての欠点マスタを取得できる")
        void shouldFindAllDefects() {
            // Arrange
            qualityService.registerDefect(
                    DefectMasterCreateCommand.builder()
                            .defectCode("DEF001")
                            .defectDescription("キズ")
                            .build());
            qualityService.registerDefect(
                    DefectMasterCreateCommand.builder()
                            .defectCode("DEF002")
                            .defectDescription("汚れ")
                            .build());

            // Act
            List<DefectMaster> defects = qualityService.findAllDefects();

            // Assert
            assertThat(defects).hasSize(2);
        }
    }

    @Nested
    @DisplayName("出荷検査")
    class ShipmentInspectionTest {

        @BeforeEach
        void setUpDefects() {
            qualityService.registerDefect(
                    DefectMasterCreateCommand.builder()
                            .defectCode("DEF001")
                            .defectDescription("キズ")
                            .build());
            qualityService.registerDefect(
                    DefectMasterCreateCommand.builder()
                            .defectCode("DEF002")
                            .defectDescription("汚れ")
                            .build());
        }

        @Test
        @DisplayName("出荷検査を登録できる")
        void shouldRegisterShipmentInspection() {
            // Act
            ShipmentInspection inspection = qualityService.registerShipmentInspection(
                    ShipmentInspectionCreateCommand.builder()
                            .shipmentNumber("SHP-001")
                            .itemCode("PROD-001")
                            .inspectionDate(LocalDate.of(2025, 1, 15))
                            .inspectorCode("EMP001")
                            .inspectionQuantity(new BigDecimal("100"))
                            .passedQuantity(new BigDecimal("98"))
                            .failedQuantity(new BigDecimal("2"))
                            .judgment(InspectionJudgment.PASSED)
                            .results(List.of(
                                    ShipmentInspectionResultCommand.builder()
                                            .defectCode("DEF001")
                                            .quantity(new BigDecimal("1"))
                                            .build(),
                                    ShipmentInspectionResultCommand.builder()
                                            .defectCode("DEF002")
                                            .quantity(new BigDecimal("1"))
                                            .build()
                            ))
                            .build());

            // Assert
            assertThat(inspection).isNotNull();
            assertThat(inspection.getShipmentInspectionNumber()).matches("SI-\\d{4}-\\d{4}");
            assertThat(inspection.getShipmentNumber()).isEqualTo("SHP-001");
            assertThat(inspection.getJudgment()).isEqualTo(InspectionJudgment.PASSED);
            assertThat(inspection.getResults()).hasSize(2);
        }

        @Test
        @DisplayName("出荷検査を取得できる")
        void shouldFindShipmentInspection() {
            // Arrange
            ShipmentInspection created = qualityService.registerShipmentInspection(
                    ShipmentInspectionCreateCommand.builder()
                            .shipmentNumber("SHP-001")
                            .itemCode("PROD-001")
                            .inspectionDate(LocalDate.of(2025, 1, 15))
                            .inspectorCode("EMP001")
                            .inspectionQuantity(new BigDecimal("100"))
                            .passedQuantity(new BigDecimal("100"))
                            .failedQuantity(BigDecimal.ZERO)
                            .judgment(InspectionJudgment.PASSED)
                            .build());

            // Act
            ShipmentInspection found = qualityService.findShipmentInspection(
                    created.getShipmentInspectionNumber());

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getShipmentInspectionNumber()).isEqualTo(created.getShipmentInspectionNumber());
        }

        @Test
        @DisplayName("出荷番号で出荷検査を検索できる")
        void shouldFindByShipmentNumber() {
            // Arrange
            qualityService.registerShipmentInspection(
                    ShipmentInspectionCreateCommand.builder()
                            .shipmentNumber("SHP-001")
                            .itemCode("PROD-001")
                            .inspectionDate(LocalDate.of(2025, 1, 15))
                            .inspectorCode("EMP001")
                            .inspectionQuantity(new BigDecimal("100"))
                            .passedQuantity(new BigDecimal("100"))
                            .failedQuantity(BigDecimal.ZERO)
                            .judgment(InspectionJudgment.PASSED)
                            .build());

            // Act
            List<ShipmentInspection> inspections = qualityService.findShipmentInspectionsByShipmentNumber("SHP-001");

            // Assert
            assertThat(inspections).hasSize(1);
            assertThat(inspections.get(0).getShipmentNumber()).isEqualTo("SHP-001");
        }
    }

    @Nested
    @DisplayName("ロット管理")
    class LotManagementTest {

        @Test
        @DisplayName("購入ロットを作成できる")
        void shouldCreatePurchaseLot() {
            // Act
            LotMaster lot = qualityService.createLot(
                    LotCreateCommand.builder()
                            .lotNumber("LOT-P-001")
                            .itemCode("MAT-001")
                            .lotType(LotType.PURCHASE)
                            .productionDate(LocalDate.of(2025, 1, 10))
                            .expirationDate(LocalDate.of(2026, 1, 10))
                            .quantity(new BigDecimal("1000"))
                            .build());

            // Assert
            assertThat(lot).isNotNull();
            assertThat(lot.getLotNumber()).isEqualTo("LOT-P-001");
            assertThat(lot.getLotType()).isEqualTo(LotType.PURCHASE);
        }

        @Test
        @DisplayName("製造ロットを作成できる")
        void shouldCreateProductionLot() {
            // Act
            LotMaster lot = qualityService.createLot(
                    LotCreateCommand.builder()
                            .lotNumber("LOT-M-001")
                            .itemCode("PROD-001")
                            .lotType(LotType.PRODUCTION)
                            .productionDate(LocalDate.of(2025, 1, 15))
                            .expirationDate(LocalDate.of(2026, 1, 15))
                            .quantity(new BigDecimal("500"))
                            .build());

            // Assert
            assertThat(lot).isNotNull();
            assertThat(lot.getLotNumber()).isEqualTo("LOT-M-001");
            assertThat(lot.getLotType()).isEqualTo(LotType.PRODUCTION);
        }

        @Test
        @DisplayName("ロットを取得できる")
        void shouldFindLot() {
            // Arrange
            qualityService.createLot(
                    LotCreateCommand.builder()
                            .lotNumber("LOT-P-001")
                            .itemCode("MAT-001")
                            .lotType(LotType.PURCHASE)
                            .productionDate(LocalDate.of(2025, 1, 10))
                            .quantity(new BigDecimal("1000"))
                            .build());

            // Act
            LotMaster found = qualityService.findLot("LOT-P-001");

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getLotNumber()).isEqualTo("LOT-P-001");
        }

        @Test
        @DisplayName("品目コードでロットを検索できる")
        void shouldFindLotsByItemCode() {
            // Arrange
            qualityService.createLot(
                    LotCreateCommand.builder()
                            .lotNumber("LOT-P-001")
                            .itemCode("MAT-001")
                            .lotType(LotType.PURCHASE)
                            .productionDate(LocalDate.of(2025, 1, 10))
                            .quantity(new BigDecimal("1000"))
                            .build());
            qualityService.createLot(
                    LotCreateCommand.builder()
                            .lotNumber("LOT-P-002")
                            .itemCode("MAT-001")
                            .lotType(LotType.PURCHASE)
                            .productionDate(LocalDate.of(2025, 1, 15))
                            .quantity(new BigDecimal("500"))
                            .build());

            // Act
            List<LotMaster> lots = qualityService.findLotsByItemCode("MAT-001");

            // Assert
            assertThat(lots).hasSize(2);
        }
    }

    @Nested
    @DisplayName("トレーサビリティ")
    class TraceabilityTest {

        @BeforeEach
        void setUpLots() {
            // 購入ロット（材料）
            qualityService.createLot(
                    LotCreateCommand.builder()
                            .lotNumber("LOT-P-001")
                            .itemCode("MAT-001")
                            .lotType(LotType.PURCHASE)
                            .productionDate(LocalDate.of(2025, 1, 1))
                            .quantity(new BigDecimal("1000"))
                            .build());
            qualityService.createLot(
                    LotCreateCommand.builder()
                            .lotNumber("LOT-P-002")
                            .itemCode("MAT-002")
                            .lotType(LotType.PURCHASE)
                            .productionDate(LocalDate.of(2025, 1, 2))
                            .quantity(new BigDecimal("500"))
                            .build());

            // 製造ロット（製品）
            qualityService.createLot(
                    LotCreateCommand.builder()
                            .lotNumber("LOT-M-001")
                            .itemCode("PROD-001")
                            .lotType(LotType.PRODUCTION)
                            .productionDate(LocalDate.of(2025, 1, 10))
                            .quantity(new BigDecimal("100"))
                            .build());
        }

        @Test
        @DisplayName("ロット構成を登録できる")
        void shouldRegisterLotComposition() {
            // Act
            LotComposition composition = qualityService.registerLotComposition(
                    LotCompositionCreateCommand.builder()
                            .parentLotNumber("LOT-M-001")
                            .childLotNumber("LOT-P-001")
                            .usedQuantity(new BigDecimal("100"))
                            .build());

            // Assert
            assertThat(composition).isNotNull();
            assertThat(composition.getParentLotNumber()).isEqualTo("LOT-M-001");
            assertThat(composition.getChildLotNumber()).isEqualTo("LOT-P-001");
        }

        @Test
        @DisplayName("親ロットからロット構成を取得できる")
        void shouldFindCompositionsByParent() {
            // Arrange
            qualityService.registerLotComposition(
                    LotCompositionCreateCommand.builder()
                            .parentLotNumber("LOT-M-001")
                            .childLotNumber("LOT-P-001")
                            .usedQuantity(new BigDecimal("100"))
                            .build());
            qualityService.registerLotComposition(
                    LotCompositionCreateCommand.builder()
                            .parentLotNumber("LOT-M-001")
                            .childLotNumber("LOT-P-002")
                            .usedQuantity(new BigDecimal("50"))
                            .build());

            // Act
            List<LotComposition> compositions = qualityService.findLotCompositionsByParent("LOT-M-001");

            // Assert
            assertThat(compositions).hasSize(2);
        }

        @Test
        @DisplayName("子ロットからロット構成を取得できる（逆方向トレース）")
        void shouldFindCompositionsByChild() {
            // Arrange
            qualityService.registerLotComposition(
                    LotCompositionCreateCommand.builder()
                            .parentLotNumber("LOT-M-001")
                            .childLotNumber("LOT-P-001")
                            .usedQuantity(new BigDecimal("100"))
                            .build());

            // Act
            List<LotComposition> compositions = qualityService.findLotCompositionsByChild("LOT-P-001");

            // Assert
            assertThat(compositions).hasSize(1);
            assertThat(compositions.get(0).getParentLotNumber()).isEqualTo("LOT-M-001");
        }

        @Test
        @DisplayName("順方向トレースができる")
        void shouldTraceForward() {
            // Arrange
            qualityService.registerLotComposition(
                    LotCompositionCreateCommand.builder()
                            .parentLotNumber("LOT-M-001")
                            .childLotNumber("LOT-P-001")
                            .usedQuantity(new BigDecimal("100"))
                            .build());
            qualityService.registerLotComposition(
                    LotCompositionCreateCommand.builder()
                            .parentLotNumber("LOT-M-001")
                            .childLotNumber("LOT-P-002")
                            .usedQuantity(new BigDecimal("50"))
                            .build());

            // Act
            List<LotMaster> childLots = qualityService.traceForward("LOT-M-001");

            // Assert
            assertThat(childLots).hasSize(2);
        }

        @Test
        @DisplayName("逆方向トレースができる")
        void shouldTraceBackward() {
            // Arrange
            qualityService.registerLotComposition(
                    LotCompositionCreateCommand.builder()
                            .parentLotNumber("LOT-M-001")
                            .childLotNumber("LOT-P-001")
                            .usedQuantity(new BigDecimal("100"))
                            .build());

            // Act
            List<LotMaster> parentLots = qualityService.traceBackward("LOT-P-001");

            // Assert
            assertThat(parentLots).hasSize(1);
            assertThat(parentLots.get(0).getLotNumber()).isEqualTo("LOT-M-001");
        }
    }
}
