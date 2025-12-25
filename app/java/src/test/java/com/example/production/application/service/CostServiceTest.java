package com.example.production.application.service;

import com.example.production.application.port.in.command.ActualCostCalculateCommand;
import com.example.production.application.port.in.command.StandardCostCreateCommand;
import com.example.production.application.port.out.ActualCostRepository;
import com.example.production.application.port.out.CostVarianceRepository;
import com.example.production.application.port.out.StandardCostRepository;
import com.example.production.domain.model.cost.ActualCost;
import com.example.production.domain.model.cost.CostVariance;
import com.example.production.domain.model.cost.StandardCost;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

@DisplayName("製造原価管理サービス")
class CostServiceTest extends BaseIntegrationTest {

    @Autowired
    private CostService costService;

    @Autowired
    private StandardCostRepository standardCostRepository;

    @Autowired
    private ActualCostRepository actualCostRepository;

    @Autowired
    private CostVarianceRepository costVarianceRepository;

    @BeforeEach
    void setUp() {
        // クリーンアップ（依存関係の順序に注意）
        costVarianceRepository.deleteAll();
        actualCostRepository.deleteAll();
        standardCostRepository.deleteAll();
    }

    @Nested
    @DisplayName("標準原価管理")
    class StandardCostTest {

        @Test
        @DisplayName("標準原価を登録できる")
        void shouldRegisterStandardCost() {
            // Act
            StandardCost standardCost = costService.registerStandardCost(
                    StandardCostCreateCommand.builder()
                            .itemCode("PROD-001")
                            .effectiveStartDate(LocalDate.of(2025, 1, 1))
                            .effectiveEndDate(LocalDate.of(2025, 12, 31))
                            .standardMaterialCost(new BigDecimal("1000.00"))
                            .standardLaborCost(new BigDecimal("500.00"))
                            .standardExpense(new BigDecimal("200.00"))
                            .build());

            // Assert
            assertThat(standardCost).isNotNull();
            assertThat(standardCost.getId()).isNotNull();
            assertThat(standardCost.getItemCode()).isEqualTo("PROD-001");
            assertThat(standardCost.getStandardMaterialCost()).isEqualByComparingTo("1000.00");
            assertThat(standardCost.getStandardLaborCost()).isEqualByComparingTo("500.00");
            assertThat(standardCost.getStandardExpense()).isEqualByComparingTo("200.00");
            assertThat(standardCost.getStandardManufacturingCost()).isEqualByComparingTo("1700.00");
        }

        @Test
        @DisplayName("品目コードと日付で標準原価を取得できる")
        void shouldFindStandardCostByItemCodeAndDate() {
            // Arrange
            costService.registerStandardCost(
                    StandardCostCreateCommand.builder()
                            .itemCode("PROD-001")
                            .effectiveStartDate(LocalDate.of(2025, 1, 1))
                            .effectiveEndDate(LocalDate.of(2025, 6, 30))
                            .standardMaterialCost(new BigDecimal("1000.00"))
                            .standardLaborCost(new BigDecimal("500.00"))
                            .standardExpense(new BigDecimal("200.00"))
                            .build());

            // Act
            StandardCost found = costService.findStandardCost("PROD-001", LocalDate.of(2025, 3, 15));

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getItemCode()).isEqualTo("PROD-001");
        }

        @Test
        @DisplayName("適用終了日がnullでも標準原価を取得できる")
        void shouldFindStandardCostWithNullEndDate() {
            // Arrange
            costService.registerStandardCost(
                    StandardCostCreateCommand.builder()
                            .itemCode("PROD-001")
                            .effectiveStartDate(LocalDate.of(2025, 1, 1))
                            .effectiveEndDate(null)
                            .standardMaterialCost(new BigDecimal("1000.00"))
                            .standardLaborCost(new BigDecimal("500.00"))
                            .standardExpense(new BigDecimal("200.00"))
                            .build());

            // Act
            StandardCost found = costService.findStandardCost("PROD-001", LocalDate.of(2025, 12, 31));

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getEffectiveEndDate()).isNull();
        }

        @Test
        @DisplayName("品目コードで標準原価を検索できる")
        void shouldFindStandardCostsByItemCode() {
            // Arrange
            costService.registerStandardCost(
                    StandardCostCreateCommand.builder()
                            .itemCode("PROD-001")
                            .effectiveStartDate(LocalDate.of(2025, 1, 1))
                            .effectiveEndDate(LocalDate.of(2025, 6, 30))
                            .standardMaterialCost(new BigDecimal("1000.00"))
                            .standardLaborCost(new BigDecimal("500.00"))
                            .standardExpense(new BigDecimal("200.00"))
                            .build());
            costService.registerStandardCost(
                    StandardCostCreateCommand.builder()
                            .itemCode("PROD-001")
                            .effectiveStartDate(LocalDate.of(2025, 7, 1))
                            .effectiveEndDate(LocalDate.of(2025, 12, 31))
                            .standardMaterialCost(new BigDecimal("1100.00"))
                            .standardLaborCost(new BigDecimal("550.00"))
                            .standardExpense(new BigDecimal("220.00"))
                            .build());

            // Act
            List<StandardCost> costs = costService.findStandardCostsByItemCode("PROD-001");

            // Assert
            assertThat(costs).hasSize(2);
        }

        @Test
        @DisplayName("全ての標準原価を取得できる")
        void shouldFindAllStandardCosts() {
            // Arrange
            costService.registerStandardCost(
                    StandardCostCreateCommand.builder()
                            .itemCode("PROD-001")
                            .effectiveStartDate(LocalDate.of(2025, 1, 1))
                            .standardMaterialCost(new BigDecimal("1000.00"))
                            .standardLaborCost(new BigDecimal("500.00"))
                            .standardExpense(new BigDecimal("200.00"))
                            .build());
            costService.registerStandardCost(
                    StandardCostCreateCommand.builder()
                            .itemCode("PROD-002")
                            .effectiveStartDate(LocalDate.of(2025, 1, 1))
                            .standardMaterialCost(new BigDecimal("2000.00"))
                            .standardLaborCost(new BigDecimal("800.00"))
                            .standardExpense(new BigDecimal("300.00"))
                            .build());

            // Act
            List<StandardCost> costs = costService.findAllStandardCosts();

            // Assert
            assertThat(costs).hasSize(2);
        }
    }

    @Nested
    @DisplayName("実際原価管理")
    class ActualCostTest {

        @Test
        @DisplayName("実際原価を計算して登録できる")
        void shouldCalculateAndRegisterActualCost() {
            // Act
            ActualCost actualCost = costService.calculateAndRegisterActualCost(
                    ActualCostCalculateCommand.builder()
                            .workOrderNumber("WO-2025-0001")
                            .itemCode("PROD-001")
                            .completedQuantity(new BigDecimal("100"))
                            .actualMaterialCost(new BigDecimal("105000.00"))
                            .actualLaborCost(new BigDecimal("52000.00"))
                            .actualExpense(new BigDecimal("21000.00"))
                            .build());

            // Assert
            assertThat(actualCost).isNotNull();
            assertThat(actualCost.getId()).isNotNull();
            assertThat(actualCost.getWorkOrderNumber()).isEqualTo("WO-2025-0001");
            assertThat(actualCost.getActualManufacturingCost()).isEqualByComparingTo("178000.00");
            assertThat(actualCost.getUnitCost()).isEqualByComparingTo("1780.0000");
        }

        @Test
        @DisplayName("作業指示番号で実際原価を取得できる")
        void shouldFindActualCostByWorkOrderNumber() {
            // Arrange
            costService.calculateAndRegisterActualCost(
                    ActualCostCalculateCommand.builder()
                            .workOrderNumber("WO-2025-0001")
                            .itemCode("PROD-001")
                            .completedQuantity(new BigDecimal("100"))
                            .actualMaterialCost(new BigDecimal("105000.00"))
                            .actualLaborCost(new BigDecimal("52000.00"))
                            .actualExpense(new BigDecimal("21000.00"))
                            .build());

            // Act
            ActualCost found = costService.findActualCost("WO-2025-0001");

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getWorkOrderNumber()).isEqualTo("WO-2025-0001");
        }

        @Test
        @DisplayName("品目コードで実際原価を検索できる")
        void shouldFindActualCostsByItemCode() {
            // Arrange
            costService.calculateAndRegisterActualCost(
                    ActualCostCalculateCommand.builder()
                            .workOrderNumber("WO-2025-0001")
                            .itemCode("PROD-001")
                            .completedQuantity(new BigDecimal("100"))
                            .actualMaterialCost(new BigDecimal("105000.00"))
                            .actualLaborCost(new BigDecimal("52000.00"))
                            .actualExpense(new BigDecimal("21000.00"))
                            .build());
            costService.calculateAndRegisterActualCost(
                    ActualCostCalculateCommand.builder()
                            .workOrderNumber("WO-2025-0002")
                            .itemCode("PROD-001")
                            .completedQuantity(new BigDecimal("50"))
                            .actualMaterialCost(new BigDecimal("50000.00"))
                            .actualLaborCost(new BigDecimal("25000.00"))
                            .actualExpense(new BigDecimal("10000.00"))
                            .build());

            // Act
            List<ActualCost> costs = costService.findActualCostsByItemCode("PROD-001");

            // Assert
            assertThat(costs).hasSize(2);
        }

        @Test
        @DisplayName("全ての実際原価を取得できる")
        void shouldFindAllActualCosts() {
            // Arrange
            costService.calculateAndRegisterActualCost(
                    ActualCostCalculateCommand.builder()
                            .workOrderNumber("WO-2025-0001")
                            .itemCode("PROD-001")
                            .completedQuantity(new BigDecimal("100"))
                            .actualMaterialCost(new BigDecimal("105000.00"))
                            .actualLaborCost(new BigDecimal("52000.00"))
                            .actualExpense(new BigDecimal("21000.00"))
                            .build());
            costService.calculateAndRegisterActualCost(
                    ActualCostCalculateCommand.builder()
                            .workOrderNumber("WO-2025-0002")
                            .itemCode("PROD-002")
                            .completedQuantity(new BigDecimal("200"))
                            .actualMaterialCost(new BigDecimal("420000.00"))
                            .actualLaborCost(new BigDecimal("165000.00"))
                            .actualExpense(new BigDecimal("62000.00"))
                            .build());

            // Act
            List<ActualCost> costs = costService.findAllActualCosts();

            // Assert
            assertThat(costs).hasSize(2);
        }
    }

    @Nested
    @DisplayName("原価差異分析")
    class CostVarianceTest {

        @BeforeEach
        void setUpCosts() {
            // 標準原価を登録（単位あたり）
            costService.registerStandardCost(
                    StandardCostCreateCommand.builder()
                            .itemCode("PROD-001")
                            .effectiveStartDate(LocalDate.of(2025, 1, 1))
                            .effectiveEndDate(LocalDate.of(2025, 12, 31))
                            .standardMaterialCost(new BigDecimal("1000.00"))
                            .standardLaborCost(new BigDecimal("500.00"))
                            .standardExpense(new BigDecimal("200.00"))
                            .build());

            // 実際原価を登録
            costService.calculateAndRegisterActualCost(
                    ActualCostCalculateCommand.builder()
                            .workOrderNumber("WO-2025-0001")
                            .itemCode("PROD-001")
                            .completedQuantity(new BigDecimal("100"))
                            .actualMaterialCost(new BigDecimal("105000.00"))
                            .actualLaborCost(new BigDecimal("52000.00"))
                            .actualExpense(new BigDecimal("21000.00"))
                            .build());
        }

        @Test
        @DisplayName("原価差異を計算して登録できる")
        void shouldCalculateAndRegisterCostVariance() {
            // Act
            CostVariance variance = costService.calculateAndRegisterCostVariance(
                    "WO-2025-0001", LocalDate.of(2025, 6, 15));

            // Assert
            assertThat(variance).isNotNull();
            assertThat(variance.getId()).isNotNull();
            assertThat(variance.getWorkOrderNumber()).isEqualTo("WO-2025-0001");
            assertThat(variance.getItemCode()).isEqualTo("PROD-001");
            // 材料費差異: 105000 - (1000 * 100) = 5000
            assertThat(variance.getMaterialCostVariance()).isEqualByComparingTo("5000.00");
            // 労務費差異: 52000 - (500 * 100) = 2000
            assertThat(variance.getLaborCostVariance()).isEqualByComparingTo("2000.00");
            // 経費差異: 21000 - (200 * 100) = 1000
            assertThat(variance.getExpenseVariance()).isEqualByComparingTo("1000.00");
            // 総差異: 5000 + 2000 + 1000 = 8000
            assertThat(variance.getTotalVariance()).isEqualByComparingTo("8000.00");
        }

        @Test
        @DisplayName("作業指示番号で原価差異を取得できる")
        void shouldFindCostVarianceByWorkOrderNumber() {
            // Arrange
            costService.calculateAndRegisterCostVariance("WO-2025-0001", LocalDate.of(2025, 6, 15));

            // Act
            CostVariance found = costService.findCostVariance("WO-2025-0001");

            // Assert
            assertThat(found).isNotNull();
            assertThat(found.getWorkOrderNumber()).isEqualTo("WO-2025-0001");
        }

        @Test
        @DisplayName("品目コードで原価差異を検索できる")
        void shouldFindCostVariancesByItemCode() {
            // Arrange
            costService.calculateAndRegisterCostVariance("WO-2025-0001", LocalDate.of(2025, 6, 15));

            // Act
            List<CostVariance> variances = costService.findCostVariancesByItemCode("PROD-001");

            // Assert
            assertThat(variances).hasSize(1);
            assertThat(variances.get(0).getItemCode()).isEqualTo("PROD-001");
        }

        @Test
        @DisplayName("全ての原価差異を取得できる")
        void shouldFindAllCostVariances() {
            // Arrange
            costService.calculateAndRegisterCostVariance("WO-2025-0001", LocalDate.of(2025, 6, 15));

            // 追加の標準原価と実際原価を登録
            costService.registerStandardCost(
                    StandardCostCreateCommand.builder()
                            .itemCode("PROD-002")
                            .effectiveStartDate(LocalDate.of(2025, 1, 1))
                            .standardMaterialCost(new BigDecimal("2000.00"))
                            .standardLaborCost(new BigDecimal("800.00"))
                            .standardExpense(new BigDecimal("300.00"))
                            .build());
            costService.calculateAndRegisterActualCost(
                    ActualCostCalculateCommand.builder()
                            .workOrderNumber("WO-2025-0002")
                            .itemCode("PROD-002")
                            .completedQuantity(new BigDecimal("50"))
                            .actualMaterialCost(new BigDecimal("95000.00"))
                            .actualLaborCost(new BigDecimal("38000.00"))
                            .actualExpense(new BigDecimal("14000.00"))
                            .build());
            costService.calculateAndRegisterCostVariance("WO-2025-0002", LocalDate.of(2025, 6, 15));

            // Act
            List<CostVariance> variances = costService.findAllCostVariances();

            // Assert
            assertThat(variances).hasSize(2);
        }

        @Test
        @DisplayName("有利差異を正しく計算できる（実際が標準より低い場合）")
        void shouldCalculateFavorableVariance() {
            // Arrange - 標準より低い実際原価
            costService.calculateAndRegisterActualCost(
                    ActualCostCalculateCommand.builder()
                            .workOrderNumber("WO-2025-0003")
                            .itemCode("PROD-001")
                            .completedQuantity(new BigDecimal("100"))
                            .actualMaterialCost(new BigDecimal("95000.00"))
                            .actualLaborCost(new BigDecimal("48000.00"))
                            .actualExpense(new BigDecimal("19000.00"))
                            .build());

            // Act
            CostVariance variance = costService.calculateAndRegisterCostVariance(
                    "WO-2025-0003", LocalDate.of(2025, 6, 15));

            // Assert
            // 材料費差異: 95000 - (1000 * 100) = -5000 (有利)
            assertThat(variance.getMaterialCostVariance()).isEqualByComparingTo("-5000.00");
            // 労務費差異: 48000 - (500 * 100) = -2000 (有利)
            assertThat(variance.getLaborCostVariance()).isEqualByComparingTo("-2000.00");
            // 経費差異: 19000 - (200 * 100) = -1000 (有利)
            assertThat(variance.getExpenseVariance()).isEqualByComparingTo("-1000.00");
            // 総差異: -5000 + -2000 + -1000 = -8000 (有利)
            assertThat(variance.getTotalVariance()).isEqualByComparingTo("-8000.00");
        }
    }
}
