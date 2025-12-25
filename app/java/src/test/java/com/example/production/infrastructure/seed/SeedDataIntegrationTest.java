package com.example.production.infrastructure.seed;

import com.example.production.application.port.out.*;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.jdbc.Sql;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Seed データ投入の統合テスト
 *
 * E 精密工業株式会社（架空）のテストデータが正しく投入されることを検証する
 */
@Sql(scripts = "/db/truncate-all.sql", executionPhase = Sql.ExecutionPhase.BEFORE_TEST_CLASS)
class SeedDataIntegrationTest extends BaseIntegrationTest {

    @Autowired
    private SeedDataService seedDataService;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private BomRepository bomRepository;

    @Autowired
    private SupplierRepository supplierRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private EmployeeRepository employeeRepository;

    @Autowired
    private ProcessRepository processRepository;

    @Autowired
    private LocationRepository locationRepository;

    @Autowired
    private WarehouseRepository warehouseRepository;

    @Autowired
    private RoutingRepository routingRepository;

    @Autowired
    private UnitPriceRepository unitPriceRepository;

    @Autowired
    private DefectMasterRepository defectMasterRepository;

    @Autowired
    private StockRepository stockRepository;

    @Autowired
    private OrderRepository orderRepository;

    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;

    @Autowired
    private WorkOrderRepository workOrderRepository;

    @Autowired
    private CompletionResultRepository completionResultRepository;

    @Autowired
    private LaborHoursRepository laborHoursRepository;

    @BeforeEach
    void setUp() {
        // テスト前にすべてのデータをクリア
        seedDataService.seedAll();
    }

    @Nested
    @DisplayName("マスタデータの投入")
    class MasterDataTests {

        @Test
        @DisplayName("部門マスタが正しく投入される")
        void shouldSeedDepartments() {
            var departments = departmentRepository.findAll();
            assertThat(departments).hasSize(7);
            assertThat(departments)
                    .extracting("departmentCode")
                    .contains("SALES", "PROD-PLAN", "MFG", "QUALITY", "PURCHASE", "WAREHOUSE", "OUTSOURCE");
        }

        @Test
        @DisplayName("仕入先マスタが正しく投入される")
        void shouldSeedSuppliers() {
            var suppliers = supplierRepository.findAll();
            assertThat(suppliers).hasSize(10);
            assertThat(suppliers)
                    .extracting("supplierCode")
                    .contains("SUP-001", "SUP-002", "SUP-003", "SUP-004", "SUP-005",
                            "OUT-001", "OUT-002", "CUS-001", "CUS-002", "CUS-003");
        }

        @Test
        @DisplayName("保管場所マスタが正しく投入される")
        void shouldSeedLocations() {
            var locations = locationRepository.findAll();
            assertThat(locations).hasSize(8);
            assertThat(locations)
                    .extracting("locationCode")
                    .contains("WH-MAT", "WH-PART", "WH-PROD", "LINE-1",
                            "LINE-2", "LINE-3", "INSP-1", "OUT-AREA");
        }

        @Test
        @DisplayName("倉庫マスタが正しく投入される")
        void shouldSeedWarehouses() {
            var warehouses = warehouseRepository.findAll();
            assertThat(warehouses).hasSize(3);
            assertThat(warehouses)
                    .extracting("warehouseCode")
                    .contains("WH-MAT", "WH-PART", "WH-PROD");
        }

        @Test
        @DisplayName("工程マスタが正しく投入される")
        void shouldSeedProcesses() {
            var processes = processRepository.findAll();
            assertThat(processes).hasSize(12);
            assertThat(processes)
                    .extracting("processCode")
                    .contains("LATHE", "MILL", "GRIND", "HOB", "DRILL", "ASM",
                            "FINAL-ASM", "INS-PROC", "INS-SHIP", "INS-RCV", "OUT-MEKI", "OUT-HEAT");
        }

        @Test
        @DisplayName("品目マスタが正しく投入される")
        void shouldSeedItems() {
            List<Item> items = itemRepository.findAll();
            assertThat(items).hasSize(18);

            // 製品（PROD-xxx）
            assertThat(items)
                    .filteredOn(item -> item.getItemCode().startsWith("PROD-"))
                    .hasSize(3);

            // 半製品（SEMI-xxx）
            assertThat(items)
                    .filteredOn(item -> item.getItemCode().startsWith("SEMI-"))
                    .hasSize(5);

            // 部品（PART-xxx）
            assertThat(items)
                    .filteredOn(item -> item.getItemCode().startsWith("PART-"))
                    .hasSize(5);

            // 材料（MAT-xxx）
            assertThat(items)
                    .filteredOn(item -> item.getItemCode().startsWith("MAT-"))
                    .hasSize(5);
        }

        @Test
        @DisplayName("BOM が正しく投入される")
        void shouldSeedBoms() {
            var boms = bomRepository.findAll();
            assertThat(boms).hasSize(19);
        }

        @Test
        @DisplayName("作業手順マスタが正しく投入される")
        void shouldSeedRoutings() {
            var routings = routingRepository.findAll();
            assertThat(routings).hasSize(24);
        }

        @Test
        @DisplayName("従業員マスタが正しく投入される")
        void shouldSeedEmployees() {
            var employees = employeeRepository.findAll();
            assertThat(employees).hasSize(12);
        }

        @Test
        @DisplayName("単価マスタが正しく投入される")
        void shouldSeedUnitPrices() {
            var unitPrices = unitPriceRepository.findAll();
            assertThat(unitPrices).hasSize(13);
        }

        @Test
        @DisplayName("不良マスタが正しく投入される")
        void shouldSeedDefectMasters() {
            var defectMasters = defectMasterRepository.findAll();
            assertThat(defectMasters).hasSize(6);
        }
    }

    @Nested
    @DisplayName("トランザクションデータの投入")
    class TransactionDataTests {

        @Test
        @DisplayName("在庫データが正しく投入される")
        void shouldSeedStocks() {
            List<Stock> stocks = stockRepository.findAll();
            assertThat(stocks).hasSize(17);

            // 全在庫が非負数であることを確認
            assertThat(stocks)
                    .allSatisfy(stock -> {
                        assertThat(stock.getStockQuantity()).isGreaterThanOrEqualTo(BigDecimal.ZERO);
                    });
        }

        @Test
        @DisplayName("受注データが正しく投入される")
        void shouldSeedOrders() {
            var orders = orderRepository.findAll();
            assertThat(orders).hasSize(9);
        }

        @Test
        @DisplayName("発注データが正しく投入される")
        void shouldSeedPurchaseOrders() {
            var purchaseOrders = purchaseOrderRepository.findAll();
            assertThat(purchaseOrders).hasSize(3);
        }

        @Test
        @DisplayName("作業指示データが正しく投入される")
        void shouldSeedWorkOrders() {
            var workOrders = workOrderRepository.findAll();
            assertThat(workOrders).hasSize(2);
        }

        @Test
        @DisplayName("完成実績データが正しく投入される")
        void shouldSeedCompletionResults() {
            var completionResults = completionResultRepository.findAll();
            assertThat(completionResults).hasSize(2);
        }

        @Test
        @DisplayName("作業時間データが正しく投入される")
        void shouldSeedLaborHours() {
            var laborHours = laborHoursRepository.findAll();
            assertThat(laborHours).hasSize(3);
        }
    }

    @Nested
    @DisplayName("データ整合性の検証")
    class DataIntegrityTests {

        @Test
        @DisplayName("BOM の親品目が存在する")
        void bomParentItemsShouldExist() {
            var boms = bomRepository.findAll();
            var itemCodes = itemRepository.findAll().stream()
                    .map(Item::getItemCode)
                    .toList();

            assertThat(boms)
                    .allSatisfy(bom -> {
                        assertThat(itemCodes).contains(bom.getParentItemCode());
                    });
        }

        @Test
        @DisplayName("BOM の子品目が存在する")
        void bomChildItemsShouldExist() {
            var boms = bomRepository.findAll();
            var itemCodes = itemRepository.findAll().stream()
                    .map(Item::getItemCode)
                    .toList();

            assertThat(boms)
                    .allSatisfy(bom -> {
                        assertThat(itemCodes).contains(bom.getChildItemCode());
                    });
        }

        @Test
        @DisplayName("在庫の保管場所が存在する")
        void stockLocationsShouldExist() {
            var stocks = stockRepository.findAll();
            var locationCodes = locationRepository.findAll().stream()
                    .map(loc -> loc.getLocationCode())
                    .toList();

            assertThat(stocks)
                    .allSatisfy(stock -> {
                        assertThat(locationCodes).contains(stock.getLocationCode());
                    });
        }

        @Test
        @DisplayName("在庫の品目が存在する")
        void stockItemsShouldExist() {
            var stocks = stockRepository.findAll();
            var itemCodes = itemRepository.findAll().stream()
                    .map(Item::getItemCode)
                    .toList();

            assertThat(stocks)
                    .allSatisfy(stock -> {
                        assertThat(itemCodes).contains(stock.getItemCode());
                    });
        }
    }

    @Nested
    @DisplayName("再実行時のデータ整合性")
    class RerunTests {

        @Test
        @DisplayName("seedAll を複数回実行しても正しいデータ件数が維持される")
        void shouldMaintainCorrectCountsAfterRerun() {
            // 初回は BeforeEach で実行済み
            int initialItemCount = itemRepository.findAll().size();
            int initialBomCount = bomRepository.findAll().size();
            int initialStockCount = stockRepository.findAll().size();

            // 2回目の実行
            seedDataService.seedAll();

            // データ件数が変わらないことを確認
            assertThat(itemRepository.findAll()).hasSize(initialItemCount);
            assertThat(bomRepository.findAll()).hasSize(initialBomCount);
            assertThat(stockRepository.findAll()).hasSize(initialStockCount);
        }
    }
}
