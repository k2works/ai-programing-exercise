package com.example.production.infrastructure.seed;

import com.example.production.application.port.out.*;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.inventory.Warehouse;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.item.Unit;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import com.example.production.domain.model.organization.Department;
import com.example.production.domain.model.organization.Employee;
import com.example.production.domain.model.process.Process;
import com.example.production.domain.model.process.Routing;
import com.example.production.domain.model.purchase.UnitPrice;
import com.example.production.domain.model.quality.DefectMaster;
import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * マスタデータ投入
 *
 * E 精密工業株式会社のマスタデータを投入する
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class MasterDataSeeder {

    private final DepartmentRepository departmentRepository;
    private final SupplierRepository supplierRepository;
    private final LocationRepository locationRepository;
    private final WarehouseRepository warehouseRepository;
    private final ProcessRepository processRepository;
    private final UnitRepository unitRepository;
    private final ItemRepository itemRepository;
    private final BomRepository bomRepository;
    private final RoutingRepository routingRepository;
    private final EmployeeRepository employeeRepository;
    private final UnitPriceRepository unitPriceRepository;
    private final DefectMasterRepository defectMasterRepository;

    /**
     * すべてのマスタデータを投入する
     */
    public void seedAll(LocalDate effectiveDate) {
        seedDepartments();
        seedSuppliers(effectiveDate);
        seedLocations();
        seedWarehouses();
        seedProcesses();
        seedUnits();
        seedItems(effectiveDate);
        seedBoms(effectiveDate);
        seedRoutings();
        seedEmployees();
        seedUnitPrices(effectiveDate);
        seedDefects();
    }

    /**
     * すべてのマスタデータを削除する
     */
    public void deleteAll() {
        defectMasterRepository.deleteAll();
        unitPriceRepository.deleteAll();
        routingRepository.deleteAll();
        bomRepository.deleteAll();
        employeeRepository.deleteAll();
        itemRepository.deleteAll();
        unitRepository.deleteAll();
        processRepository.deleteAll();
        warehouseRepository.deleteAll();
        locationRepository.deleteAll();
        supplierRepository.deleteAll();
        departmentRepository.deleteAll();
    }

    private void seedDepartments() {
        log.info("部門マスタを投入中...");

        List<Department> departments = List.of(
                Department.builder().departmentCode("SALES").departmentName("営業部").build(),
                Department.builder().departmentCode("PROD-PLAN").departmentName("生産管理部").build(),
                Department.builder().departmentCode("MFG").departmentName("製造部").build(),
                Department.builder().departmentCode("QUALITY").departmentName("品質管理部").build(),
                Department.builder().departmentCode("PURCHASE").departmentName("購買部").build(),
                Department.builder().departmentCode("WAREHOUSE").departmentName("倉庫部").build(),
                Department.builder().departmentCode("OUTSOURCE").departmentName("外注管理部").build()
        );

        departments.forEach(departmentRepository::save);
        log.info("部門マスタ {}件 投入完了", departments.size());
    }

    private void seedSuppliers(LocalDate effectiveDate) {
        log.info("取引先マスタを投入中...");

        List<Supplier> suppliers = List.of(
                // 仕入先
                Supplier.builder().supplierCode("SUP-001").effectiveFrom(effectiveDate)
                        .supplierName("東京スチール株式会社").supplierType(SupplierType.VENDOR).build(),
                Supplier.builder().supplierCode("SUP-002").effectiveFrom(effectiveDate)
                        .supplierName("大阪金属工業").supplierType(SupplierType.VENDOR).build(),
                Supplier.builder().supplierCode("SUP-003").effectiveFrom(effectiveDate)
                        .supplierName("名古屋ベアリング").supplierType(SupplierType.VENDOR).build(),
                Supplier.builder().supplierCode("SUP-004").effectiveFrom(effectiveDate)
                        .supplierName("横浜部品センター").supplierType(SupplierType.VENDOR).build(),
                Supplier.builder().supplierCode("SUP-005").effectiveFrom(effectiveDate)
                        .supplierName("神戸包装資材").supplierType(SupplierType.VENDOR).build(),
                // 外注先
                Supplier.builder().supplierCode("OUT-001").effectiveFrom(effectiveDate)
                        .supplierName("メッキ工業所").supplierType(SupplierType.SUBCONTRACTOR).build(),
                Supplier.builder().supplierCode("OUT-002").effectiveFrom(effectiveDate)
                        .supplierName("熱処理センター").supplierType(SupplierType.SUBCONTRACTOR).build(),
                // 得意先
                Supplier.builder().supplierCode("CUS-001").effectiveFrom(effectiveDate)
                        .supplierName("機械メーカーA社").supplierType(SupplierType.CUSTOMER).build(),
                Supplier.builder().supplierCode("CUS-002").effectiveFrom(effectiveDate)
                        .supplierName("産業機器B社").supplierType(SupplierType.CUSTOMER).build(),
                Supplier.builder().supplierCode("CUS-003").effectiveFrom(effectiveDate)
                        .supplierName("精密機械C社").supplierType(SupplierType.CUSTOMER).build()
        );

        suppliers.forEach(supplierRepository::save);
        log.info("取引先マスタ {}件 投入完了", suppliers.size());
    }

    private void seedLocations() {
        log.info("場所マスタを投入中...");

        List<Location> locations = List.of(
                Location.builder().locationCode("WH-MAT").locationName("材料倉庫")
                        .locationType(LocationType.WAREHOUSE).build(),
                Location.builder().locationCode("WH-PART").locationName("部品倉庫")
                        .locationType(LocationType.WAREHOUSE).build(),
                Location.builder().locationCode("WH-PROD").locationName("製品倉庫")
                        .locationType(LocationType.WAREHOUSE).build(),
                Location.builder().locationCode("LINE-1").locationName("製造ライン1")
                        .locationType(LocationType.MANUFACTURING).build(),
                Location.builder().locationCode("LINE-2").locationName("製造ライン2")
                        .locationType(LocationType.MANUFACTURING).build(),
                Location.builder().locationCode("LINE-3").locationName("組立ライン")
                        .locationType(LocationType.MANUFACTURING).build(),
                Location.builder().locationCode("INSP-1").locationName("検査場")
                        .locationType(LocationType.INSPECTION).build(),
                Location.builder().locationCode("OUT-AREA").locationName("外注エリア")
                        .locationType(LocationType.SUBCONTRACT).build()
        );

        locations.forEach(locationRepository::save);
        log.info("場所マスタ {}件 投入完了", locations.size());
    }

    private void seedWarehouses() {
        log.info("倉庫マスタを投入中...");

        List<Warehouse> warehouses = List.of(
                Warehouse.builder().warehouseCode("WH-MAT").warehouseCategory("M")
                        .warehouseName("材料倉庫").departmentCode("WAREHOUSE").build(),
                Warehouse.builder().warehouseCode("WH-PART").warehouseCategory("P")
                        .warehouseName("部品倉庫").departmentCode("WAREHOUSE").build(),
                Warehouse.builder().warehouseCode("WH-PROD").warehouseCategory("F")
                        .warehouseName("製品倉庫").departmentCode("WAREHOUSE").build()
        );

        warehouses.forEach(warehouseRepository::save);
        log.info("倉庫マスタ {}件 投入完了", warehouses.size());
    }

    private void seedProcesses() {
        log.info("工程マスタを投入中...");

        List<Process> processes = List.of(
                // 切削工程
                Process.builder().processCode("LATHE").processName("旋盤加工").build(),
                Process.builder().processCode("MILL").processName("フライス加工").build(),
                Process.builder().processCode("GRIND").processName("研削加工").build(),
                Process.builder().processCode("HOB").processName("ホブ切り").build(),
                Process.builder().processCode("DRILL").processName("穴あけ加工").build(),
                // 組立工程
                Process.builder().processCode("ASM").processName("組立").build(),
                Process.builder().processCode("FINAL-ASM").processName("最終組立").build(),
                // 検査工程
                Process.builder().processCode("INS-PROC").processName("工程検査").build(),
                Process.builder().processCode("INS-SHIP").processName("出荷検査").build(),
                Process.builder().processCode("INS-RCV").processName("受入検査").build(),
                // 外注工程
                Process.builder().processCode("OUT-MEKI").processName("メッキ処理").build(),
                Process.builder().processCode("OUT-HEAT").processName("熱処理").build()
        );

        processes.forEach(processRepository::save);
        log.info("工程マスタ {}件 投入完了", processes.size());
    }

    private void seedUnits() {
        log.info("単位マスタを投入中...");

        List<Unit> units = List.of(
                Unit.builder().unitCode("PCS").unitSymbol("個").unitName("個数").build(),
                Unit.builder().unitCode("KG").unitSymbol("kg").unitName("キログラム").build(),
                Unit.builder().unitCode("SET").unitSymbol("SET").unitName("セット").build(),
                Unit.builder().unitCode("M").unitSymbol("m").unitName("メートル").build(),
                Unit.builder().unitCode("L").unitSymbol("L").unitName("リットル").build()
        );

        units.forEach(unitRepository::save);
        log.info("単位マスタ {}件 投入完了", units.size());
    }

    private void seedItems(LocalDate effectiveDate) {
        log.info("品目マスタを投入中...");

        List<Item> items = List.of(
                // 製品
                Item.builder().itemCode("PROD-A001").effectiveFrom(effectiveDate)
                        .itemName("精密シャフトA").itemCategory(ItemCategory.PRODUCT)
                        .unitCode("PCS").leadTime(7).safetyStock(new BigDecimal("100")).build(),
                Item.builder().itemCode("PROD-B001").effectiveFrom(effectiveDate)
                        .itemName("ギアボックスアセンブリ").itemCategory(ItemCategory.PRODUCT)
                        .unitCode("PCS").leadTime(14).safetyStock(new BigDecimal("50")).build(),
                Item.builder().itemCode("PROD-C001").effectiveFrom(effectiveDate)
                        .itemName("精密プレート").itemCategory(ItemCategory.PRODUCT)
                        .unitCode("PCS").leadTime(5).safetyStock(new BigDecimal("80")).build(),

                // 半製品
                Item.builder().itemCode("SEMI-A001").effectiveFrom(effectiveDate)
                        .itemName("加工済みシャフト").itemCategory(ItemCategory.SEMI_PRODUCT)
                        .unitCode("PCS").leadTime(5).safetyStock(new BigDecimal("120")).build(),
                Item.builder().itemCode("SEMI-B001").effectiveFrom(effectiveDate)
                        .itemName("ギアボックス本体").itemCategory(ItemCategory.SEMI_PRODUCT)
                        .unitCode("PCS").leadTime(7).safetyStock(new BigDecimal("60")).build(),
                Item.builder().itemCode("SEMI-B002").effectiveFrom(effectiveDate)
                        .itemName("駆動ギア").itemCategory(ItemCategory.SEMI_PRODUCT)
                        .unitCode("PCS").leadTime(7).safetyStock(new BigDecimal("80")).build(),
                Item.builder().itemCode("SEMI-B003").effectiveFrom(effectiveDate)
                        .itemName("従動ギア").itemCategory(ItemCategory.SEMI_PRODUCT)
                        .unitCode("PCS").leadTime(7).safetyStock(new BigDecimal("80")).build(),
                Item.builder().itemCode("SEMI-C001").effectiveFrom(effectiveDate)
                        .itemName("加工済みプレート").itemCategory(ItemCategory.SEMI_PRODUCT)
                        .unitCode("PCS").leadTime(3).safetyStock(new BigDecimal("100")).build(),

                // 部品
                Item.builder().itemCode("PART-001").effectiveFrom(effectiveDate)
                        .itemName("ベアリング 6205").itemCategory(ItemCategory.PART)
                        .unitCode("PCS").leadTime(7).safetyStock(new BigDecimal("100")).build(),
                Item.builder().itemCode("PART-002").effectiveFrom(effectiveDate)
                        .itemName("オイルシール φ20").itemCategory(ItemCategory.PART)
                        .unitCode("PCS").leadTime(7).safetyStock(new BigDecimal("100")).build(),
                Item.builder().itemCode("PART-003").effectiveFrom(effectiveDate)
                        .itemName("標準シャフト φ10").itemCategory(ItemCategory.PART)
                        .unitCode("PCS").leadTime(7).safetyStock(new BigDecimal("50")).build(),
                Item.builder().itemCode("PART-004").effectiveFrom(effectiveDate)
                        .itemName("オイルシール φ30").itemCategory(ItemCategory.PART)
                        .unitCode("PCS").leadTime(7).safetyStock(new BigDecimal("80")).build(),
                Item.builder().itemCode("PART-005").effectiveFrom(effectiveDate)
                        .itemName("ボルトセット M6").itemCategory(ItemCategory.PART)
                        .unitCode("SET").leadTime(3).safetyStock(new BigDecimal("200")).build(),

                // 材料
                Item.builder().itemCode("MAT-001").effectiveFrom(effectiveDate)
                        .itemName("丸棒材 SUS304 φ20").itemCategory(ItemCategory.MATERIAL)
                        .unitCode("KG").leadTime(14).safetyStock(new BigDecimal("500")).build(),
                Item.builder().itemCode("MAT-002").effectiveFrom(effectiveDate)
                        .itemName("アルミダイキャスト素材").itemCategory(ItemCategory.MATERIAL)
                        .unitCode("PCS").leadTime(21).safetyStock(new BigDecimal("100")).build(),
                Item.builder().itemCode("MAT-003").effectiveFrom(effectiveDate)
                        .itemName("歯車用素材 SCM415").itemCategory(ItemCategory.MATERIAL)
                        .unitCode("KG").leadTime(14).safetyStock(new BigDecimal("300")).build(),
                Item.builder().itemCode("MAT-004").effectiveFrom(effectiveDate)
                        .itemName("鋼板 SS400 t3").itemCategory(ItemCategory.MATERIAL)
                        .unitCode("KG").leadTime(7).safetyStock(new BigDecimal("200")).build(),
                Item.builder().itemCode("MAT-010").effectiveFrom(effectiveDate)
                        .itemName("包装材セット").itemCategory(ItemCategory.MATERIAL)
                        .unitCode("SET").leadTime(3).safetyStock(new BigDecimal("500")).build()
        );

        items.forEach(itemRepository::save);
        log.info("品目マスタ {}件 投入完了", items.size());
    }

    private void seedBoms(LocalDate effectiveDate) {
        log.info("BOMを投入中...");

        List<Bom> boms = List.of(
                // 精密シャフトA の構成
                Bom.builder().parentItemCode("PROD-A001").childItemCode("SEMI-A001")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE)
                        .defectRate(new BigDecimal("0.02")).sequence(1).build(),
                Bom.builder().parentItemCode("PROD-A001").childItemCode("PART-001")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(2).build(),
                Bom.builder().parentItemCode("PROD-A001").childItemCode("PART-002")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(3).build(),
                Bom.builder().parentItemCode("PROD-A001").childItemCode("MAT-010")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(4).build(),

                // 加工済みシャフト の構成
                Bom.builder().parentItemCode("SEMI-A001").childItemCode("MAT-001")
                        .effectiveFrom(effectiveDate).requiredQuantity(new BigDecimal("0.5"))
                        .defectRate(new BigDecimal("0.05")).sequence(1).build(),

                // ギアボックスアセンブリ の構成
                Bom.builder().parentItemCode("PROD-B001").childItemCode("SEMI-B001")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(1).build(),
                Bom.builder().parentItemCode("PROD-B001").childItemCode("SEMI-B002")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(2).build(),
                Bom.builder().parentItemCode("PROD-B001").childItemCode("SEMI-B003")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(3).build(),
                Bom.builder().parentItemCode("PROD-B001").childItemCode("PART-003")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(4).build(),
                Bom.builder().parentItemCode("PROD-B001").childItemCode("PART-001")
                        .effectiveFrom(effectiveDate).requiredQuantity(new BigDecimal("2")).sequence(5).build(),
                Bom.builder().parentItemCode("PROD-B001").childItemCode("PART-004")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(6).build(),
                Bom.builder().parentItemCode("PROD-B001").childItemCode("PART-005")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(7).build(),
                Bom.builder().parentItemCode("PROD-B001").childItemCode("MAT-010")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(8).build(),

                // ギアボックス本体 の構成
                Bom.builder().parentItemCode("SEMI-B001").childItemCode("MAT-002")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE)
                        .defectRate(new BigDecimal("0.03")).sequence(1).build(),

                // 駆動ギア の構成
                Bom.builder().parentItemCode("SEMI-B002").childItemCode("MAT-003")
                        .effectiveFrom(effectiveDate).requiredQuantity(new BigDecimal("0.8"))
                        .defectRate(new BigDecimal("0.05")).sequence(1).build(),

                // 従動ギア の構成
                Bom.builder().parentItemCode("SEMI-B003").childItemCode("MAT-003")
                        .effectiveFrom(effectiveDate).requiredQuantity(new BigDecimal("0.6"))
                        .defectRate(new BigDecimal("0.05")).sequence(1).build(),

                // 精密プレート の構成
                Bom.builder().parentItemCode("PROD-C001").childItemCode("SEMI-C001")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(1).build(),
                Bom.builder().parentItemCode("PROD-C001").childItemCode("MAT-010")
                        .effectiveFrom(effectiveDate).requiredQuantity(BigDecimal.ONE).sequence(2).build(),

                // 加工済みプレート の構成
                Bom.builder().parentItemCode("SEMI-C001").childItemCode("MAT-004")
                        .effectiveFrom(effectiveDate).requiredQuantity(new BigDecimal("2.5"))
                        .defectRate(new BigDecimal("0.03")).sequence(1).build()
        );

        boms.forEach(bomRepository::save);
        log.info("BOM {}件 投入完了", boms.size());
    }

    private void seedRoutings() {
        log.info("工程表を投入中...");

        List<Routing> routings = List.of(
                // 精密シャフトA
                Routing.builder().itemCode("PROD-A001").processCode("ASM").sequence(1).build(),
                Routing.builder().itemCode("PROD-A001").processCode("INS-SHIP").sequence(2).build(),

                // 加工済みシャフト
                Routing.builder().itemCode("SEMI-A001").processCode("LATHE").sequence(1).build(),
                Routing.builder().itemCode("SEMI-A001").processCode("GRIND").sequence(2).build(),
                Routing.builder().itemCode("SEMI-A001").processCode("OUT-MEKI").sequence(3).build(),
                Routing.builder().itemCode("SEMI-A001").processCode("INS-PROC").sequence(4).build(),

                // ギアボックスアセンブリ
                Routing.builder().itemCode("PROD-B001").processCode("FINAL-ASM").sequence(1).build(),
                Routing.builder().itemCode("PROD-B001").processCode("INS-SHIP").sequence(2).build(),

                // ギアボックス本体
                Routing.builder().itemCode("SEMI-B001").processCode("MILL").sequence(1).build(),
                Routing.builder().itemCode("SEMI-B001").processCode("DRILL").sequence(2).build(),
                Routing.builder().itemCode("SEMI-B001").processCode("INS-PROC").sequence(3).build(),

                // 駆動ギア
                Routing.builder().itemCode("SEMI-B002").processCode("LATHE").sequence(1).build(),
                Routing.builder().itemCode("SEMI-B002").processCode("HOB").sequence(2).build(),
                Routing.builder().itemCode("SEMI-B002").processCode("OUT-HEAT").sequence(3).build(),
                Routing.builder().itemCode("SEMI-B002").processCode("INS-PROC").sequence(4).build(),

                // 従動ギア
                Routing.builder().itemCode("SEMI-B003").processCode("LATHE").sequence(1).build(),
                Routing.builder().itemCode("SEMI-B003").processCode("HOB").sequence(2).build(),
                Routing.builder().itemCode("SEMI-B003").processCode("OUT-HEAT").sequence(3).build(),
                Routing.builder().itemCode("SEMI-B003").processCode("INS-PROC").sequence(4).build(),

                // 精密プレート
                Routing.builder().itemCode("PROD-C001").processCode("ASM").sequence(1).build(),
                Routing.builder().itemCode("PROD-C001").processCode("INS-SHIP").sequence(2).build(),

                // 加工済みプレート
                Routing.builder().itemCode("SEMI-C001").processCode("MILL").sequence(1).build(),
                Routing.builder().itemCode("SEMI-C001").processCode("DRILL").sequence(2).build(),
                Routing.builder().itemCode("SEMI-C001").processCode("INS-PROC").sequence(3).build()
        );

        routings.forEach(routingRepository::save);
        log.info("工程表 {}件 投入完了", routings.size());
    }

    private void seedEmployees() {
        log.info("担当者マスタを投入中...");

        List<Employee> employees = List.of(
                Employee.builder().employeeCode("EMP-001").employeeName("田中 太郎").departmentCode("MFG").build(),
                Employee.builder().employeeCode("EMP-002").employeeName("鈴木 一郎").departmentCode("MFG").build(),
                Employee.builder().employeeCode("EMP-003").employeeName("佐藤 次郎").departmentCode("MFG").build(),
                Employee.builder().employeeCode("EMP-004").employeeName("高橋 三郎").departmentCode("MFG").build(),
                Employee.builder().employeeCode("EMP-005").employeeName("伊藤 四郎").departmentCode("MFG").build(),
                Employee.builder().employeeCode("EMP-006").employeeName("渡辺 五郎").departmentCode("QUALITY").build(),
                Employee.builder().employeeCode("EMP-007").employeeName("山本 花子").departmentCode("QUALITY").build(),
                Employee.builder().employeeCode("EMP-008").employeeName("中村 美咲").departmentCode("PROD-PLAN").build(),
                Employee.builder().employeeCode("EMP-009").employeeName("小林 健一").departmentCode("PURCHASE").build(),
                Employee.builder().employeeCode("EMP-010").employeeName("加藤 正").departmentCode("WAREHOUSE").build(),
                Employee.builder().employeeCode("EMP-011").employeeName("吉田 誠").departmentCode("OUTSOURCE").build(),
                Employee.builder().employeeCode("EMP-012").employeeName("山田 浩二").departmentCode("SALES").build()
        );

        employees.forEach(employeeRepository::save);
        log.info("担当者マスタ {}件 投入完了", employees.size());
    }

    private void seedUnitPrices(LocalDate effectiveDate) {
        log.info("単価マスタを投入中...");

        List<UnitPrice> unitPrices = List.of(
                // 材料の仕入単価
                UnitPrice.builder().itemCode("MAT-001").supplierCode("SUP-001")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("1500")).build(),
                UnitPrice.builder().itemCode("MAT-002").supplierCode("SUP-002")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("3500")).build(),
                UnitPrice.builder().itemCode("MAT-003").supplierCode("SUP-002")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("2000")).build(),
                UnitPrice.builder().itemCode("MAT-004").supplierCode("SUP-001")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("800")).build(),

                // 部品の仕入単価
                UnitPrice.builder().itemCode("PART-001").supplierCode("SUP-004")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("450")).build(),
                UnitPrice.builder().itemCode("PART-002").supplierCode("SUP-003")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("80")).build(),
                UnitPrice.builder().itemCode("PART-003").supplierCode("SUP-003")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("320")).build(),
                UnitPrice.builder().itemCode("PART-004").supplierCode("SUP-003")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("120")).build(),
                UnitPrice.builder().itemCode("PART-005").supplierCode("SUP-003")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("250")).build(),

                // 包装材の仕入単価
                UnitPrice.builder().itemCode("MAT-010").supplierCode("SUP-005")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("150")).build(),

                // 外注加工単価
                UnitPrice.builder().itemCode("SEMI-A001").supplierCode("OUT-001")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("500")).build(),
                UnitPrice.builder().itemCode("SEMI-B002").supplierCode("OUT-002")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("800")).build(),
                UnitPrice.builder().itemCode("SEMI-B003").supplierCode("OUT-002")
                        .effectiveFrom(effectiveDate).unitPrice(new BigDecimal("700")).build()
        );

        unitPrices.forEach(unitPriceRepository::save);
        log.info("単価マスタ {}件 投入完了", unitPrices.size());
    }

    private void seedDefects() {
        log.info("欠点マスタを投入中...");

        List<DefectMaster> defects = List.of(
                DefectMaster.builder().defectCode("DEF-001").defectDescription("寸法不良").build(),
                DefectMaster.builder().defectCode("DEF-002").defectDescription("表面傷").build(),
                DefectMaster.builder().defectCode("DEF-003").defectDescription("メッキ不良").build(),
                DefectMaster.builder().defectCode("DEF-004").defectDescription("熱処理不良").build(),
                DefectMaster.builder().defectCode("DEF-005").defectDescription("組立不良").build(),
                DefectMaster.builder().defectCode("DEF-006").defectDescription("材料不良").build()
        );

        defects.forEach(defectMasterRepository::save);
        log.info("欠点マスタ {}件 投入完了", defects.size());
    }
}
