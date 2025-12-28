package com.example.production.infrastructure.in.seed;

import com.example.production.application.port.out.*;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.domain.model.process.*;
import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderDetail;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * トランザクションデータ投入
 *
 * E 精密工業株式会社のトランザクションデータを投入する
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class TransactionDataSeeder {

    private final StockRepository stockRepository;
    private final OrderRepository orderRepository;
    private final PurchaseOrderRepository purchaseOrderRepository;
    private final PurchaseOrderDetailRepository purchaseOrderDetailRepository;
    private final WorkOrderRepository workOrderRepository;
    private final WorkOrderDetailRepository workOrderDetailRepository;
    private final CompletionResultRepository completionResultRepository;
    private final LaborHoursRepository laborHoursRepository;

    /**
     * すべてのトランザクションデータを投入する
     */
    public void seedAll(LocalDate effectiveDate) {
        seedStocks();
        seedOrders(effectiveDate);
        seedPurchaseOrders(effectiveDate);
        seedWorkOrders(effectiveDate);
        seedCompletionResults(effectiveDate);
        seedLaborHours(effectiveDate);
    }

    /**
     * すべてのトランザクションデータを削除する
     */
    public void deleteAll() {
        laborHoursRepository.deleteAll();
        completionResultRepository.deleteAll();
        workOrderDetailRepository.deleteAll();
        workOrderRepository.deleteAll();
        purchaseOrderDetailRepository.deleteAll();
        purchaseOrderRepository.deleteAll();
        orderRepository.deleteAll();
        stockRepository.deleteAll();
    }

    private void seedStocks() {
        log.info("在庫情報を投入中...");

        List<Stock> stocks = List.of(
                // 材料倉庫
                Stock.builder().locationCode("WH-MAT").itemCode("MAT-001")
                        .stockQuantity(new BigDecimal("800"))
                        .passedQuantity(new BigDecimal("800"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-MAT").itemCode("MAT-002")
                        .stockQuantity(new BigDecimal("150"))
                        .passedQuantity(new BigDecimal("150"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-MAT").itemCode("MAT-003")
                        .stockQuantity(new BigDecimal("450"))
                        .passedQuantity(new BigDecimal("450"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-MAT").itemCode("MAT-004")
                        .stockQuantity(new BigDecimal("300"))
                        .passedQuantity(new BigDecimal("300"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-MAT").itemCode("MAT-010")
                        .stockQuantity(new BigDecimal("600"))
                        .passedQuantity(new BigDecimal("600"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),

                // 部品倉庫
                Stock.builder().locationCode("WH-PART").itemCode("PART-001")
                        .stockQuantity(new BigDecimal("200"))
                        .passedQuantity(new BigDecimal("200"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-PART").itemCode("PART-002")
                        .stockQuantity(new BigDecimal("150"))
                        .passedQuantity(new BigDecimal("150"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-PART").itemCode("PART-003")
                        .stockQuantity(new BigDecimal("80"))
                        .passedQuantity(new BigDecimal("80"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-PART").itemCode("PART-004")
                        .stockQuantity(new BigDecimal("120"))
                        .passedQuantity(new BigDecimal("120"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-PART").itemCode("PART-005")
                        .stockQuantity(new BigDecimal("300"))
                        .passedQuantity(new BigDecimal("300"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),

                // 半製品在庫
                Stock.builder().locationCode("WH-PART").itemCode("SEMI-A001")
                        .stockQuantity(new BigDecimal("50"))
                        .passedQuantity(new BigDecimal("50"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-PART").itemCode("SEMI-B001")
                        .stockQuantity(new BigDecimal("30"))
                        .passedQuantity(new BigDecimal("30"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-PART").itemCode("SEMI-B002")
                        .stockQuantity(new BigDecimal("40"))
                        .passedQuantity(new BigDecimal("40"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-PART").itemCode("SEMI-B003")
                        .stockQuantity(new BigDecimal("40"))
                        .passedQuantity(new BigDecimal("40"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),

                // 製品倉庫
                Stock.builder().locationCode("WH-PROD").itemCode("PROD-A001")
                        .stockQuantity(new BigDecimal("80"))
                        .passedQuantity(new BigDecimal("80"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-PROD").itemCode("PROD-B001")
                        .stockQuantity(new BigDecimal("45"))
                        .passedQuantity(new BigDecimal("45"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build(),
                Stock.builder().locationCode("WH-PROD").itemCode("PROD-C001")
                        .stockQuantity(new BigDecimal("60"))
                        .passedQuantity(new BigDecimal("60"))
                        .defectiveQuantity(BigDecimal.ZERO)
                        .uninspectedQuantity(BigDecimal.ZERO).build()
        );

        stocks.forEach(stockRepository::save);
        log.info("在庫情報 {}件 投入完了", stocks.size());
    }

    private void seedOrders(LocalDate effectiveDate) {
        log.info("オーダ情報を投入中...");

        List<Order> orders = List.of(
                // 製造オーダ
                Order.builder().orderNumber("MO-2025-001").orderType(OrderType.MANUFACTURING)
                        .itemCode("PROD-A001").startDate(effectiveDate)
                        .dueDate(LocalDate.of(2025, 1, 31)).planQuantity(new BigDecimal("100"))
                        .locationCode("LINE-1").status(PlanStatus.CONFIRMED).build(),
                Order.builder().orderNumber("MO-2025-002").orderType(OrderType.MANUFACTURING)
                        .itemCode("PROD-A001").startDate(effectiveDate)
                        .dueDate(LocalDate.of(2025, 2, 7)).planQuantity(new BigDecimal("100"))
                        .locationCode("LINE-1").status(PlanStatus.DRAFT).build(),
                Order.builder().orderNumber("MO-2025-003").orderType(OrderType.MANUFACTURING)
                        .itemCode("PROD-B001").startDate(effectiveDate)
                        .dueDate(LocalDate.of(2025, 1, 31)).planQuantity(new BigDecimal("50"))
                        .locationCode("LINE-2").status(PlanStatus.CONFIRMED).build(),
                Order.builder().orderNumber("MO-2025-004").orderType(OrderType.MANUFACTURING)
                        .itemCode("PROD-B001").startDate(effectiveDate)
                        .dueDate(LocalDate.of(2025, 2, 7)).planQuantity(new BigDecimal("50"))
                        .locationCode("LINE-2").status(PlanStatus.DRAFT).build(),
                Order.builder().orderNumber("MO-2025-005").orderType(OrderType.MANUFACTURING)
                        .itemCode("SEMI-A001").startDate(effectiveDate)
                        .dueDate(LocalDate.of(2025, 1, 28)).planQuantity(new BigDecimal("120"))
                        .locationCode("LINE-1").status(PlanStatus.CONFIRMED).build(),
                Order.builder().orderNumber("MO-2025-006").orderType(OrderType.MANUFACTURING)
                        .itemCode("SEMI-B002").startDate(effectiveDate)
                        .dueDate(LocalDate.of(2025, 1, 25)).planQuantity(new BigDecimal("60"))
                        .locationCode("LINE-2").status(PlanStatus.EXPANDED).build(),

                // 購買オーダ
                Order.builder().orderNumber("PO-2025-001").orderType(OrderType.PURCHASE)
                        .itemCode("MAT-001").startDate(effectiveDate)
                        .dueDate(LocalDate.of(2025, 1, 20)).planQuantity(new BigDecimal("200"))
                        .locationCode("WH-MAT").status(PlanStatus.CONFIRMED).build(),
                Order.builder().orderNumber("PO-2025-002").orderType(OrderType.PURCHASE)
                        .itemCode("MAT-003").startDate(effectiveDate)
                        .dueDate(LocalDate.of(2025, 1, 22)).planQuantity(new BigDecimal("150"))
                        .locationCode("WH-MAT").status(PlanStatus.CONFIRMED).build(),
                Order.builder().orderNumber("PO-2025-003").orderType(OrderType.PURCHASE)
                        .itemCode("PART-001").startDate(effectiveDate)
                        .dueDate(LocalDate.of(2025, 1, 25)).planQuantity(new BigDecimal("100"))
                        .locationCode("WH-PART").status(PlanStatus.DRAFT).build()
        );

        orders.forEach(orderRepository::save);
        log.info("オーダ情報 {}件 投入完了", orders.size());
    }

    private void seedPurchaseOrders(LocalDate effectiveDate) {
        log.info("発注データを投入中...");

        // 発注1
        PurchaseOrder po1 = PurchaseOrder.builder()
                .purchaseOrderNumber("PUR-2025-001")
                .orderDate(LocalDate.of(2025, 1, 10))
                .supplierCode("SUP-001")
                .ordererCode("EMP-009")
                .departmentCode("PURCHASE")
                .status(PurchaseOrderStatus.ORDERED)
                .build();
        purchaseOrderRepository.save(po1);

        purchaseOrderDetailRepository.save(PurchaseOrderDetail.builder()
                .purchaseOrderNumber("PUR-2025-001")
                .lineNumber(1)
                .orderNumber("PO-2025-001")
                .itemCode("MAT-001")
                .deliveryLocationCode("WH-MAT")
                .expectedReceivingDate(LocalDate.of(2025, 1, 20))
                .orderQuantity(new BigDecimal("200"))
                .orderUnitPrice(new BigDecimal("1500"))
                .orderAmount(new BigDecimal("300000"))
                .build());

        // 発注2
        PurchaseOrder po2 = PurchaseOrder.builder()
                .purchaseOrderNumber("PUR-2025-002")
                .orderDate(LocalDate.of(2025, 1, 10))
                .supplierCode("SUP-002")
                .ordererCode("EMP-009")
                .departmentCode("PURCHASE")
                .status(PurchaseOrderStatus.ORDERED)
                .build();
        purchaseOrderRepository.save(po2);

        purchaseOrderDetailRepository.save(PurchaseOrderDetail.builder()
                .purchaseOrderNumber("PUR-2025-002")
                .lineNumber(1)
                .orderNumber("PO-2025-002")
                .itemCode("MAT-003")
                .deliveryLocationCode("WH-MAT")
                .expectedReceivingDate(LocalDate.of(2025, 1, 22))
                .orderQuantity(new BigDecimal("150"))
                .orderUnitPrice(new BigDecimal("2000"))
                .orderAmount(new BigDecimal("300000"))
                .build());

        // 発注3
        PurchaseOrder po3 = PurchaseOrder.builder()
                .purchaseOrderNumber("PUR-2025-003")
                .orderDate(LocalDate.of(2025, 1, 12))
                .supplierCode("SUP-004")
                .ordererCode("EMP-009")
                .departmentCode("PURCHASE")
                .status(PurchaseOrderStatus.ORDERED)
                .build();
        purchaseOrderRepository.save(po3);

        purchaseOrderDetailRepository.save(PurchaseOrderDetail.builder()
                .purchaseOrderNumber("PUR-2025-003")
                .lineNumber(1)
                .orderNumber("PO-2025-003")
                .itemCode("PART-001")
                .deliveryLocationCode("WH-PART")
                .expectedReceivingDate(LocalDate.of(2025, 1, 25))
                .orderQuantity(new BigDecimal("100"))
                .orderUnitPrice(new BigDecimal("450"))
                .orderAmount(new BigDecimal("45000"))
                .build());
        purchaseOrderDetailRepository.save(PurchaseOrderDetail.builder()
                .purchaseOrderNumber("PUR-2025-003")
                .lineNumber(2)
                .itemCode("PART-002")
                .deliveryLocationCode("WH-PART")
                .expectedReceivingDate(LocalDate.of(2025, 1, 25))
                .orderQuantity(new BigDecimal("100"))
                .orderUnitPrice(new BigDecimal("80"))
                .orderAmount(new BigDecimal("8000"))
                .build());

        log.info("発注データ 3件 投入完了");
    }

    private void seedWorkOrders(LocalDate effectiveDate) {
        log.info("作業指示データを投入中...");

        // 作業指示1 - 進行中
        WorkOrder wo1 = WorkOrder.builder()
                .workOrderNumber("WO-2025-001")
                .orderNumber("MO-2025-001")
                .workOrderDate(LocalDate.of(2025, 1, 15))
                .itemCode("PROD-A001")
                .orderQuantity(new BigDecimal("100"))
                .locationCode("LINE-1")
                .plannedStartDate(LocalDate.of(2025, 1, 20))
                .plannedEndDate(LocalDate.of(2025, 1, 25))
                .actualStartDate(LocalDate.of(2025, 1, 20))
                .status(WorkOrderStatus.IN_PROGRESS)
                .build();
        workOrderRepository.save(wo1);

        workOrderDetailRepository.save(WorkOrderDetail.builder()
                .workOrderNumber("WO-2025-001").sequence(1).processCode("ASM").build());
        workOrderDetailRepository.save(WorkOrderDetail.builder()
                .workOrderNumber("WO-2025-001").sequence(2).processCode("INS-SHIP").build());

        // 作業指示2 - 完了
        WorkOrder wo2 = WorkOrder.builder()
                .workOrderNumber("WO-2025-002")
                .orderNumber("MO-2025-005")
                .workOrderDate(LocalDate.of(2025, 1, 12))
                .itemCode("SEMI-A001")
                .orderQuantity(new BigDecimal("120"))
                .locationCode("LINE-1")
                .plannedStartDate(LocalDate.of(2025, 1, 15))
                .plannedEndDate(LocalDate.of(2025, 1, 23))
                .actualStartDate(LocalDate.of(2025, 1, 15))
                .actualEndDate(LocalDate.of(2025, 1, 23))
                .completedQuantity(new BigDecimal("118"))
                .totalGoodQuantity(new BigDecimal("115"))
                .totalDefectQuantity(new BigDecimal("3"))
                .status(WorkOrderStatus.COMPLETED)
                .completedFlag(true)
                .build();
        workOrderRepository.save(wo2);

        workOrderDetailRepository.save(WorkOrderDetail.builder()
                .workOrderNumber("WO-2025-002").sequence(1).processCode("LATHE").build());
        workOrderDetailRepository.save(WorkOrderDetail.builder()
                .workOrderNumber("WO-2025-002").sequence(2).processCode("GRIND").build());
        workOrderDetailRepository.save(WorkOrderDetail.builder()
                .workOrderNumber("WO-2025-002").sequence(3).processCode("OUT-MEKI").build());
        workOrderDetailRepository.save(WorkOrderDetail.builder()
                .workOrderNumber("WO-2025-002").sequence(4).processCode("INS-PROC").build());

        log.info("作業指示データ 2件 投入完了");
    }

    private void seedCompletionResults(LocalDate effectiveDate) {
        log.info("完成実績データを投入中...");

        List<CompletionResult> results = List.of(
                CompletionResult.builder()
                        .completionResultNumber("CR-2025-001")
                        .workOrderNumber("WO-2025-002")
                        .itemCode("SEMI-A001")
                        .completionDate(LocalDate.of(2025, 1, 16))
                        .completedQuantity(new BigDecimal("118"))
                        .goodQuantity(new BigDecimal("116"))
                        .defectQuantity(new BigDecimal("2"))
                        .createdBy("EMP-001")
                        .build(),
                CompletionResult.builder()
                        .completionResultNumber("CR-2025-002")
                        .workOrderNumber("WO-2025-002")
                        .itemCode("SEMI-A001")
                        .completionDate(LocalDate.of(2025, 1, 17))
                        .completedQuantity(new BigDecimal("116"))
                        .goodQuantity(new BigDecimal("115"))
                        .defectQuantity(new BigDecimal("1"))
                        .createdBy("EMP-002")
                        .build()
        );

        results.forEach(completionResultRepository::save);
        log.info("完成実績データ {}件 投入完了", results.size());
    }

    private void seedLaborHours(LocalDate effectiveDate) {
        log.info("工数実績データを投入中...");

        List<LaborHours> laborHours = List.of(
                LaborHours.builder()
                        .laborHoursNumber("LH-2025-001")
                        .workOrderNumber("WO-2025-002")
                        .itemCode("SEMI-A001")
                        .sequence(1)
                        .processCode("LATHE")
                        .departmentCode("MFG")
                        .employeeCode("EMP-001")
                        .workDate(LocalDate.of(2025, 1, 15))
                        .hours(new BigDecimal("8.0"))
                        .build(),
                LaborHours.builder()
                        .laborHoursNumber("LH-2025-002")
                        .workOrderNumber("WO-2025-002")
                        .itemCode("SEMI-A001")
                        .sequence(1)
                        .processCode("LATHE")
                        .departmentCode("MFG")
                        .employeeCode("EMP-001")
                        .workDate(LocalDate.of(2025, 1, 16))
                        .hours(new BigDecimal("6.0"))
                        .build(),
                LaborHours.builder()
                        .laborHoursNumber("LH-2025-003")
                        .workOrderNumber("WO-2025-002")
                        .itemCode("SEMI-A001")
                        .sequence(2)
                        .processCode("GRIND")
                        .departmentCode("MFG")
                        .employeeCode("EMP-002")
                        .workDate(LocalDate.of(2025, 1, 17))
                        .hours(new BigDecimal("7.5"))
                        .build()
        );

        laborHours.forEach(laborHoursRepository::save);
        log.info("工数実績データ {}件 投入完了", laborHours.size());
    }
}
