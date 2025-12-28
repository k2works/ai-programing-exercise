package com.example.production.infrastructure.in.web.controller;

import com.example.production.application.port.in.InventoryUseCase;
import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.in.WorkOrderUseCase;
import com.example.production.application.port.out.OrderRepository;
import com.example.production.application.service.ReportService;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.process.WorkOrder;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.time.LocalDate;
import java.util.List;

/**
 * 帳票出力 Controller（モノリス版）
 */
@Controller
@RequestMapping("/reports")
@RequiredArgsConstructor
public class ReportController {

    private final ReportService reportService;
    private final InventoryUseCase inventoryUseCase;
    private final ItemUseCase itemUseCase;
    private final OrderRepository orderRepository;
    private final WorkOrderUseCase workOrderUseCase;

    /**
     * 帳票出力画面
     */
    @GetMapping
    public String index(Model model) {
        return "reports/index";
    }

    /**
     * 在庫一覧 Excel 出力
     */
    @GetMapping("/inventory/excel")
    public ResponseEntity<Resource> exportInventoryExcel(
            @RequestParam(required = false) String locationCode) {

        List<Stock> stocks;
        if (locationCode != null && !locationCode.isBlank()) {
            stocks = inventoryUseCase.getStocksByLocation(locationCode);
        } else {
            stocks = inventoryUseCase.getAllStocks();
        }

        Resource resource = reportService.exportInventoryToExcel(stocks);
        String filename = "inventory-" + LocalDate.now() + ".xlsx";

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename)
                .contentType(MediaType.parseMediaType(
                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
                .body(resource);
    }

    /**
     * 品目一覧 Excel 出力
     */
    @GetMapping("/items/excel")
    public ResponseEntity<Resource> exportItemsExcel() {
        List<Item> items = itemUseCase.getAllItems();
        Resource resource = reportService.exportItemsToExcel(items);
        String filename = "items-" + LocalDate.now() + ".xlsx";

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename)
                .contentType(MediaType.parseMediaType(
                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
                .body(resource);
    }

    /**
     * オーダ一覧 Excel 出力
     */
    @GetMapping("/orders/excel")
    public ResponseEntity<Resource> exportOrdersExcel() {
        List<Order> orders = orderRepository.findAll();
        Resource resource = reportService.exportOrdersToExcel(orders);
        String filename = "orders-" + LocalDate.now() + ".xlsx";

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename)
                .contentType(MediaType.parseMediaType(
                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
                .body(resource);
    }

    /**
     * 作業指示一覧 Excel 出力
     */
    @GetMapping("/work-orders/excel")
    public ResponseEntity<Resource> exportWorkOrdersExcel() {
        List<WorkOrder> workOrders = workOrderUseCase.getAllWorkOrders();
        Resource resource = reportService.exportWorkOrdersToExcel(workOrders);
        String filename = "work-orders-" + LocalDate.now() + ".xlsx";

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename)
                .contentType(MediaType.parseMediaType(
                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
                .body(resource);
    }
}
