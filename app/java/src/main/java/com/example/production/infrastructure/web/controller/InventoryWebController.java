package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.in.InventoryUseCase;
import com.example.production.application.port.out.ItemRepository;
import com.example.production.application.port.out.LocationRepository;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.domain.model.inventory.StockStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.List;

/**
 * 在庫照会 Controller（モノリス版）
 */
@Controller
@RequestMapping("/inventory")
@RequiredArgsConstructor
public class InventoryWebController {

    private final InventoryUseCase inventoryUseCase;
    private final LocationRepository locationRepository;
    private final ItemRepository itemRepository;

    /**
     * 在庫一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) String locationCode,
            @RequestParam(required = false) String itemCode,
            Model model) {

        List<Stock> stocks;
        if (locationCode != null && !locationCode.isBlank() && itemCode != null && !itemCode.isBlank()) {
            Stock stock = inventoryUseCase.getStock(locationCode, itemCode);
            stocks = stock.getStockQuantity().compareTo(BigDecimal.ZERO) > 0 ? List.of(stock) : List.of();
        } else if (locationCode != null && !locationCode.isBlank()) {
            stocks = inventoryUseCase.getStocksByLocation(locationCode);
        } else if (itemCode != null && !itemCode.isBlank()) {
            stocks = inventoryUseCase.getStocksByItem(itemCode);
        } else {
            stocks = inventoryUseCase.getAllStocks();
        }

        // 集計
        BigDecimal totalStock = stocks.stream()
                .map(Stock::getStockQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        BigDecimal totalPassed = stocks.stream()
                .map(Stock::getPassedQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        BigDecimal totalDefective = stocks.stream()
                .map(Stock::getDefectiveQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        BigDecimal totalUninspected = stocks.stream()
                .map(Stock::getUninspectedQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        model.addAttribute("stocks", stocks);
        model.addAttribute("locations", locationRepository.findAll());
        model.addAttribute("items", itemRepository.findAll());
        model.addAttribute("selectedLocation", locationCode);
        model.addAttribute("selectedItem", itemCode);
        model.addAttribute("totalStock", totalStock);
        model.addAttribute("totalPassed", totalPassed);
        model.addAttribute("totalDefective", totalDefective);
        model.addAttribute("totalUninspected", totalUninspected);

        return "inventory/list";
    }

    /**
     * 在庫詳細画面
     */
    @GetMapping("/{locationCode}/{itemCode}")
    public String detail(
            @PathVariable String locationCode,
            @PathVariable String itemCode,
            Model model) {

        Stock stock = inventoryUseCase.getStock(locationCode, itemCode);
        model.addAttribute("stock", stock);
        model.addAttribute("statuses", StockStatus.values());
        return "inventory/detail";
    }
}
