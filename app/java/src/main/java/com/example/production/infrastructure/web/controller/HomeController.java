package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.in.InventoryUseCase;
import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.out.OrderRepository;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.PlanStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * ホーム画面 Controller（モノリス版）
 */
@Controller
@RequiredArgsConstructor
public class HomeController {

    private final ItemUseCase itemUseCase;
    private final OrderRepository orderRepository;
    private final InventoryUseCase inventoryUseCase;

    /**
     * ダッシュボード画面
     */
    @GetMapping("/")
    public String home(Model model) {
        // ダッシュボード情報を取得
        model.addAttribute("today", LocalDate.now());

        // 品目数を取得
        List<Item> items = itemUseCase.getAllItems();
        model.addAttribute("itemCount", items.size());

        // 未処理オーダ数を取得（草案または確定状態のオーダ）
        List<Order> orders = orderRepository.findAll();
        long pendingOrderCount = orders.stream()
                .filter(o -> o.getStatus() == PlanStatus.DRAFT || o.getStatus() == PlanStatus.CONFIRMED)
                .count();
        model.addAttribute("pendingOrderCount", pendingOrderCount);

        // 在庫不足品目数を取得（在庫がゼロまたは不良品がある品目）
        List<Stock> stocks = inventoryUseCase.getAllStocks();
        long lowStockCount = stocks.stream()
                .filter(s -> s.getStockQuantity().compareTo(BigDecimal.ZERO) == 0 ||
                             s.getDefectiveQuantity().compareTo(BigDecimal.ZERO) > 0)
                .count();
        model.addAttribute("lowStockCount", lowStockCount);

        return "home";
    }
}
