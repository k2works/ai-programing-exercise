package com.example.production.infrastructure.in.web.controller;

import com.example.production.application.port.out.OrderRepository;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
import com.example.production.domain.model.plan.PlanStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.List;

/**
 * オーダ照会 Controller（モノリス版）
 */
@Controller
@RequestMapping("/orders")
@RequiredArgsConstructor
public class OrderWebController {

    private final OrderRepository orderRepository;

    /**
     * オーダ一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) PlanStatus status,
            @RequestParam(required = false) OrderType orderType,
            Model model) {

        List<Order> orders = orderRepository.findAll();

        // ステータスでフィルタリング
        if (status != null) {
            orders = orders.stream()
                    .filter(o -> o.getStatus() == status)
                    .toList();
        }
        // オーダタイプでフィルタリング
        if (orderType != null) {
            orders = orders.stream()
                    .filter(o -> o.getOrderType() == orderType)
                    .toList();
        }

        model.addAttribute("orders", orders);
        model.addAttribute("statuses", PlanStatus.values());
        model.addAttribute("orderTypes", OrderType.values());
        model.addAttribute("selectedStatus", status);
        model.addAttribute("selectedOrderType", orderType);

        return "orders/list";
    }

    /**
     * オーダ詳細画面
     */
    @GetMapping("/{id}")
    public String detail(@PathVariable Integer id, Model model) {
        Order order = orderRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Order not found: " + id));

        // 子オーダを取得
        List<Order> childOrders = orderRepository.findByParentOrderId(id);

        // 親オーダを取得
        Order parentOrder = null;
        if (order.getParentOrderId() != null) {
            parentOrder = orderRepository.findById(order.getParentOrderId()).orElse(null);
        }

        model.addAttribute("order", order);
        model.addAttribute("childOrders", childOrders);
        model.addAttribute("parentOrder", parentOrder);

        return "orders/detail";
    }

    /**
     * オーダ確定処理
     */
    @PostMapping("/{id}/confirm")
    public String confirm(
            @PathVariable Integer id,
            RedirectAttributes redirectAttributes) {

        Order order = orderRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Order not found: " + id));

        if (order.getStatus() != PlanStatus.DRAFT) {
            redirectAttributes.addFlashAttribute("errorMessage", "草案状態のオーダのみ確定できます");
            return "redirect:/orders/" + id;
        }

        orderRepository.updateStatus(id, PlanStatus.CONFIRMED);
        redirectAttributes.addFlashAttribute("successMessage",
                "オーダ「" + order.getOrderNumber() + "」を確定しました");
        return "redirect:/orders/" + id;
    }

    /**
     * オーダ取消処理
     */
    @PostMapping("/{id}/cancel")
    public String cancel(
            @PathVariable Integer id,
            RedirectAttributes redirectAttributes) {

        Order order = orderRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Order not found: " + id));

        if (order.getStatus() == PlanStatus.CANCELLED) {
            redirectAttributes.addFlashAttribute("errorMessage", "既に取り消されています");
            return "redirect:/orders/" + id;
        }

        orderRepository.updateStatus(id, PlanStatus.CANCELLED);
        redirectAttributes.addFlashAttribute("successMessage",
                "オーダ「" + order.getOrderNumber() + "」を取り消しました");
        return "redirect:/orders";
    }
}
