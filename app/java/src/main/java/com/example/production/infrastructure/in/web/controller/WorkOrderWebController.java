package com.example.production.infrastructure.in.web.controller;

import com.example.production.application.port.in.WorkOrderUseCase;
import com.example.production.application.port.out.LocationRepository;
import com.example.production.application.port.out.OrderRepository;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;
import com.example.production.infrastructure.in.web.form.WorkOrderForm;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.time.LocalDate;
import java.util.List;

/**
 * 作業指示 Controller（モノリス版）
 */
@Controller
@RequestMapping("/work-orders")
@RequiredArgsConstructor
public class WorkOrderWebController {

    private final WorkOrderUseCase workOrderUseCase;
    private final OrderRepository orderRepository;
    private final LocationRepository locationRepository;

    /**
     * 作業指示一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) WorkOrderStatus status,
            Model model) {

        List<WorkOrder> workOrders;
        if (status != null) {
            workOrders = workOrderUseCase.getWorkOrdersByStatus(status);
        } else {
            workOrders = workOrderUseCase.getAllWorkOrders();
        }

        model.addAttribute("workOrders", workOrders);
        model.addAttribute("statuses", WorkOrderStatus.values());
        model.addAttribute("selectedStatus", status);

        return "work-orders/list";
    }

    /**
     * 作業指示詳細画面
     */
    @GetMapping("/{workOrderNumber}")
    public String detail(@PathVariable String workOrderNumber, Model model) {
        WorkOrder workOrder = workOrderUseCase.getWorkOrder(workOrderNumber);
        model.addAttribute("workOrder", workOrder);
        return "work-orders/detail";
    }

    /**
     * 作業指示登録画面
     */
    @GetMapping("/new")
    public String newForm(
            @RequestParam(required = false) String orderNumber,
            Model model) {

        WorkOrderForm form = new WorkOrderForm();
        form.setOrderNumber(orderNumber);
        form.setWorkOrderDate(LocalDate.now());
        form.setPlannedStartDate(LocalDate.now());
        form.setPlannedEndDate(LocalDate.now().plusDays(7));

        model.addAttribute("form", form);
        model.addAttribute("orders", getManufacturingOrders());
        model.addAttribute("locations", locationRepository.findAll());
        return "work-orders/new";
    }

    /**
     * 作業指示登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") WorkOrderForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("orders", getManufacturingOrders());
            model.addAttribute("locations", locationRepository.findAll());
            return "work-orders/new";
        }

        try {
            WorkOrder workOrder = workOrderUseCase.createWorkOrder(form.toCommand());
            redirectAttributes.addFlashAttribute("successMessage",
                    "作業指示「" + workOrder.getWorkOrderNumber() + "」を登録しました");
            return "redirect:/work-orders";
        } catch (IllegalArgumentException e) {
            model.addAttribute("errorMessage", e.getMessage());
            model.addAttribute("orders", getManufacturingOrders());
            model.addAttribute("locations", locationRepository.findAll());
            return "work-orders/new";
        }
    }

    /**
     * 作業開始処理
     */
    @PostMapping("/{workOrderNumber}/start")
    public String startWork(
            @PathVariable String workOrderNumber,
            RedirectAttributes redirectAttributes) {
        try {
            workOrderUseCase.startWork(workOrderNumber);
            redirectAttributes.addFlashAttribute("successMessage",
                    "作業指示「" + workOrderNumber + "」の作業を開始しました");
        } catch (IllegalStateException e) {
            redirectAttributes.addFlashAttribute("errorMessage", e.getMessage());
        }
        return "redirect:/work-orders/" + workOrderNumber;
    }

    /**
     * 作業完了処理
     */
    @PostMapping("/{workOrderNumber}/complete")
    public String completeWork(
            @PathVariable String workOrderNumber,
            RedirectAttributes redirectAttributes) {
        try {
            workOrderUseCase.completeWork(workOrderNumber);
            redirectAttributes.addFlashAttribute("successMessage",
                    "作業指示「" + workOrderNumber + "」の作業を完了しました");
        } catch (IllegalStateException e) {
            redirectAttributes.addFlashAttribute("errorMessage", e.getMessage());
        }
        return "redirect:/work-orders/" + workOrderNumber;
    }

    /**
     * 製造オーダを取得（確定済みで作業指示未発行のもの）
     */
    private List<Order> getManufacturingOrders() {
        return orderRepository.findAll().stream()
                .filter(o -> o.getOrderType() == OrderType.MANUFACTURING)
                .filter(o -> o.getStatus() == PlanStatus.CONFIRMED || o.getStatus() == PlanStatus.EXPANDED)
                .toList();
    }
}
