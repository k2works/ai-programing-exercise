package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.in.PurchaseOrderUseCase;
import com.example.production.application.port.out.SupplierRepository;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import com.example.production.infrastructure.web.form.PurchaseOrderForm;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.List;

/**
 * 発注 Controller（モノリス版）
 */
@Controller
@RequestMapping("/purchase-orders")
@RequiredArgsConstructor
public class PurchaseOrderWebController {

    private final PurchaseOrderUseCase purchaseOrderUseCase;
    private final SupplierRepository supplierRepository;
    private final ItemUseCase itemUseCase;

    /**
     * 発注一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) PurchaseOrderStatus status,
            @RequestParam(required = false) String supplierCode,
            Model model) {

        List<PurchaseOrder> orders = purchaseOrderUseCase.getAllOrders();

        // フィルタリング
        if (status != null) {
            orders = orders.stream()
                    .filter(o -> o.getStatus() == status)
                    .toList();
        }
        if (supplierCode != null && !supplierCode.isBlank()) {
            orders = orders.stream()
                    .filter(o -> supplierCode.equals(o.getSupplierCode()))
                    .toList();
        }

        model.addAttribute("orders", orders);
        model.addAttribute("statuses", PurchaseOrderStatus.values());
        model.addAttribute("suppliers", supplierRepository.findAll());
        model.addAttribute("selectedStatus", status);
        model.addAttribute("selectedSupplier", supplierCode);

        return "purchase-orders/list";
    }

    /**
     * 発注詳細画面
     */
    @GetMapping("/{orderNumber}")
    public String detail(@PathVariable String orderNumber, Model model) {
        PurchaseOrder order = purchaseOrderUseCase.getOrder(orderNumber);
        model.addAttribute("order", order);
        return "purchase-orders/detail";
    }

    /**
     * 発注登録画面
     */
    @GetMapping("/new")
    public String newForm(Model model) {
        PurchaseOrderForm form = new PurchaseOrderForm();
        // 初期明細行を1行追加
        form.getDetails().add(new PurchaseOrderForm.DetailForm());

        model.addAttribute("form", form);
        model.addAttribute("suppliers", supplierRepository.findAll());
        model.addAttribute("items", itemUseCase.getItemsByCategory(ItemCategory.MATERIAL));
        return "purchase-orders/new";
    }

    /**
     * 発注登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") PurchaseOrderForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("suppliers", supplierRepository.findAll());
            model.addAttribute("items", itemUseCase.getItemsByCategory(ItemCategory.MATERIAL));
            return "purchase-orders/new";
        }

        PurchaseOrder order = purchaseOrderUseCase.createOrder(form.toCommand());
        redirectAttributes.addFlashAttribute("successMessage",
                "発注「" + order.getPurchaseOrderNumber() + "」を登録しました");
        return "redirect:/purchase-orders";
    }

    /**
     * 発注確定処理
     */
    @PostMapping("/{orderNumber}/confirm")
    public String confirm(
            @PathVariable String orderNumber,
            RedirectAttributes redirectAttributes) {

        purchaseOrderUseCase.confirmOrder(orderNumber);
        redirectAttributes.addFlashAttribute("successMessage",
                "発注「" + orderNumber + "」を確定しました");
        return "redirect:/purchase-orders/" + orderNumber;
    }

    /**
     * 発注取消処理
     */
    @PostMapping("/{orderNumber}/cancel")
    public String cancel(
            @PathVariable String orderNumber,
            RedirectAttributes redirectAttributes) {

        purchaseOrderUseCase.cancelOrder(orderNumber);
        redirectAttributes.addFlashAttribute("successMessage",
                "発注「" + orderNumber + "」を取り消しました");
        return "redirect:/purchase-orders";
    }

    /**
     * 明細行追加（htmx フラグメント）
     */
    @GetMapping("/add-detail-row")
    public String addDetailRow(
            @RequestParam(defaultValue = "0") int index,
            Model model) {

        model.addAttribute("index", index);
        model.addAttribute("items", itemUseCase.getItemsByCategory(ItemCategory.MATERIAL));
        return "purchase-orders/fragments :: detailRow";
    }
}
