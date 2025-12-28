package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.out.PurchaseOrderRepository;
import com.example.production.application.port.out.PurchaseOrderDetailRepository;
import com.example.production.application.port.out.ReceivingRepository;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import com.example.production.domain.model.purchase.Receiving;
import com.example.production.domain.model.purchase.ReceivingType;
import com.example.production.infrastructure.web.form.ReceivingForm;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;

/**
 * 入荷 Controller（モノリス版）
 */
@Controller
@RequestMapping("/receivings")
@RequiredArgsConstructor
public class ReceivingWebController {

    private final ReceivingRepository receivingRepository;
    private final PurchaseOrderRepository purchaseOrderRepository;
    private final PurchaseOrderDetailRepository purchaseOrderDetailRepository;

    /**
     * 入荷一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) String purchaseOrderNumber,
            Model model) {

        List<Receiving> receivings;
        if (purchaseOrderNumber != null && !purchaseOrderNumber.isBlank()) {
            receivings = receivingRepository.findByPurchaseOrderNumber(purchaseOrderNumber);
        } else {
            receivings = receivingRepository.findAll();
        }

        // 発注済・一部入荷の発注を取得
        var orders = purchaseOrderRepository.findAll().stream()
                .filter(o -> o.getStatus() == PurchaseOrderStatus.ORDERED ||
                             o.getStatus() == PurchaseOrderStatus.PARTIALLY_RECEIVED)
                .toList();

        model.addAttribute("receivings", receivings);
        model.addAttribute("orders", orders);
        model.addAttribute("selectedPurchaseOrder", purchaseOrderNumber);

        return "receivings/list";
    }

    /**
     * 入荷詳細画面
     */
    @GetMapping("/{receivingNumber}")
    public String detail(@PathVariable String receivingNumber, Model model) {
        Receiving receiving = receivingRepository.findByReceivingNumber(receivingNumber)
                .orElseThrow(() -> new IllegalArgumentException("Receiving not found: " + receivingNumber));
        model.addAttribute("receiving", receiving);
        return "receivings/detail";
    }

    /**
     * 入荷登録画面
     */
    @GetMapping("/new")
    public String newForm(
            @RequestParam(required = false) String purchaseOrderNumber,
            Model model) {

        ReceivingForm form = new ReceivingForm();
        form.setPurchaseOrderNumber(purchaseOrderNumber);
        form.setReceivingDate(LocalDate.now());
        form.setReceivingType(ReceivingType.NORMAL);

        // 発注明細を取得して品目を設定
        if (purchaseOrderNumber != null && !purchaseOrderNumber.isBlank()) {
            var details = purchaseOrderDetailRepository.findByPurchaseOrderNumber(purchaseOrderNumber);
            if (!details.isEmpty()) {
                var detail = details.get(0);
                form.setLineNumber(detail.getLineNumber());
                form.setItemCode(detail.getItemCode());
            }
        }

        model.addAttribute("form", form);
        model.addAttribute("receivingTypes", ReceivingType.values());
        model.addAttribute("orders", getReceivableOrders());
        return "receivings/new";
    }

    /**
     * 入荷登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") ReceivingForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("receivingTypes", ReceivingType.values());
            model.addAttribute("orders", getReceivableOrders());
            return "receivings/new";
        }

        Receiving receiving = form.toEntity();
        receiving.setReceivingNumber(generateReceivingNumber(form.getReceivingDate()));
        receivingRepository.save(receiving);

        // 発注ステータスを更新（一部入荷）
        purchaseOrderRepository.updateStatus(form.getPurchaseOrderNumber(), PurchaseOrderStatus.PARTIALLY_RECEIVED);

        redirectAttributes.addFlashAttribute("successMessage",
                "入荷「" + receiving.getReceivingNumber() + "」を登録しました");
        return "redirect:/receivings";
    }

    private List<com.example.production.domain.model.purchase.PurchaseOrder> getReceivableOrders() {
        return purchaseOrderRepository.findAll().stream()
                .filter(o -> o.getStatus() == PurchaseOrderStatus.ORDERED ||
                             o.getStatus() == PurchaseOrderStatus.PARTIALLY_RECEIVED)
                .toList();
    }

    private String generateReceivingNumber(LocalDate receivingDate) {
        String prefix = "RCV-" + receivingDate.format(DateTimeFormatter.ofPattern("yyyyMM")) + "-";
        return receivingRepository.findLatestReceivingNumber(prefix)
                .map(latest -> {
                    int currentSequence = Integer.parseInt(latest.substring(latest.length() - 4));
                    return prefix + String.format("%04d", currentSequence + 1);
                })
                .orElse(prefix + "0001");
    }
}
