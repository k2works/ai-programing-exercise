package com.example.production.infrastructure.in.web.controller;

import com.example.production.application.port.out.MpsRepository;
import com.example.production.application.port.out.OrderRepository;
import com.example.production.application.service.MrpService;
import com.example.production.domain.model.plan.MasterProductionSchedule;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.PlanStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.time.LocalDateTime;
import java.util.List;

/**
 * MRP 実行 Controller（モノリス版）
 */
@Controller
@RequestMapping("/mrp")
@RequiredArgsConstructor
public class MrpWebController {

    private final MrpService mrpService;
    private final MpsRepository mpsRepository;
    private final OrderRepository orderRepository;

    /**
     * MRP 実行画面
     */
    @GetMapping
    public String index(
            @RequestParam(required = false) PlanStatus status,
            Model model) {

        List<MasterProductionSchedule> mpsList;
        if (status != null) {
            mpsList = mpsRepository.findByStatus(status);
        } else {
            mpsList = mpsRepository.findAll();
        }

        model.addAttribute("mpsList", mpsList);
        model.addAttribute("statuses", PlanStatus.values());
        model.addAttribute("selectedStatus", status);

        return "mrp/index";
    }

    /**
     * MRP 実行処理
     */
    @PostMapping("/execute/{mpsId}")
    public String execute(
            @PathVariable Integer mpsId,
            RedirectAttributes redirectAttributes) {

        try {
            mrpService.executeMrp(mpsId, mpsRepository);
            redirectAttributes.addFlashAttribute("successMessage",
                    "MRP を実行しました（MPS ID: " + mpsId + "）");
        } catch (Exception e) {
            redirectAttributes.addFlashAttribute("errorMessage",
                    "MRP 実行に失敗しました: " + e.getMessage());
        }

        return "redirect:/mrp/result/" + mpsId;
    }

    /**
     * MRP 実行結果画面
     */
    @GetMapping("/result/{mpsId}")
    public String result(@PathVariable Integer mpsId, Model model) {
        MasterProductionSchedule mps = mpsRepository.findById(mpsId)
                .orElseThrow(() -> new IllegalArgumentException("MPS not found: " + mpsId));

        List<Order> orders = orderRepository.findByMpsId(mpsId);

        model.addAttribute("mps", mps);
        model.addAttribute("orders", orders);
        model.addAttribute("executionTime", LocalDateTime.now());

        return "mrp/result";
    }

    /**
     * 一括 MRP 実行処理
     */
    @PostMapping("/execute-all")
    public String executeAll(RedirectAttributes redirectAttributes) {
        List<MasterProductionSchedule> confirmedList = mpsRepository.findByStatus(PlanStatus.CONFIRMED);

        int successCount = 0;
        int errorCount = 0;

        for (MasterProductionSchedule mps : confirmedList) {
            try {
                mrpService.executeMrp(mps.getId(), mpsRepository);
                successCount++;
            } catch (Exception e) {
                errorCount++;
            }
        }

        if (errorCount == 0) {
            redirectAttributes.addFlashAttribute("successMessage",
                    successCount + " 件の MRP を実行しました");
        } else {
            redirectAttributes.addFlashAttribute("warningMessage",
                    "成功: " + successCount + " 件、失敗: " + errorCount + " 件");
        }

        return "redirect:/mrp";
    }
}
