package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.out.CompletionResultRepository;
import com.example.production.application.port.out.WorkOrderRepository;
import com.example.production.application.service.CompletionResultService;
import com.example.production.domain.model.process.CompletionResult;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;
import com.example.production.infrastructure.web.form.CompletionForm;
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
 * 完成実績 Controller（モノリス版）
 */
@Controller
@RequestMapping("/completions")
@RequiredArgsConstructor
public class CompletionWebController {

    private final CompletionResultService completionResultService;
    private final CompletionResultRepository completionResultRepository;
    private final WorkOrderRepository workOrderRepository;

    /**
     * 完成実績一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) String workOrderNumber,
            Model model) {

        List<CompletionResult> completions;
        if (workOrderNumber != null && !workOrderNumber.isBlank()) {
            completions = completionResultService.findByWorkOrderNumber(workOrderNumber);
        } else {
            completions = completionResultRepository.findAll();
        }

        model.addAttribute("completions", completions);
        model.addAttribute("workOrders", getInProgressWorkOrders());
        model.addAttribute("selectedWorkOrder", workOrderNumber);

        return "completions/list";
    }

    /**
     * 完成実績詳細画面
     */
    @GetMapping("/{completionResultNumber}")
    public String detail(@PathVariable String completionResultNumber, Model model) {
        CompletionResult completion = completionResultService.findByCompletionResultNumber(completionResultNumber);
        if (completion == null) {
            throw new IllegalArgumentException("Completion result not found: " + completionResultNumber);
        }
        model.addAttribute("completion", completion);
        return "completions/detail";
    }

    /**
     * 完成実績登録画面
     */
    @GetMapping("/new")
    public String newForm(
            @RequestParam(required = false) String workOrderNumber,
            Model model) {

        CompletionForm form = new CompletionForm();
        form.setWorkOrderNumber(workOrderNumber);
        form.setCompletionDate(LocalDate.now());

        // 作業指示情報を取得して数量を設定
        if (workOrderNumber != null && !workOrderNumber.isBlank()) {
            workOrderRepository.findByWorkOrderNumber(workOrderNumber).ifPresent(wo -> {
                // 残数量を初期値として設定
                var remaining = wo.getOrderQuantity().subtract(wo.getCompletedQuantity());
                form.setCompletedQuantity(remaining);
                form.setGoodQuantity(remaining);
            });
        }

        model.addAttribute("form", form);
        model.addAttribute("workOrders", getInProgressWorkOrders());
        return "completions/new";
    }

    /**
     * 完成実績登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") CompletionForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("workOrders", getInProgressWorkOrders());
            return "completions/new";
        }

        try {
            CompletionResult completion = completionResultService.createCompletionResult(form.toCommand());
            redirectAttributes.addFlashAttribute("successMessage",
                    "完成実績「" + completion.getCompletionResultNumber() + "」を登録しました");
            return "redirect:/completions";
        } catch (IllegalArgumentException | IllegalStateException e) {
            model.addAttribute("errorMessage", e.getMessage());
            model.addAttribute("workOrders", getInProgressWorkOrders());
            return "completions/new";
        }
    }

    /**
     * 作業中の作業指示を取得
     */
    private List<WorkOrder> getInProgressWorkOrders() {
        return workOrderRepository.findByStatus(WorkOrderStatus.IN_PROGRESS);
    }
}
