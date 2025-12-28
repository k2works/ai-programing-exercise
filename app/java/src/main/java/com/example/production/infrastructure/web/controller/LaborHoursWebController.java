package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.out.LaborHoursRepository;
import com.example.production.application.port.out.WorkOrderDetailRepository;
import com.example.production.application.port.out.WorkOrderRepository;
import com.example.production.application.service.LaborHoursService;
import com.example.production.domain.model.process.LaborHours;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;
import com.example.production.infrastructure.web.form.LaborHoursForm;
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
 * 工数実績 Controller（モノリス版）
 */
@Controller
@RequestMapping("/labor-hours")
@RequiredArgsConstructor
public class LaborHoursWebController {

    private final LaborHoursService laborHoursService;
    private final LaborHoursRepository laborHoursRepository;
    private final WorkOrderRepository workOrderRepository;
    private final WorkOrderDetailRepository workOrderDetailRepository;

    /**
     * 工数実績一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) String workOrderNumber,
            Model model) {

        List<LaborHours> laborHours;
        if (workOrderNumber != null && !workOrderNumber.isBlank()) {
            laborHours = laborHoursService.findByWorkOrderNumber(workOrderNumber);
        } else {
            laborHours = laborHoursRepository.findAll();
        }

        model.addAttribute("laborHours", laborHours);
        model.addAttribute("workOrders", getInProgressWorkOrders());
        model.addAttribute("selectedWorkOrder", workOrderNumber);

        return "labor-hours/list";
    }

    /**
     * 工数実績詳細画面
     */
    @GetMapping("/{laborHoursNumber}")
    public String detail(@PathVariable String laborHoursNumber, Model model) {
        LaborHours laborHours = laborHoursService.findByLaborHoursNumber(laborHoursNumber);
        if (laborHours == null) {
            throw new IllegalArgumentException("Labor hours not found: " + laborHoursNumber);
        }
        model.addAttribute("laborHours", laborHours);
        return "labor-hours/detail";
    }

    /**
     * 工数実績登録画面
     */
    @GetMapping("/new")
    public String newForm(
            @RequestParam(required = false) String workOrderNumber,
            Model model) {

        LaborHoursForm form = new LaborHoursForm();
        form.setWorkOrderNumber(workOrderNumber);
        form.setWorkDate(LocalDate.now());
        form.setSequence(1);

        model.addAttribute("form", form);
        model.addAttribute("workOrders", getInProgressWorkOrders());

        // 作業指示の工程を取得
        if (workOrderNumber != null && !workOrderNumber.isBlank()) {
            var details = workOrderDetailRepository.findByWorkOrderNumber(workOrderNumber);
            model.addAttribute("workOrderDetails", details);
        }

        return "labor-hours/new";
    }

    /**
     * 工数実績登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") LaborHoursForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("workOrders", getInProgressWorkOrders());
            if (form.getWorkOrderNumber() != null && !form.getWorkOrderNumber().isBlank()) {
                var details = workOrderDetailRepository.findByWorkOrderNumber(form.getWorkOrderNumber());
                model.addAttribute("workOrderDetails", details);
            }
            return "labor-hours/new";
        }

        try {
            LaborHours laborHours = laborHoursService.createLaborHours(form.toCommand());
            redirectAttributes.addFlashAttribute("successMessage",
                    "工数実績「" + laborHours.getLaborHoursNumber() + "」を登録しました");
            return "redirect:/labor-hours";
        } catch (IllegalArgumentException | IllegalStateException e) {
            model.addAttribute("errorMessage", e.getMessage());
            model.addAttribute("workOrders", getInProgressWorkOrders());
            if (form.getWorkOrderNumber() != null && !form.getWorkOrderNumber().isBlank()) {
                var details = workOrderDetailRepository.findByWorkOrderNumber(form.getWorkOrderNumber());
                model.addAttribute("workOrderDetails", details);
            }
            return "labor-hours/new";
        }
    }

    /**
     * 工数サマリ画面
     */
    @GetMapping("/summary/{workOrderNumber}")
    public String summary(@PathVariable String workOrderNumber, Model model) {
        var summary = laborHoursService.getSummary(workOrderNumber);
        var workOrder = workOrderRepository.findByWorkOrderNumber(workOrderNumber)
                .orElseThrow(() -> new IllegalArgumentException("Work order not found: " + workOrderNumber));

        model.addAttribute("summary", summary);
        model.addAttribute("workOrder", workOrder);
        return "labor-hours/summary";
    }

    /**
     * 作業中の作業指示を取得
     */
    private List<WorkOrder> getInProgressWorkOrders() {
        return workOrderRepository.findByStatus(WorkOrderStatus.IN_PROGRESS);
    }
}
