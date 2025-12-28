package com.example.production.infrastructure.in.web.controller;

import com.example.production.application.port.in.command.IssueDetailCommand;
import com.example.production.application.port.in.command.IssueExecuteCommand;
import com.example.production.application.port.out.ItemRepository;
import com.example.production.application.port.out.LocationRepository;
import com.example.production.application.port.out.WorkOrderRepository;
import com.example.production.application.service.IssueService;
import com.example.production.domain.model.inventory.Issue;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;
import com.example.production.infrastructure.in.web.form.IssueForm;
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
 * 払出 Controller（モノリス版）
 */
@Controller
@RequestMapping("/issues")
@RequiredArgsConstructor
public class IssueWebController {

    private final IssueService issueService;
    private final WorkOrderRepository workOrderRepository;
    private final LocationRepository locationRepository;
    private final ItemRepository itemRepository;

    /**
     * 払出一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) String workOrderNumber,
            Model model) {

        List<Issue> issues;
        if (workOrderNumber != null && !workOrderNumber.isBlank()) {
            issues = issueService.findByWorkOrderNumber(workOrderNumber);
        } else {
            issues = issueService.findAll();
        }

        model.addAttribute("issues", issues);
        model.addAttribute("workOrders", getInProgressWorkOrders());
        model.addAttribute("selectedWorkOrder", workOrderNumber);

        return "issues/list";
    }

    /**
     * 払出詳細画面
     */
    @GetMapping("/{issueNumber}")
    public String detail(@PathVariable String issueNumber, Model model) {
        Issue issue = issueService.findByIssueNumber(issueNumber);
        if (issue == null) {
            return "redirect:/issues";
        }
        model.addAttribute("issue", issue);
        return "issues/detail";
    }

    /**
     * 払出登録画面
     */
    @GetMapping("/new")
    public String newForm(
            @RequestParam(required = false) String workOrderNumber,
            Model model) {

        IssueForm form = new IssueForm();
        form.setWorkOrderNumber(workOrderNumber);
        form.setIssueDate(LocalDate.now());
        form.setRoutingSequence(1);

        model.addAttribute("form", form);
        model.addAttribute("workOrders", getInProgressWorkOrders());
        model.addAttribute("locations", locationRepository.findAll());
        model.addAttribute("items", itemRepository.findAll());

        return "issues/new";
    }

    /**
     * 払出登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") IssueForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("workOrders", getInProgressWorkOrders());
            model.addAttribute("locations", locationRepository.findAll());
            model.addAttribute("items", itemRepository.findAll());
            return "issues/new";
        }

        try {
            List<IssueDetailCommand> details = List.of(
                    IssueDetailCommand.builder()
                            .itemCode(form.getItemCode())
                            .issueQuantity(form.getIssueQuantity())
                            .build()
            );

            IssueExecuteCommand command = IssueExecuteCommand.builder()
                    .workOrderNumber(form.getWorkOrderNumber())
                    .routingSequence(form.getRoutingSequence())
                    .locationCode(form.getLocationCode())
                    .issueDate(form.getIssueDate())
                    .issuerCode(form.getIssuerCode())
                    .details(details)
                    .build();

            Issue issue = issueService.executeIssue(command);
            redirectAttributes.addFlashAttribute("successMessage",
                    "払出「" + issue.getIssueNumber() + "」を登録しました");
            return "redirect:/issues";
        } catch (Exception e) {
            model.addAttribute("errorMessage", e.getMessage());
            model.addAttribute("workOrders", getInProgressWorkOrders());
            model.addAttribute("locations", locationRepository.findAll());
            model.addAttribute("items", itemRepository.findAll());
            return "issues/new";
        }
    }

    /**
     * 作業中の作業指示を取得
     */
    private List<WorkOrder> getInProgressWorkOrders() {
        return workOrderRepository.findAll().stream()
                .filter(wo -> wo.getStatus() == WorkOrderStatus.IN_PROGRESS ||
                              wo.getStatus() == WorkOrderStatus.NOT_STARTED)
                .toList();
    }
}
