package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.in.command.StocktakingConfirmCommand;
import com.example.production.application.port.in.command.StocktakingIssueCommand;
import com.example.production.application.port.out.LocationRepository;
import com.example.production.application.service.StocktakingService;
import com.example.production.domain.model.inventory.Stocktaking;
import com.example.production.domain.model.inventory.StocktakingStatus;
import com.example.production.infrastructure.web.form.StocktakingForm;
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
 * 棚卸 Controller（モノリス版）
 */
@Controller
@RequestMapping("/stocktakings")
@RequiredArgsConstructor
public class StocktakingWebController {

    private final StocktakingService stocktakingService;
    private final LocationRepository locationRepository;

    /**
     * 棚卸一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) StocktakingStatus status,
            Model model) {

        List<Stocktaking> stocktakings;
        if (status != null) {
            stocktakings = stocktakingService.findAll().stream()
                    .filter(s -> s.getStatus() == status)
                    .toList();
        } else {
            stocktakings = stocktakingService.findAll();
        }

        model.addAttribute("stocktakings", stocktakings);
        model.addAttribute("statuses", StocktakingStatus.values());
        model.addAttribute("selectedStatus", status);

        return "stocktakings/list";
    }

    /**
     * 棚卸詳細画面
     */
    @GetMapping("/{stocktakingNumber}")
    public String detail(@PathVariable String stocktakingNumber, Model model) {
        Stocktaking stocktaking = stocktakingService.findByStocktakingNumber(stocktakingNumber);
        if (stocktaking == null) {
            return "redirect:/stocktakings";
        }
        model.addAttribute("stocktaking", stocktaking);
        return "stocktakings/detail";
    }

    /**
     * 棚卸表発行画面
     */
    @GetMapping("/new")
    public String newForm(Model model) {
        StocktakingForm form = new StocktakingForm();
        form.setStocktakingDate(LocalDate.now());

        model.addAttribute("form", form);
        model.addAttribute("locations", locationRepository.findAll());

        return "stocktakings/new";
    }

    /**
     * 棚卸表発行処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") StocktakingForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("locations", locationRepository.findAll());
            return "stocktakings/new";
        }

        try {
            StocktakingIssueCommand command = StocktakingIssueCommand.builder()
                    .locationCode(form.getLocationCode())
                    .stocktakingDate(form.getStocktakingDate())
                    .build();

            Stocktaking stocktaking = stocktakingService.issueStocktakingSheet(command);
            redirectAttributes.addFlashAttribute("successMessage",
                    "棚卸表「" + stocktaking.getStocktakingNumber() + "」を発行しました");
            return "redirect:/stocktakings/" + stocktaking.getStocktakingNumber();
        } catch (Exception e) {
            model.addAttribute("errorMessage", e.getMessage());
            model.addAttribute("locations", locationRepository.findAll());
            return "stocktakings/new";
        }
    }

    /**
     * 棚卸確定処理
     */
    @PostMapping("/{stocktakingNumber}/confirm")
    public String confirm(
            @PathVariable String stocktakingNumber,
            @RequestParam(required = false) String adjusterCode,
            @RequestParam(required = false) String adjustmentReasonCode,
            RedirectAttributes redirectAttributes) {
        try {
            StocktakingConfirmCommand command = StocktakingConfirmCommand.builder()
                    .stocktakingNumber(stocktakingNumber)
                    .adjusterCode(adjusterCode)
                    .adjustmentReasonCode(adjustmentReasonCode)
                    .build();

            stocktakingService.confirmStocktaking(command);
            redirectAttributes.addFlashAttribute("successMessage",
                    "棚卸「" + stocktakingNumber + "」を確定しました");
        } catch (IllegalStateException | IllegalArgumentException e) {
            redirectAttributes.addFlashAttribute("errorMessage", e.getMessage());
        }
        return "redirect:/stocktakings/" + stocktakingNumber;
    }
}
