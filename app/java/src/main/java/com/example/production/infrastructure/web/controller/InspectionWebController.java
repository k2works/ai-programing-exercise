package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.out.InspectionRepository;
import com.example.production.application.port.out.ReceivingRepository;
import com.example.production.domain.model.purchase.Inspection;
import com.example.production.domain.model.purchase.Receiving;
import com.example.production.infrastructure.web.form.InspectionForm;
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
 * 検収 Controller（モノリス版）
 */
@Controller
@RequestMapping("/inspections")
@RequiredArgsConstructor
public class InspectionWebController {

    private final InspectionRepository inspectionRepository;
    private final ReceivingRepository receivingRepository;

    /**
     * 検収一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) String receivingNumber,
            Model model) {

        List<Inspection> inspections;
        if (receivingNumber != null && !receivingNumber.isBlank()) {
            inspections = inspectionRepository.findByReceivingNumber(receivingNumber);
        } else {
            inspections = inspectionRepository.findAll();
        }

        model.addAttribute("inspections", inspections);
        model.addAttribute("receivings", receivingRepository.findAll());
        model.addAttribute("selectedReceiving", receivingNumber);

        return "inspections/list";
    }

    /**
     * 検収詳細画面
     */
    @GetMapping("/{inspectionNumber}")
    public String detail(@PathVariable String inspectionNumber, Model model) {
        Inspection inspection = inspectionRepository.findByInspectionNumber(inspectionNumber)
                .orElseThrow(() -> new IllegalArgumentException("Inspection not found: " + inspectionNumber));
        model.addAttribute("inspection", inspection);
        return "inspections/detail";
    }

    /**
     * 検収登録画面
     */
    @GetMapping("/new")
    public String newForm(
            @RequestParam(required = false) String receivingNumber,
            Model model) {

        InspectionForm form = new InspectionForm();
        form.setReceivingNumber(receivingNumber);
        form.setInspectionDate(LocalDate.now());

        // 入荷情報を取得して発注情報を設定
        if (receivingNumber != null && !receivingNumber.isBlank()) {
            receivingRepository.findByReceivingNumber(receivingNumber).ifPresent(receiving -> {
                form.setPurchaseOrderNumber(receiving.getPurchaseOrderNumber());
                form.setLineNumber(receiving.getLineNumber());
                form.setItemCode(receiving.getItemCode());
                form.setGoodQuantity(receiving.getReceivingQuantity());
            });
        }

        model.addAttribute("form", form);
        model.addAttribute("receivings", receivingRepository.findAll());
        return "inspections/new";
    }

    /**
     * 検収登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") InspectionForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("receivings", receivingRepository.findAll());
            return "inspections/new";
        }

        Inspection inspection = form.toEntity();
        inspection.setInspectionNumber(generateInspectionNumber(form.getInspectionDate()));
        inspectionRepository.save(inspection);

        redirectAttributes.addFlashAttribute("successMessage",
                "検収「" + inspection.getInspectionNumber() + "」を登録しました");
        return "redirect:/inspections";
    }

    private String generateInspectionNumber(LocalDate inspectionDate) {
        String prefix = "INS-" + inspectionDate.format(DateTimeFormatter.ofPattern("yyyyMM")) + "-";
        return inspectionRepository.findLatestInspectionNumber(prefix)
                .map(latest -> {
                    int currentSequence = Integer.parseInt(latest.substring(latest.length() - 4));
                    return prefix + String.format("%04d", currentSequence + 1);
                })
                .orElse(prefix + "0001");
    }
}
