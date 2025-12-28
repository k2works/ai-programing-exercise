package com.example.production.infrastructure.in.web.controller;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.out.LocationRepository;
import com.example.production.application.port.out.MpsRepository;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.plan.MasterProductionSchedule;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.infrastructure.in.web.form.MpsForm;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.List;

/**
 * 基準生産計画 Controller（モノリス版）
 */
@Controller
@RequestMapping("/mps")
@RequiredArgsConstructor
public class MpsWebController {

    private final MpsRepository mpsRepository;
    private final ItemUseCase itemUseCase;
    private final LocationRepository locationRepository;

    /**
     * MPS一覧画面
     */
    @GetMapping
    public String list(
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

        return "mps/list";
    }

    /**
     * MPS詳細画面
     */
    @GetMapping("/{id}")
    public String detail(@PathVariable Integer id, Model model) {
        MasterProductionSchedule mps = mpsRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("MPS not found: " + id));
        model.addAttribute("mps", mps);
        return "mps/detail";
    }

    /**
     * MPS登録画面
     */
    @GetMapping("/new")
    public String newForm(Model model) {
        model.addAttribute("form", new MpsForm());
        model.addAttribute("items", itemUseCase.getItemsByCategory(ItemCategory.PRODUCT));
        model.addAttribute("locations", locationRepository.findAll());
        return "mps/new";
    }

    /**
     * MPS登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") MpsForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("items", itemUseCase.getItemsByCategory(ItemCategory.PRODUCT));
            model.addAttribute("locations", locationRepository.findAll());
            return "mps/new";
        }

        MasterProductionSchedule mps = form.toEntity();
        mpsRepository.save(mps);

        redirectAttributes.addFlashAttribute("successMessage",
                "基準生産計画「" + mps.getMpsNumber() + "」を登録しました");
        return "redirect:/mps";
    }

    /**
     * MPS編集画面
     */
    @GetMapping("/{id}/edit")
    public String editForm(@PathVariable Integer id, Model model) {
        MasterProductionSchedule mps = mpsRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("MPS not found: " + id));

        // 草案状態のみ編集可能
        if (mps.getStatus() != PlanStatus.DRAFT) {
            return "redirect:/mps/" + id;
        }

        model.addAttribute("form", MpsForm.from(mps));
        model.addAttribute("items", itemUseCase.getItemsByCategory(ItemCategory.PRODUCT));
        model.addAttribute("locations", locationRepository.findAll());
        return "mps/edit";
    }

    /**
     * MPS更新処理
     */
    @PostMapping("/{id}")
    public String update(
            @PathVariable Integer id,
            @Valid @ModelAttribute("form") MpsForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        MasterProductionSchedule mps = mpsRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("MPS not found: " + id));

        // 草案状態のみ編集可能
        if (mps.getStatus() != PlanStatus.DRAFT) {
            redirectAttributes.addFlashAttribute("errorMessage", "確定済みのMPSは編集できません");
            return "redirect:/mps/" + id;
        }

        if (bindingResult.hasErrors()) {
            model.addAttribute("items", itemUseCase.getItemsByCategory(ItemCategory.PRODUCT));
            model.addAttribute("locations", locationRepository.findAll());
            return "mps/edit";
        }

        mpsRepository.update(form.toEntity(mps));

        redirectAttributes.addFlashAttribute("successMessage",
                "基準生産計画「" + mps.getMpsNumber() + "」を更新しました");
        return "redirect:/mps/" + id;
    }

    /**
     * MPS確定処理
     */
    @PostMapping("/{id}/confirm")
    public String confirm(
            @PathVariable Integer id,
            RedirectAttributes redirectAttributes) {

        MasterProductionSchedule mps = mpsRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("MPS not found: " + id));

        if (mps.getStatus() != PlanStatus.DRAFT) {
            redirectAttributes.addFlashAttribute("errorMessage", "草案状態のMPSのみ確定できます");
            return "redirect:/mps/" + id;
        }

        mpsRepository.updateStatus(id, PlanStatus.CONFIRMED);
        redirectAttributes.addFlashAttribute("successMessage",
                "基準生産計画「" + mps.getMpsNumber() + "」を確定しました");
        return "redirect:/mps/" + id;
    }

    /**
     * MPS取消処理
     */
    @PostMapping("/{id}/cancel")
    public String cancel(
            @PathVariable Integer id,
            RedirectAttributes redirectAttributes) {

        MasterProductionSchedule mps = mpsRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("MPS not found: " + id));

        if (mps.getStatus() == PlanStatus.CANCELLED) {
            redirectAttributes.addFlashAttribute("errorMessage", "既に取り消されています");
            return "redirect:/mps/" + id;
        }

        mpsRepository.updateStatus(id, PlanStatus.CANCELLED);
        redirectAttributes.addFlashAttribute("successMessage",
                "基準生産計画「" + mps.getMpsNumber() + "」を取り消しました");
        return "redirect:/mps";
    }
}
