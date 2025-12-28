package com.example.production.infrastructure.in.web.controller;

import com.example.production.application.port.out.SupplierRepository;
import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import com.example.production.infrastructure.in.web.form.SupplierForm;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.List;

/**
 * 取引先マスタ Controller（モノリス版）
 */
@Controller
@RequestMapping("/suppliers")
@RequiredArgsConstructor
public class SupplierWebController {

    private final SupplierRepository supplierRepository;

    /**
     * 取引先一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) SupplierType type,
            @RequestParam(required = false) String keyword,
            Model model) {

        List<Supplier> suppliers;
        if (type != null) {
            suppliers = supplierRepository.findBySupplierType(type);
        } else {
            suppliers = supplierRepository.findAll();
        }

        // キーワード検索（メモリ上でフィルタリング）
        if (keyword != null && !keyword.isBlank()) {
            String lowerKeyword = keyword.toLowerCase();
            suppliers = suppliers.stream()
                    .filter(s -> s.getSupplierCode().toLowerCase().contains(lowerKeyword)
                            || s.getSupplierName().toLowerCase().contains(lowerKeyword)
                            || (s.getSupplierNameKana() != null
                                && s.getSupplierNameKana().toLowerCase().contains(lowerKeyword)))
                    .toList();
        }

        model.addAttribute("suppliers", suppliers);
        model.addAttribute("types", SupplierType.values());
        model.addAttribute("selectedType", type);
        model.addAttribute("keyword", keyword);

        return "suppliers/list";
    }

    /**
     * 取引先詳細画面
     */
    @GetMapping("/{supplierCode}")
    public String detail(@PathVariable String supplierCode, Model model) {
        Supplier supplier = supplierRepository.findBySupplierCode(supplierCode)
                .orElseThrow(() -> new IllegalArgumentException("取引先が見つかりません: " + supplierCode));
        model.addAttribute("supplier", supplier);
        return "suppliers/detail";
    }

    /**
     * 取引先登録画面
     */
    @GetMapping("/new")
    public String newForm(Model model) {
        model.addAttribute("form", new SupplierForm());
        model.addAttribute("types", SupplierType.values());
        return "suppliers/new";
    }

    /**
     * 取引先登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") SupplierForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("types", SupplierType.values());
            return "suppliers/new";
        }

        // 重複チェック
        if (supplierRepository.findBySupplierCode(form.getSupplierCode()).isPresent()) {
            model.addAttribute("errorMessage",
                    "取引先コード「" + form.getSupplierCode() + "」は既に存在します");
            model.addAttribute("types", SupplierType.values());
            return "suppliers/new";
        }

        supplierRepository.save(form.toEntity());
        redirectAttributes.addFlashAttribute("successMessage",
                "取引先「" + form.getSupplierCode() + "」を登録しました");
        return "redirect:/suppliers";
    }

    /**
     * 取引先編集画面
     */
    @GetMapping("/{supplierCode}/edit")
    public String editForm(@PathVariable String supplierCode, Model model) {
        Supplier supplier = supplierRepository.findBySupplierCode(supplierCode)
                .orElseThrow(() -> new IllegalArgumentException("取引先が見つかりません: " + supplierCode));
        model.addAttribute("form", SupplierForm.from(supplier));
        model.addAttribute("types", SupplierType.values());
        model.addAttribute("isEdit", true);
        return "suppliers/edit";
    }

    /**
     * 取引先更新処理
     */
    @PostMapping("/{supplierCode}")
    public String update(
            @PathVariable String supplierCode,
            @Valid @ModelAttribute("form") SupplierForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("types", SupplierType.values());
            model.addAttribute("isEdit", true);
            return "suppliers/edit";
        }

        // フォームの supplierCode をパスパラメータで上書き
        form.setSupplierCode(supplierCode);
        supplierRepository.save(form.toEntity());
        redirectAttributes.addFlashAttribute("successMessage",
                "取引先「" + supplierCode + "」を更新しました");
        return "redirect:/suppliers/" + supplierCode;
    }
}
