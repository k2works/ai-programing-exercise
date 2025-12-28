package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.out.BomRepository;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.bom.BomExplosion;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * BOM マスタ Controller（モノリス版）
 */
@Controller
@RequestMapping("/bom")
@RequiredArgsConstructor
public class BomWebController {

    private final BomRepository bomRepository;
    private final ItemUseCase itemUseCase;

    /**
     * BOM 一覧画面
     */
    @GetMapping
    public String list(Model model) {
        List<Item> products = itemUseCase.getItemsByCategory(ItemCategory.PRODUCT);
        List<Bom> allBom = bomRepository.findAll();
        model.addAttribute("products", products);
        model.addAttribute("allBom", allBom);
        return "bom/list";
    }

    /**
     * BOM 新規登録画面
     */
    @GetMapping("/new")
    public String newBom(@RequestParam(required = false) String parentItemCode, Model model) {
        List<Item> items = itemUseCase.getAllItems();
        model.addAttribute("items", items);
        model.addAttribute("parentItemCode", parentItemCode);
        model.addAttribute("bom", Bom.builder()
                .parentItemCode(parentItemCode)
                .effectiveFrom(LocalDate.now())
                .baseQuantity(BigDecimal.ONE)
                .requiredQuantity(BigDecimal.ONE)
                .defectRate(BigDecimal.ZERO)
                .sequence(1)
                .build());
        model.addAttribute("isNew", true);
        return "bom/form";
    }

    /**
     * BOM 登録処理
     */
    @PostMapping
    public String create(
            @RequestParam String parentItemCode,
            @RequestParam String childItemCode,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate effectiveFrom,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate effectiveTo,
            @RequestParam BigDecimal baseQuantity,
            @RequestParam BigDecimal requiredQuantity,
            @RequestParam(defaultValue = "0") BigDecimal defectRate,
            @RequestParam(defaultValue = "1") Integer sequence,
            RedirectAttributes redirectAttributes) {

        Bom bom = Bom.builder()
                .parentItemCode(parentItemCode)
                .childItemCode(childItemCode)
                .effectiveFrom(effectiveFrom)
                .effectiveTo(effectiveTo)
                .baseQuantity(baseQuantity)
                .requiredQuantity(requiredQuantity)
                .defectRate(defectRate)
                .sequence(sequence)
                .build();

        bomRepository.save(bom);
        redirectAttributes.addFlashAttribute("successMessage", "BOM を登録しました");
        return "redirect:/bom/" + parentItemCode + "/children";
    }

    /**
     * BOM 編集画面
     */
    @GetMapping("/edit")
    public String edit(
            @RequestParam String parentItemCode,
            @RequestParam String childItemCode,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate effectiveFrom,
            Model model) {

        Bom bom = bomRepository.findByKey(parentItemCode, childItemCode, effectiveFrom);
        if (bom == null) {
            return "redirect:/bom";
        }

        List<Item> items = itemUseCase.getAllItems();
        model.addAttribute("items", items);
        model.addAttribute("bom", bom);
        model.addAttribute("isNew", false);
        return "bom/form";
    }

    /**
     * BOM 更新処理
     */
    @PostMapping("/update")
    public String update(
            @RequestParam String parentItemCode,
            @RequestParam String childItemCode,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate effectiveFrom,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate effectiveTo,
            @RequestParam BigDecimal baseQuantity,
            @RequestParam BigDecimal requiredQuantity,
            @RequestParam(defaultValue = "0") BigDecimal defectRate,
            @RequestParam(defaultValue = "1") Integer sequence,
            RedirectAttributes redirectAttributes) {

        Bom bom = Bom.builder()
                .parentItemCode(parentItemCode)
                .childItemCode(childItemCode)
                .effectiveFrom(effectiveFrom)
                .effectiveTo(effectiveTo)
                .baseQuantity(baseQuantity)
                .requiredQuantity(requiredQuantity)
                .defectRate(defectRate)
                .sequence(sequence)
                .build();

        bomRepository.update(bom);
        redirectAttributes.addFlashAttribute("successMessage", "BOM を更新しました");
        return "redirect:/bom/" + parentItemCode + "/children";
    }

    /**
     * BOM 削除処理
     */
    @PostMapping("/delete")
    public String delete(
            @RequestParam String parentItemCode,
            @RequestParam String childItemCode,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate effectiveFrom,
            RedirectAttributes redirectAttributes) {

        bomRepository.delete(parentItemCode, childItemCode, effectiveFrom);
        redirectAttributes.addFlashAttribute("successMessage", "BOM を削除しました");
        return "redirect:/bom/" + parentItemCode + "/children";
    }

    /**
     * BOM 構成表示（ツリー展開）
     */
    @GetMapping("/{itemCode}/explode")
    public String explode(
            @PathVariable String itemCode,
            @RequestParam(defaultValue = "1") BigDecimal quantity,
            Model model) {

        List<BomExplosion> explosions = bomRepository.explode(itemCode, quantity);
        Item item = itemUseCase.getItemByCode(itemCode);

        model.addAttribute("item", item);
        model.addAttribute("explosions", explosions);
        model.addAttribute("quantity", quantity);
        return "bom/explode";
    }

    /**
     * 逆展開（使用先照会）
     */
    @GetMapping("/{itemCode}/where-used")
    public String whereUsed(@PathVariable String itemCode, Model model) {
        List<Bom> parents = bomRepository.findByChildItemCode(itemCode);
        Item item = itemUseCase.getItemByCode(itemCode);

        model.addAttribute("item", item);
        model.addAttribute("parents", parents);
        return "bom/where-used";
    }

    /**
     * 品目の直下子品目一覧
     */
    @GetMapping("/{itemCode}/children")
    public String children(@PathVariable String itemCode, Model model) {
        List<Bom> children = bomRepository.findByParentItemCode(itemCode);
        Item item = itemUseCase.getItemByCode(itemCode);

        model.addAttribute("item", item);
        model.addAttribute("children", children);
        return "bom/children";
    }
}
