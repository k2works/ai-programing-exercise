package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.out.BomRepository;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.bom.BomExplosion;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
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
     * BOM 一覧画面（製品の一覧）
     */
    @GetMapping
    public String list(Model model) {
        List<Item> products = itemUseCase.getItemsByCategory(ItemCategory.PRODUCT);
        model.addAttribute("products", products);
        return "bom/list";
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
