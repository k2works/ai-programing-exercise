package com.example.production.infrastructure.in.web.controller;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.domain.exception.DuplicateItemException;
import com.example.production.domain.exception.ItemNotFoundException;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.infrastructure.in.web.form.ItemForm;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.List;

/**
 * 品目マスタ Controller（モノリス版）
 */
@Controller
@RequestMapping("/items")
@RequiredArgsConstructor
public class ItemWebController {

    private final ItemUseCase itemUseCase;

    /**
     * 品目一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) ItemCategory category,
            @RequestParam(required = false) String keyword,
            Model model) {

        List<Item> items;
        if (category != null) {
            items = itemUseCase.getItemsByCategory(category);
        } else {
            items = itemUseCase.getAllItems();
        }

        // キーワード検索（メモリ上でフィルタリング）
        if (keyword != null && !keyword.isBlank()) {
            String lowerKeyword = keyword.toLowerCase();
            items = items.stream()
                    .filter(item -> item.getItemCode().toLowerCase().contains(lowerKeyword)
                            || item.getItemName().toLowerCase().contains(lowerKeyword))
                    .toList();
        }

        model.addAttribute("items", items);
        model.addAttribute("categories", ItemCategory.values());
        model.addAttribute("selectedCategory", category);
        model.addAttribute("keyword", keyword);

        return "items/list";
    }

    /**
     * 品目詳細画面
     */
    @GetMapping("/{itemCode}")
    public String detail(@PathVariable String itemCode, Model model) {
        Item item = itemUseCase.getItemByCode(itemCode);
        model.addAttribute("item", item);
        return "items/detail";
    }

    /**
     * 品目登録画面
     */
    @GetMapping("/new")
    public String newForm(Model model) {
        model.addAttribute("form", new ItemForm());
        model.addAttribute("categories", ItemCategory.values());
        return "items/new";
    }

    /**
     * 品目登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") ItemForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("categories", ItemCategory.values());
            return "items/new";
        }

        try {
            itemUseCase.createItem(form.toCreateCommand());
            redirectAttributes.addFlashAttribute("successMessage",
                    "品目「" + form.getItemCode() + "」を登録しました");
            return "redirect:/items";
        } catch (DuplicateItemException e) {
            model.addAttribute("errorMessage",
                    "品目コード「" + form.getItemCode() + "」は既に存在します");
            model.addAttribute("categories", ItemCategory.values());
            return "items/new";
        }
    }

    /**
     * 品目編集画面
     */
    @GetMapping("/{itemCode}/edit")
    public String editForm(@PathVariable String itemCode, Model model) {
        Item item = itemUseCase.getItemByCode(itemCode);
        model.addAttribute("form", ItemForm.from(item));
        model.addAttribute("categories", ItemCategory.values());
        model.addAttribute("isEdit", true);
        return "items/edit";
    }

    /**
     * 品目更新処理
     */
    @PostMapping("/{itemCode}")
    public String update(
            @PathVariable String itemCode,
            @Valid @ModelAttribute("form") ItemForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("categories", ItemCategory.values());
            model.addAttribute("isEdit", true);
            return "items/edit";
        }

        // フォームの itemCode をパスパラメータで上書き
        form.setItemCode(itemCode);
        itemUseCase.updateItem(form.toUpdateCommand());
        redirectAttributes.addFlashAttribute("successMessage",
                "品目「" + itemCode + "」を更新しました");
        return "redirect:/items/" + itemCode;
    }

    /**
     * 品目削除処理
     */
    @PostMapping("/{itemCode}/delete")
    public String delete(
            @PathVariable String itemCode,
            RedirectAttributes redirectAttributes) {

        itemUseCase.deleteItem(itemCode);
        redirectAttributes.addFlashAttribute("successMessage",
                "品目「" + itemCode + "」を削除しました");
        return "redirect:/items";
    }

    /**
     * 例外ハンドリング（品目が見つからない場合）
     */
    @ExceptionHandler(ItemNotFoundException.class)
    @ResponseStatus(HttpStatus.NOT_FOUND)
    public String handleItemNotFound(ItemNotFoundException e, Model model) {
        model.addAttribute("errorMessage", e.getMessage());
        return "error/404";
    }
}
