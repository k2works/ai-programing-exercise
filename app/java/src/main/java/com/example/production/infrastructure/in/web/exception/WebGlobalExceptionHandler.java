package com.example.production.infrastructure.in.web.exception;

import com.example.production.domain.exception.*;
import com.example.production.domain.model.inventory.InsufficientStockException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.annotation.Order;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

/**
 * グローバル例外ハンドラ（モノリス版）
 * Web Controller 用の例外処理を行う
 */
@Slf4j
@ControllerAdvice(basePackages = "com.example.production.infrastructure.web.controller")
@Order(1)
public class WebGlobalExceptionHandler {

    /**
     * 品目が見つからない
     */
    @ExceptionHandler(ItemNotFoundException.class)
    public String handleItemNotFound(ItemNotFoundException e, Model model) {
        model.addAttribute("errorTitle", "品目が見つかりません");
        model.addAttribute("errorMessage", e.getMessage());
        return "error/404";
    }

    /**
     * 発注が見つからない
     */
    @ExceptionHandler(PurchaseOrderNotFoundException.class)
    public String handlePurchaseOrderNotFound(PurchaseOrderNotFoundException e, Model model) {
        model.addAttribute("errorTitle", "発注が見つかりません");
        model.addAttribute("errorMessage", e.getMessage());
        return "error/404";
    }

    /**
     * 作業指示が見つからない
     */
    @ExceptionHandler(WorkOrderNotFoundException.class)
    public String handleWorkOrderNotFound(WorkOrderNotFoundException e, Model model) {
        model.addAttribute("errorTitle", "作業指示が見つかりません");
        model.addAttribute("errorMessage", e.getMessage());
        return "error/404";
    }

    /**
     * 重複エラー
     */
    @ExceptionHandler(DuplicateItemException.class)
    public String handleDuplicate(DuplicateItemException e, RedirectAttributes redirectAttributes) {
        redirectAttributes.addFlashAttribute("errorMessage", e.getMessage());
        return "redirect:/items/new";
    }

    /**
     * 在庫不足
     */
    @ExceptionHandler(InsufficientStockException.class)
    public String handleInsufficientStock(InsufficientStockException e, Model model) {
        model.addAttribute("errorTitle", "在庫不足");
        model.addAttribute("errorMessage", e.getMessage());
        return "error/business-error";
    }

    /**
     * 在庫不足（旧）
     */
    @ExceptionHandler(InsufficientInventoryException.class)
    public String handleInsufficientInventory(InsufficientInventoryException e, Model model) {
        model.addAttribute("errorTitle", "在庫不足");
        model.addAttribute("errorMessage", e.getMessage());
        return "error/business-error";
    }

    /**
     * ドメインエラー
     */
    @ExceptionHandler(DomainException.class)
    public String handleDomainException(DomainException e, Model model) {
        model.addAttribute("errorTitle", "業務エラー");
        model.addAttribute("errorMessage", e.getMessage());
        return "error/business-error";
    }

    /**
     * 不正な引数
     */
    @ExceptionHandler(IllegalArgumentException.class)
    public String handleIllegalArgument(IllegalArgumentException e, Model model) {
        model.addAttribute("errorTitle", "入力エラー");
        model.addAttribute("errorMessage", e.getMessage());
        return "error/business-error";
    }

    /**
     * 不正な状態
     */
    @ExceptionHandler(IllegalStateException.class)
    public String handleIllegalState(IllegalStateException e, Model model) {
        model.addAttribute("errorTitle", "操作エラー");
        model.addAttribute("errorMessage", e.getMessage());
        return "error/business-error";
    }

    /**
     * その他の例外
     */
    @ExceptionHandler(Exception.class)
    public String handleGenericException(Exception e, Model model) {
        log.error("予期しないエラーが発生しました", e);
        model.addAttribute("errorTitle", "システムエラー");
        model.addAttribute("errorMessage", "予期しないエラーが発生しました。管理者に連絡してください。");
        model.addAttribute("errorDetail", e.getMessage());
        return "error/500";
    }
}
