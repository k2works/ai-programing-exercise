package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.domain.model.item.Item;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import java.time.LocalDate;
import java.util.List;

/**
 * ホーム画面 Controller（モノリス版）
 */
@Controller
@RequiredArgsConstructor
public class HomeController {

    private final ItemUseCase itemUseCase;

    /**
     * ダッシュボード画面
     */
    @GetMapping("/")
    public String home(Model model) {
        // ダッシュボード情報を取得
        model.addAttribute("today", LocalDate.now());

        // 品目数を取得
        List<Item> items = itemUseCase.getAllItems();
        model.addAttribute("itemCount", items.size());

        return "home";
    }
}
