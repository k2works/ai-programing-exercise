package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.out.LocationRepository;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import com.example.production.infrastructure.web.form.LocationForm;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.List;

/**
 * 場所マスタ Controller（モノリス版）
 */
@Controller
@RequestMapping("/locations")
@RequiredArgsConstructor
public class LocationWebController {

    private final LocationRepository locationRepository;

    /**
     * 場所一覧画面
     */
    @GetMapping
    public String list(
            @RequestParam(required = false) LocationType type,
            @RequestParam(required = false) String keyword,
            Model model) {

        List<Location> locations;
        if (type != null) {
            locations = locationRepository.findByLocationType(type);
        } else {
            locations = locationRepository.findAll();
        }

        // キーワード検索（メモリ上でフィルタリング）
        if (keyword != null && !keyword.isBlank()) {
            String lowerKeyword = keyword.toLowerCase();
            locations = locations.stream()
                    .filter(l -> l.getLocationCode().toLowerCase().contains(lowerKeyword)
                            || l.getLocationName().toLowerCase().contains(lowerKeyword))
                    .toList();
        }

        model.addAttribute("locations", locations);
        model.addAttribute("types", LocationType.values());
        model.addAttribute("selectedType", type);
        model.addAttribute("keyword", keyword);

        return "locations/list";
    }

    /**
     * 場所詳細画面
     */
    @GetMapping("/{locationCode}")
    public String detail(@PathVariable String locationCode, Model model) {
        Location location = locationRepository.findByLocationCode(locationCode)
                .orElseThrow(() -> new IllegalArgumentException("場所が見つかりません: " + locationCode));

        // 親場所の情報を取得
        if (location.getParentLocationCode() != null) {
            locationRepository.findByLocationCode(location.getParentLocationCode())
                    .ifPresent(parent -> model.addAttribute("parentLocation", parent));
        }

        // 子場所の一覧を取得
        List<Location> childLocations = locationRepository.findByParentLocationCode(locationCode);

        model.addAttribute("location", location);
        model.addAttribute("childLocations", childLocations);
        return "locations/detail";
    }

    /**
     * 場所登録画面
     */
    @GetMapping("/new")
    public String newForm(Model model) {
        model.addAttribute("form", new LocationForm());
        model.addAttribute("types", LocationType.values());
        model.addAttribute("allLocations", locationRepository.findAll());
        return "locations/new";
    }

    /**
     * 場所登録処理
     */
    @PostMapping
    public String create(
            @Valid @ModelAttribute("form") LocationForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("types", LocationType.values());
            model.addAttribute("allLocations", locationRepository.findAll());
            return "locations/new";
        }

        // 重複チェック
        if (locationRepository.findByLocationCode(form.getLocationCode()).isPresent()) {
            model.addAttribute("errorMessage",
                    "場所コード「" + form.getLocationCode() + "」は既に存在します");
            model.addAttribute("types", LocationType.values());
            model.addAttribute("allLocations", locationRepository.findAll());
            return "locations/new";
        }

        locationRepository.save(form.toEntity());
        redirectAttributes.addFlashAttribute("successMessage",
                "場所「" + form.getLocationCode() + "」を登録しました");
        return "redirect:/locations";
    }

    /**
     * 場所編集画面
     */
    @GetMapping("/{locationCode}/edit")
    public String editForm(@PathVariable String locationCode, Model model) {
        Location location = locationRepository.findByLocationCode(locationCode)
                .orElseThrow(() -> new IllegalArgumentException("場所が見つかりません: " + locationCode));
        model.addAttribute("form", LocationForm.from(location));
        model.addAttribute("types", LocationType.values());
        // 自分自身を除外した場所一覧（親場所として選択可能なもの）
        List<Location> availableParents = locationRepository.findAll().stream()
                .filter(l -> !l.getLocationCode().equals(locationCode))
                .toList();
        model.addAttribute("allLocations", availableParents);
        model.addAttribute("isEdit", true);
        return "locations/edit";
    }

    /**
     * 場所更新処理
     */
    @PostMapping("/{locationCode}")
    public String update(
            @PathVariable String locationCode,
            @Valid @ModelAttribute("form") LocationForm form,
            BindingResult bindingResult,
            Model model,
            RedirectAttributes redirectAttributes) {

        if (bindingResult.hasErrors()) {
            model.addAttribute("types", LocationType.values());
            List<Location> availableParents = locationRepository.findAll().stream()
                    .filter(l -> !l.getLocationCode().equals(locationCode))
                    .toList();
            model.addAttribute("allLocations", availableParents);
            model.addAttribute("isEdit", true);
            return "locations/edit";
        }

        // フォームの locationCode をパスパラメータで上書き
        form.setLocationCode(locationCode);
        locationRepository.save(form.toEntity());
        redirectAttributes.addFlashAttribute("successMessage",
                "場所「" + locationCode + "」を更新しました");
        return "redirect:/locations/" + locationCode;
    }
}
