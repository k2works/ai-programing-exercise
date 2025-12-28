package com.example.production.infrastructure.in.rest.controller;

import com.example.production.application.port.out.BomRepository;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.bom.BomExplosion;
import com.example.production.infrastructure.in.rest.dto.BomExplosionResponse;
import com.example.production.infrastructure.in.rest.dto.BomResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.List;

/**
 * BOM Controller（部品構成表 API）
 */
@RestController
@RequestMapping("/api/bom")
@Tag(name = "bom", description = "BOM（部品構成表）API")
@RequiredArgsConstructor
public class BomController {

    private final BomRepository bomRepository;

    @GetMapping("/{itemCode}/children")
    @Operation(
            summary = "子品目一覧の取得",
            description = "指定した親品目の直下の子品目を取得します"
    )
    public ResponseEntity<List<BomResponse>> getChildren(
            @Parameter(description = "親品目コード", required = true)
            @PathVariable String itemCode) {

        List<Bom> children = bomRepository.findByParentItemCode(itemCode);
        return ResponseEntity.ok(children.stream()
                .map(BomResponse::from)
                .toList());
    }

    @GetMapping("/{itemCode}/parents")
    @Operation(
            summary = "親品目一覧の取得（使用先照会）",
            description = "指定した子品目が使用されている親品目を検索します"
    )
    public ResponseEntity<List<BomResponse>> getParents(
            @Parameter(description = "子品目コード", required = true)
            @PathVariable String itemCode) {

        List<Bom> parents = bomRepository.findByChildItemCode(itemCode);
        return ResponseEntity.ok(parents.stream()
                .map(BomResponse::from)
                .toList());
    }

    @GetMapping("/{itemCode}/explode")
    @Operation(
            summary = "部品展開",
            description = "指定した品目の BOM を再帰的に展開します"
    )
    public ResponseEntity<List<BomExplosionResponse>> explodeBom(
            @Parameter(description = "親品目コード", required = true)
            @PathVariable String itemCode,
            @Parameter(description = "基準数量（デフォルト: 1）")
            @RequestParam(defaultValue = "1") BigDecimal quantity) {

        List<BomExplosion> explosions = bomRepository.explode(itemCode, quantity);
        return ResponseEntity.ok(explosions.stream()
                .map(BomExplosionResponse::from)
                .toList());
    }

    @GetMapping
    @Operation(summary = "BOM 全件取得")
    public ResponseEntity<List<BomResponse>> getAllBom() {
        List<Bom> bomList = bomRepository.findAll();
        return ResponseEntity.ok(bomList.stream()
                .map(BomResponse::from)
                .toList());
    }
}
