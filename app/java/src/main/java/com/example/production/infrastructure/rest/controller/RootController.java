package com.example.production.infrastructure.rest.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * ルート API コントローラ
 */
@RestController
@Tag(name = "root", description = "ルート API")
public class RootController {

    @GetMapping("/api")
    @Operation(summary = "API 情報の取得")
    public Map<String, Object> root() {
        return Map.of(
                "message", "生産管理システム API",
                "version", "1.0.0",
                "endpoints", List.of(
                        "/api/items",
                        "/api/bom",
                        "/api/suppliers",
                        "/api/orders",
                        "/api/inventory",
                        "/api/work-orders",
                        "/api/purchase-orders"
                ),
                "docs", "/swagger-ui.html"
        );
    }

    @GetMapping("/health")
    @Operation(summary = "ヘルスチェック")
    public Map<String, Object> health() {
        return Map.of(
                "status", "ok",
                "timestamp", Instant.now().toString()
        );
    }
}
