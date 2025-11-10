package com.example.sales.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

/**
 * ヘルスチェック用コントローラー
 */
@RestController
public class HealthController {

    /**
     * ルートエンドポイント
     */
    @GetMapping("/")
    public Map<String, Object> root() {
        Map<String, Object> response = new HashMap<>();
        response.put("application", "Sales Management System");
        response.put("status", "running");
        response.put("timestamp", LocalDateTime.now());
        response.put("message", "Application is running successfully");
        return response;
    }

    /**
     * ヘルスチェックエンドポイント
     */
    @GetMapping("/health")
    public Map<String, Object> health() {
        Map<String, Object> response = new HashMap<>();
        response.put("status", "UP");
        response.put("timestamp", LocalDateTime.now());
        return response;
    }
}
