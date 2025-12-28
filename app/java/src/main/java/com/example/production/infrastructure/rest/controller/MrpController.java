package com.example.production.infrastructure.rest.controller;

import com.example.production.application.service.MrpService;
import com.example.production.infrastructure.rest.dto.ExecuteMrpRequest;
import com.example.production.infrastructure.rest.dto.MrpResultResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Map;

/**
 * MRP Controller（Input Adapter）
 */
@RestController
@RequestMapping("/api/mrp")
@Tag(name = "mrp", description = "MRP API")
@RequiredArgsConstructor
public class MrpController {

    private final MrpService mrpService;

    @PostMapping("/execute")
    @Operation(
            summary = "MRP の実行",
            description = "指定期間の所要量展開を実行し、計画オーダを生成します"
    )
    public ResponseEntity<MrpResultResponse> execute(
            @Valid @RequestBody ExecuteMrpRequest request) {
        // MRP実行結果を返す（現在は簡易版）
        MrpResultResponse result = MrpResultResponse.builder()
                .executionTime(LocalDateTime.now())
                .periodStart(request.getStartDate())
                .periodEnd(request.getEndDate())
                .requirements(Collections.emptyList())
                .build();

        return ResponseEntity.ok(result);
    }

    @GetMapping("/results")
    @Operation(summary = "MRP 実行結果の照会")
    public ResponseEntity<Map<String, String>> getResults() {
        return ResponseEntity.ok(Map.of("message", "MRP 実行結果一覧は未実装です"));
    }

    @GetMapping("/planned-orders")
    @Operation(summary = "計画オーダ一覧の取得")
    public ResponseEntity<Map<String, String>> getPlannedOrders() {
        return ResponseEntity.ok(Map.of("message", "計画オーダ一覧は未実装です"));
    }
}
