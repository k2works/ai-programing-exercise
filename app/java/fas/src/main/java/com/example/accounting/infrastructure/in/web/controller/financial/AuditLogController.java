package com.example.accounting.infrastructure.in.web.controller.financial;

import com.example.accounting.application.service.finacial.AuditLogService;
import com.example.accounting.domain.model.audit.AuditLog;
import com.example.accounting.infrastructure.in.web.dto.financial.AuditLogResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 監査ログ REST API
 */
@RestController
@RequestMapping("/api/v1/audit-logs")
@RequiredArgsConstructor
@Tag(name = "監査ログAPI", description = "監査ログの照会API")
public class AuditLogController {
    private final AuditLogService auditLogService;

    /**
     * エンティティの変更履歴を取得
     *
     * @param entityType エンティティ種別
     * @param entityId エンティティID
     * @return 監査ログリスト
     */
    @GetMapping("/entity/{entityType}/{entityId}")
    @Operation(summary = "エンティティの変更履歴を取得する")
    public ResponseEntity<List<AuditLogResponse>> getEntityHistory(
            @PathVariable String entityType,
            @PathVariable String entityId) {

        List<AuditLog> logs = auditLogService.getLogsByEntity(entityType, entityId);

        return ResponseEntity.ok(logs.stream()
                .map(AuditLogResponse::from)
                .collect(Collectors.toList()));
    }

    /**
     * ユーザーの操作履歴を取得
     *
     * @param userId ユーザーID
     * @param startDate 開始日時
     * @param endDate 終了日時
     * @return 監査ログリスト
     */
    @GetMapping("/user/{userId}")
    @Operation(summary = "ユーザーの操作履歴を取得する")
    public ResponseEntity<List<AuditLogResponse>> getUserActivity(
            @PathVariable String userId,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime startDate,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime endDate) {

        List<AuditLog> logs = auditLogService.getLogsByUser(userId, startDate, endDate);

        return ResponseEntity.ok(logs.stream()
                .map(AuditLogResponse::from)
                .collect(Collectors.toList()));
    }

    /**
     * 期間別の監査ログを取得
     *
     * @param startDate 開始日時
     * @param endDate 終了日時
     * @param limit 取得件数上限
     * @return 監査ログリスト
     */
    @GetMapping("/period")
    @Operation(summary = "期間別の監査ログを取得する")
    public ResponseEntity<List<AuditLogResponse>> getAuditLogsByPeriod(
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime startDate,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime endDate,
            @RequestParam(defaultValue = "100") int limit) {

        List<AuditLog> logs = auditLogService.getLogsByPeriod(startDate, endDate, limit);

        return ResponseEntity.ok(logs.stream()
                .map(AuditLogResponse::from)
                .collect(Collectors.toList()));
    }
}
