package com.example.production.infrastructure.rest.controller;

import com.example.production.application.port.in.WorkOrderUseCase;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;
import com.example.production.infrastructure.rest.dto.CreateWorkOrderRequest;
import com.example.production.infrastructure.rest.dto.UpdateProgressRequest;
import com.example.production.infrastructure.rest.dto.WorkOrderResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 作業指示 Controller（Input Adapter）
 */
@RestController
@RequestMapping("/api/work-orders")
@Tag(name = "work-orders", description = "作業指示 API")
@RequiredArgsConstructor
public class WorkOrderController {

    private final WorkOrderUseCase useCase;

    @GetMapping
    @Operation(summary = "作業指示一覧の取得")
    public ResponseEntity<List<WorkOrderResponse>> getAllWorkOrders(
            @Parameter(description = "ステータスでフィルタリング")
            @RequestParam(required = false) WorkOrderStatus status) {

        List<WorkOrder> workOrders;
        if (status != null) {
            workOrders = useCase.getWorkOrdersByStatus(status);
        } else {
            workOrders = useCase.getAllWorkOrders();
        }

        return ResponseEntity.ok(workOrders.stream()
                .map(WorkOrderResponse::from)
                .toList());
    }

    @GetMapping("/{workOrderNumber}")
    @Operation(summary = "作業指示詳細の取得")
    public ResponseEntity<WorkOrderResponse> getWorkOrder(
            @PathVariable String workOrderNumber) {
        WorkOrder workOrder = useCase.getWorkOrder(workOrderNumber);
        return ResponseEntity.ok(WorkOrderResponse.from(workOrder));
    }

    @PostMapping
    @Operation(summary = "作業指示の登録")
    public ResponseEntity<WorkOrderResponse> createWorkOrder(
            @Valid @RequestBody CreateWorkOrderRequest request) {
        WorkOrder workOrder = useCase.createWorkOrder(request.toCommand());
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(WorkOrderResponse.from(workOrder));
    }

    @PostMapping("/{workOrderNumber}/start")
    @Operation(summary = "作業の開始")
    public ResponseEntity<WorkOrderResponse> startWork(
            @PathVariable String workOrderNumber) {
        WorkOrder workOrder = useCase.startWork(workOrderNumber);
        return ResponseEntity.ok(WorkOrderResponse.from(workOrder));
    }

    @PostMapping("/{workOrderNumber}/complete")
    @Operation(summary = "作業の完了")
    public ResponseEntity<WorkOrderResponse> completeWork(
            @PathVariable String workOrderNumber) {
        WorkOrder workOrder = useCase.completeWork(workOrderNumber);
        return ResponseEntity.ok(WorkOrderResponse.from(workOrder));
    }

    @PatchMapping("/{workOrderNumber}/progress")
    @Operation(summary = "作業進捗の更新")
    public ResponseEntity<WorkOrderResponse> updateProgress(
            @PathVariable String workOrderNumber,
            @Valid @RequestBody UpdateProgressRequest request) {
        WorkOrder workOrder = useCase.updateProgress(workOrderNumber, request.getStatus());
        return ResponseEntity.ok(WorkOrderResponse.from(workOrder));
    }
}
