package com.example.production.application.service;

import com.example.production.application.port.in.WorkOrderUseCase;
import com.example.production.application.port.in.command.WorkOrderCreateCommand;
import com.example.production.application.port.out.*;
import com.example.production.domain.exception.WorkOrderNotFoundException;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.process.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * 作業指示サービス
 */
@Service
@RequiredArgsConstructor
public class WorkOrderService implements WorkOrderUseCase {

    private final WorkOrderRepository workOrderRepository;
    private final WorkOrderDetailRepository workOrderDetailRepository;
    private final OrderRepository orderRepository;
    private final RoutingRepository routingRepository;

    /**
     * 作業指示番号を生成する
     */
    private String generateWorkOrderNumber(LocalDate workOrderDate) {
        String prefix = "WO-" + workOrderDate.format(DateTimeFormatter.ofPattern("yyyyMM")) + "-";
        return workOrderRepository.findLatestWorkOrderNumber(prefix)
                .map(latest -> {
                    int currentSequence = Integer.parseInt(latest.substring(latest.length() - 4));
                    return prefix + String.format("%04d", currentSequence + 1);
                })
                .orElse(prefix + "0001");
    }

    /**
     * 作業指示を作成する
     */
    @Transactional
    public WorkOrder createWorkOrder(WorkOrderCreateCommand command) {
        // オーダ情報を取得
        Order order = orderRepository.findByOrderNumber(command.getOrderNumber())
                .orElseThrow(() -> new IllegalArgumentException("Order not found: " + command.getOrderNumber()));

        // 工程表を取得
        List<Routing> routings = routingRepository.findByItemCode(order.getItemCode());
        if (routings.isEmpty()) {
            throw new IllegalArgumentException("Routing not found for item: " + order.getItemCode());
        }

        String workOrderNumber = generateWorkOrderNumber(command.getWorkOrderDate());

        // 作業指示ヘッダを作成
        WorkOrder workOrder = WorkOrder.builder()
                .workOrderNumber(workOrderNumber)
                .orderNumber(command.getOrderNumber())
                .workOrderDate(command.getWorkOrderDate())
                .itemCode(order.getItemCode())
                .orderQuantity(order.getPlanQuantity())
                .locationCode(command.getLocationCode())
                .plannedStartDate(command.getPlannedStartDate())
                .plannedEndDate(command.getPlannedEndDate())
                .completedQuantity(BigDecimal.ZERO)
                .totalGoodQuantity(BigDecimal.ZERO)
                .totalDefectQuantity(BigDecimal.ZERO)
                .status(WorkOrderStatus.NOT_STARTED)
                .completedFlag(false)
                .remarks(command.getRemarks())
                .build();
        workOrderRepository.save(workOrder);

        // 作業指示明細を作成
        List<WorkOrderDetail> details = new ArrayList<>();
        for (Routing routing : routings) {
            WorkOrderDetail detail = WorkOrderDetail.builder()
                    .workOrderNumber(workOrderNumber)
                    .sequence(routing.getSequence())
                    .processCode(routing.getProcessCode())
                    .build();
            workOrderDetailRepository.save(detail);
            details.add(detail);
        }

        workOrder.setDetails(details);
        return workOrder;
    }

    /**
     * 作業を開始する
     */
    @Transactional
    public WorkOrder startWork(String workOrderNumber) {
        WorkOrder workOrder = workOrderRepository.findByWorkOrderNumber(workOrderNumber)
                .orElseThrow(() -> new IllegalArgumentException("Work order not found: " + workOrderNumber));

        if (workOrder.getStatus() != WorkOrderStatus.NOT_STARTED) {
            throw new IllegalStateException("Only NOT_STARTED work orders can be started");
        }

        workOrderRepository.startWork(workOrderNumber, LocalDate.now());
        return workOrderRepository.findByWorkOrderNumber(workOrderNumber)
                .orElseThrow(() -> new IllegalArgumentException("Work order not found: " + workOrderNumber));
    }

    /**
     * 作業を完了する
     */
    @Transactional
    public WorkOrder completeWork(String workOrderNumber) {
        WorkOrder workOrder = workOrderRepository.findByWorkOrderNumber(workOrderNumber)
                .orElseThrow(() -> new IllegalArgumentException("Work order not found: " + workOrderNumber));

        if (workOrder.getStatus() != WorkOrderStatus.IN_PROGRESS) {
            throw new IllegalStateException("Only IN_PROGRESS work orders can be completed");
        }

        workOrderRepository.completeWork(workOrderNumber, LocalDate.now());
        return workOrderRepository.findByWorkOrderNumber(workOrderNumber)
                .orElseThrow(() -> new IllegalArgumentException("Work order not found: " + workOrderNumber));
    }

    /**
     * 作業指示を検索する
     */
    public WorkOrder findByWorkOrderNumber(String workOrderNumber) {
        WorkOrder workOrder = workOrderRepository.findByWorkOrderNumber(workOrderNumber)
                .orElse(null);
        if (workOrder != null) {
            workOrder.setDetails(workOrderDetailRepository.findByWorkOrderNumber(workOrderNumber));
        }
        return workOrder;
    }

    /**
     * 作業指示を取得する
     */
    @Override
    public WorkOrder getWorkOrder(String workOrderNumber) {
        WorkOrder workOrder = workOrderRepository.findByWorkOrderNumber(workOrderNumber)
                .orElseThrow(() -> new WorkOrderNotFoundException(workOrderNumber));
        workOrder.setDetails(workOrderDetailRepository.findByWorkOrderNumber(workOrderNumber));
        return workOrder;
    }

    /**
     * すべての作業指示を取得する
     */
    @Override
    public List<WorkOrder> getAllWorkOrders() {
        return workOrderRepository.findAll();
    }

    /**
     * ステータス別に作業指示を取得する
     */
    @Override
    public List<WorkOrder> getWorkOrdersByStatus(WorkOrderStatus status) {
        return workOrderRepository.findByStatus(status);
    }

    /**
     * 進捗を更新する
     */
    @Override
    @Transactional
    public WorkOrder updateProgress(String workOrderNumber, WorkOrderStatus status) {
        WorkOrder workOrder = workOrderRepository.findByWorkOrderNumber(workOrderNumber)
                .orElseThrow(() -> new WorkOrderNotFoundException(workOrderNumber));

        switch (status) {
            case IN_PROGRESS:
                if (workOrder.getStatus() == WorkOrderStatus.NOT_STARTED) {
                    workOrderRepository.startWork(workOrderNumber, LocalDate.now());
                }
                break;
            case COMPLETED:
                if (workOrder.getStatus() == WorkOrderStatus.IN_PROGRESS) {
                    workOrderRepository.completeWork(workOrderNumber, LocalDate.now());
                }
                break;
            default:
                throw new IllegalStateException("Invalid status transition: " + status);
        }

        return getWorkOrder(workOrderNumber);
    }
}
