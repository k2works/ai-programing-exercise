package com.example.production.application.port.in;

import com.example.production.application.port.in.command.WorkOrderCreateCommand;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;

import java.util.List;

/**
 * 作業指示ユースケース（Input Port）
 */
public interface WorkOrderUseCase {

    /**
     * 作業指示を作成する
     */
    WorkOrder createWorkOrder(WorkOrderCreateCommand command);

    /**
     * 作業指示を取得する
     */
    WorkOrder getWorkOrder(String workOrderNumber);

    /**
     * すべての作業指示を取得する
     */
    List<WorkOrder> getAllWorkOrders();

    /**
     * ステータス別に作業指示を取得する
     */
    List<WorkOrder> getWorkOrdersByStatus(WorkOrderStatus status);

    /**
     * 作業を開始する
     */
    WorkOrder startWork(String workOrderNumber);

    /**
     * 作業を完了する
     */
    WorkOrder completeWork(String workOrderNumber);

    /**
     * 進捗を更新する
     */
    WorkOrder updateProgress(String workOrderNumber, WorkOrderStatus status);
}
