import { WorkOrder, WorkOrderDetail, WorkOrderStatus } from '../../domain/model/work-order/WorkOrder';
import {
  CreateWorkOrderCommand,
  WorkOrderQuery,
  WorkOrderUseCase,
} from '../port/in/WorkOrderUseCase';
import { WorkOrderRepository } from '../port/out/WorkOrderRepository';

/**
 * 作業指示サービス (Application Service)
 */
export class WorkOrderService implements WorkOrderUseCase {
  constructor(private readonly workOrderRepository: WorkOrderRepository) {}

  async createWorkOrder(command: CreateWorkOrderCommand): Promise<WorkOrder> {
    // 明細を作成
    const details = command.明細.map((detail) =>
      WorkOrderDetail.create({
        工順: detail.工順,
        工程コード: detail.工程コード,
        開始予定日時: detail.開始予定日時,
        完了予定日時: detail.完了予定日時,
      })
    );

    // 作業指示を作成
    const workOrder = WorkOrder.create({
      作業指示番号: command.作業指示番号,
      製造オーダ番号: command.製造オーダ番号,
      作業指示日: command.作業指示日,
      品目コード: command.品目コード,
      指示数量: command.指示数量,
      ステータス: command.ステータス,
      完成数量: command.完成数量,
      開始予定日: command.開始予定日,
      完了予定日: command.完了予定日,
      備考: command.備考,
      明細: details,
    });

    return await this.workOrderRepository.save(workOrder);
  }

  async getWorkOrder(workOrderNumber: string): Promise<WorkOrder | null> {
    return await this.workOrderRepository.findByWorkOrderNumber(workOrderNumber);
  }

  async getWorkOrdersByProductionOrder(productionOrderNumber: string): Promise<WorkOrder[]> {
    return await this.workOrderRepository.findByProductionOrderNumber(productionOrderNumber);
  }

  async getWorkOrdersByStatus(status: WorkOrderStatus): Promise<WorkOrder[]> {
    return await this.workOrderRepository.findByStatus(status);
  }

  async queryWorkOrders(query: WorkOrderQuery): Promise<WorkOrder[]> {
    // 製造オーダ番号とステータスの両方が指定されている場合
    if (query.製造オーダ番号 && query.ステータス) {
      const workOrders = await this.workOrderRepository.findByProductionOrderNumber(
        query.製造オーダ番号
      );
      return workOrders.filter((wo) => wo.ステータス === query.ステータス);
    }

    // 製造オーダ番号のみ
    if (query.製造オーダ番号) {
      return await this.workOrderRepository.findByProductionOrderNumber(query.製造オーダ番号);
    }

    // ステータスのみ
    if (query.ステータス) {
      return await this.workOrderRepository.findByStatus(query.ステータス);
    }

    // どちらも指定されていない場合は全件
    return await this.workOrderRepository.findAll();
  }

  async updateWorkOrderStatus(workOrderNumber: string, status: WorkOrderStatus): Promise<void> {
    await this.workOrderRepository.updateStatus(workOrderNumber, status);
  }

  async updateCompletedQuantity(workOrderNumber: string, completedQuantity: number): Promise<void> {
    await this.workOrderRepository.updateCompletedQuantity(workOrderNumber, completedQuantity);
  }
}
