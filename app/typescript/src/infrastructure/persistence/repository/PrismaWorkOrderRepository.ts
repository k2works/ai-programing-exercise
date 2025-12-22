import { PrismaClient, WorkOrderStatus as PrismaWorkOrderStatus } from '@prisma/client';
import {
  WorkOrder,
  WorkOrderDetail,
  WorkOrderStatus,
} from '../../../domain/model/work-order/WorkOrder';
import { WorkOrderRepository } from '../../../application/port/out/WorkOrderRepository';

/**
 * Prisma を使用した作業指示リポジトリ実装
 */
export class PrismaWorkOrderRepository implements WorkOrderRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async save(workOrder: WorkOrder): Promise<WorkOrder> {
    // 既存の作業指示を確認
    const existing = await this.prisma.workOrderData.findUnique({
      where: { workOrderNumber: workOrder.作業指示番号 },
    });

    if (existing) {
      // 更新
      const updated = await this.prisma.workOrderData.update({
        where: { workOrderNumber: workOrder.作業指示番号 },
        data: {
          productionOrderNumber: workOrder.製造オーダ番号,
          workOrderDate: workOrder.作業指示日,
          itemCode: workOrder.品目コード,
          orderQuantity: workOrder.指示数量,
          completedQuantity: workOrder.完成数量,
          scheduledStartDate: workOrder.開始予定日,
          scheduledEndDate: workOrder.完了予定日,
          status: this.toDbStatus(workOrder.ステータス),
          note: workOrder.備考,
        },
        include: {
          details: true,
        },
      });

      // 既存の明細を削除
      await this.prisma.workOrderDetailData.deleteMany({
        where: { workOrderNumber: workOrder.作業指示番号 },
      });

      // 新しい明細を作成
      await this.createDetails(workOrder.作業指示番号, workOrder.明細);

      // 更新後のデータを再取得
      return (await this.findByWorkOrderNumber(workOrder.作業指示番号)) as WorkOrder;
    } else {
      // 新規作成
      await this.prisma.workOrderData.create({
        data: {
          workOrderNumber: workOrder.作業指示番号,
          productionOrderNumber: workOrder.製造オーダ番号,
          workOrderDate: workOrder.作業指示日,
          itemCode: workOrder.品目コード,
          orderQuantity: workOrder.指示数量,
          completedQuantity: workOrder.完成数量,
          scheduledStartDate: workOrder.開始予定日,
          scheduledEndDate: workOrder.完了予定日,
          status: this.toDbStatus(workOrder.ステータス),
          note: workOrder.備考,
        },
      });

      // 明細を作成
      await this.createDetails(workOrder.作業指示番号, workOrder.明細);

      // 作成後のデータを取得
      return (await this.findByWorkOrderNumber(workOrder.作業指示番号)) as WorkOrder;
    }
  }

  async findByWorkOrderNumber(workOrderNumber: string): Promise<WorkOrder | null> {
    const result = await this.prisma.workOrderData.findUnique({
      where: { workOrderNumber },
      include: {
        details: {
          orderBy: { sequence: 'asc' },
        },
      },
    });

    if (!result) {
      return null;
    }

    return this.toWorkOrder(result);
  }

  async findByProductionOrderNumber(productionOrderNumber: string): Promise<WorkOrder[]> {
    const results = await this.prisma.workOrderData.findMany({
      where: { productionOrderNumber },
      include: {
        details: {
          orderBy: { sequence: 'asc' },
        },
      },
      orderBy: { workOrderDate: 'desc' },
    });

    return results.map((result) => this.toWorkOrder(result));
  }

  async findByStatus(status: WorkOrderStatus): Promise<WorkOrder[]> {
    const results = await this.prisma.workOrderData.findMany({
      where: { status: this.toDbStatus(status) },
      include: {
        details: {
          orderBy: { sequence: 'asc' },
        },
      },
      orderBy: { workOrderDate: 'desc' },
    });

    return results.map((result) => this.toWorkOrder(result));
  }

  async findAll(): Promise<WorkOrder[]> {
    const results = await this.prisma.workOrderData.findMany({
      include: {
        details: {
          orderBy: { sequence: 'asc' },
        },
      },
      orderBy: { workOrderDate: 'desc' },
    });

    return results.map((result) => this.toWorkOrder(result));
  }

  async updateStatus(workOrderNumber: string, status: WorkOrderStatus): Promise<void> {
    await this.prisma.workOrderData.update({
      where: { workOrderNumber },
      data: { status: this.toDbStatus(status) },
    });
  }

  async updateCompletedQuantity(workOrderNumber: string, completedQuantity: number): Promise<void> {
    await this.prisma.workOrderData.update({
      where: { workOrderNumber },
      data: { completedQuantity },
    });
  }

  /**
   * 明細を一括作成
   */
  private async createDetails(
    workOrderNumber: string,
    details: WorkOrderDetail[]
  ): Promise<void> {
    await this.prisma.workOrderDetailData.createMany({
      data: details.map((detail) => ({
        workOrderNumber,
        sequence: detail.工順,
        processCode: detail.工程コード,
        scheduledStartTime: detail.開始予定日時,
        scheduledEndTime: detail.完了予定日時,
      })),
    });
  }

  /**
   * DB データをドメインモデルに変換
   */
  private toWorkOrder(data: any): WorkOrder {
    const details = data.details.map(
      (detail: any) =>
        new WorkOrderDetail(
          Number(detail.sequence),
          detail.processCode,
          detail.scheduledStartTime || undefined,
          detail.scheduledEndTime || undefined
        )
    );

    return new WorkOrder(
      data.workOrderNumber,
      data.productionOrderNumber,
      data.workOrderDate,
      data.itemCode,
      Number(data.orderQuantity),
      this.toDomainStatus(data.status),
      details,
      Number(data.completedQuantity),
      data.scheduledStartDate || undefined,
      data.scheduledEndDate || undefined,
      data.note || undefined
    );
  }

  /**
   * ドメインステータスを DB ステータスに変換
   */
  private toDbStatus(status: WorkOrderStatus): PrismaWorkOrderStatus {
    const statusMap: Record<WorkOrderStatus, PrismaWorkOrderStatus> = {
      [WorkOrderStatus.NOT_STARTED]: 'NOT_STARTED' as PrismaWorkOrderStatus,
      [WorkOrderStatus.IN_PROGRESS]: 'IN_PROGRESS' as PrismaWorkOrderStatus,
      [WorkOrderStatus.COMPLETED]: 'COMPLETED' as PrismaWorkOrderStatus,
      [WorkOrderStatus.SUSPENDED]: 'SUSPENDED' as PrismaWorkOrderStatus,
    };
    return statusMap[status];
  }

  /**
   * DB ステータスをドメインステータスに変換
   */
  private toDomainStatus(status: PrismaWorkOrderStatus): WorkOrderStatus {
    const statusMap: Record<PrismaWorkOrderStatus, WorkOrderStatus> = {
      [PrismaWorkOrderStatus.NOT_STARTED]: WorkOrderStatus.NOT_STARTED,
      [PrismaWorkOrderStatus.IN_PROGRESS]: WorkOrderStatus.IN_PROGRESS,
      [PrismaWorkOrderStatus.COMPLETED]: WorkOrderStatus.COMPLETED,
      [PrismaWorkOrderStatus.SUSPENDED]: WorkOrderStatus.SUSPENDED,
    };
    return statusMap[status];
  }
}
