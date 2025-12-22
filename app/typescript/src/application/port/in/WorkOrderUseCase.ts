import { WorkOrder, WorkOrderStatus } from '../../../domain/model/work-order/WorkOrder';

/**
 * 作業指示作成コマンド
 */
export interface CreateWorkOrderCommand {
  作業指示番号: string;
  製造オーダ番号: string;
  作業指示日: Date;
  品目コード: string;
  指示数量: number;
  ステータス?: WorkOrderStatus;
  完成数量?: number;
  開始予定日?: Date;
  完了予定日?: Date;
  備考?: string;
  明細: {
    工順: number;
    工程コード: string;
    開始予定日時?: Date;
    完了予定日時?: Date;
  }[];
}

/**
 * 作業指示クエリ
 */
export interface WorkOrderQuery {
  製造オーダ番号?: string;
  ステータス?: WorkOrderStatus;
}

/**
 * 作業指示ユースケース（Input Port）
 */
export interface WorkOrderUseCase {
  /**
   * 作業指示を作成
   */
  createWorkOrder(command: CreateWorkOrderCommand): Promise<WorkOrder>;

  /**
   * 作業指示番号で作業指示を取得
   */
  getWorkOrder(workOrderNumber: string): Promise<WorkOrder | null>;

  /**
   * 製造オーダ番号で作業指示一覧を取得
   */
  getWorkOrdersByProductionOrder(productionOrderNumber: string): Promise<WorkOrder[]>;

  /**
   * ステータス別の作業指示一覧を取得
   */
  getWorkOrdersByStatus(status: WorkOrderStatus): Promise<WorkOrder[]>;

  /**
   * 作業指示を検索
   */
  queryWorkOrders(query: WorkOrderQuery): Promise<WorkOrder[]>;

  /**
   * 作業指示のステータスを更新
   */
  updateWorkOrderStatus(workOrderNumber: string, status: WorkOrderStatus): Promise<void>;

  /**
   * 完成数量を更新
   */
  updateCompletedQuantity(workOrderNumber: string, completedQuantity: number): Promise<void>;
}
