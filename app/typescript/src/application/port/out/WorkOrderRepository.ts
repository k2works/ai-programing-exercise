import { WorkOrder, WorkOrderStatus } from '../../../domain/model/work-order/WorkOrder';

/**
 * 作業指示リポジトリ（Output Port）
 */
export interface WorkOrderRepository {
  /**
   * 作業指示を保存
   */
  save(workOrder: WorkOrder): Promise<WorkOrder>;

  /**
   * 作業指示番号で作業指示を検索
   */
  findByWorkOrderNumber(workOrderNumber: string): Promise<WorkOrder | null>;

  /**
   * 製造オーダ番号で作業指示を検索
   */
  findByProductionOrderNumber(productionOrderNumber: string): Promise<WorkOrder[]>;

  /**
   * ステータス別の作業指示一覧を取得
   */
  findByStatus(status: WorkOrderStatus): Promise<WorkOrder[]>;

  /**
   * すべての作業指示を取得
   */
  findAll(): Promise<WorkOrder[]>;

  /**
   * 作業指示のステータスを更新
   */
  updateStatus(workOrderNumber: string, status: WorkOrderStatus): Promise<void>;

  /**
   * 完成数量を更新
   */
  updateCompletedQuantity(workOrderNumber: string, completedQuantity: number): Promise<void>;
}
