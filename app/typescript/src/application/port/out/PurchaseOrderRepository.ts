import { PurchaseOrder, PurchaseOrderStatus } from '../../../domain/model/purchase/PurchaseOrder';

/**
 * 発注リポジトリインターフェース (Output Port)
 */
export interface PurchaseOrderRepository {
  /**
   * 発注を登録
   */
  save(purchaseOrder: PurchaseOrder): Promise<PurchaseOrder>;

  /**
   * 発注番号で検索
   */
  findByOrderNumber(orderNumber: string): Promise<PurchaseOrder | null>;

  /**
   * 取引先別の発注一覧を取得
   */
  findBySupplier(supplierCode: string): Promise<PurchaseOrder[]>;

  /**
   * ステータス別の発注一覧を取得
   */
  findByStatus(status: PurchaseOrderStatus): Promise<PurchaseOrder[]>;

  /**
   * 全発注一覧を取得
   */
  findAll(): Promise<PurchaseOrder[]>;

  /**
   * ステータスを更新
   */
  updateStatus(orderNumber: string, status: PurchaseOrderStatus): Promise<void>;
}
