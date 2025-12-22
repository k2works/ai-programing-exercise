import { PurchaseOrder, PurchaseOrderStatus } from '../../../domain/model/purchase/PurchaseOrder';

/**
 * 発注作成コマンド
 */
export interface CreatePurchaseOrderCommand {
  発注番号: string;
  発注日: Date;
  取引先コード: string;
  ステータス?: PurchaseOrderStatus;
  発注担当者コード?: string;
  発注部門コード?: string;
  備考?: string;
  明細: {
    行番号: number;
    品目コード: string;
    受入予定日: Date;
    発注単価: number;
    発注数量: number;
    製造オーダ番号?: string;
    納入場所コード?: string;
    回答納期?: Date;
    入荷済数量?: number;
    検査済数量?: number;
    検収済数量?: number;
    消費税金額?: number;
    完了フラグ?: boolean;
    明細備考?: string;
  }[];
}

/**
 * 発注検索クエリ
 */
export interface PurchaseOrderQuery {
  取引先コード?: string;
  ステータス?: PurchaseOrderStatus;
}

/**
 * 発注ユースケースインターフェース (Input Port)
 */
export interface PurchaseOrderUseCase {
  /**
   * 発注を作成
   */
  createPurchaseOrder(command: CreatePurchaseOrderCommand): Promise<PurchaseOrder>;

  /**
   * 発注番号で発注を取得
   */
  getPurchaseOrder(orderNumber: string): Promise<PurchaseOrder | null>;

  /**
   * 取引先別の発注一覧を取得
   */
  getPurchaseOrdersBySupplier(supplierCode: string): Promise<PurchaseOrder[]>;

  /**
   * ステータス別の発注一覧を取得
   */
  getPurchaseOrdersByStatus(status: PurchaseOrderStatus): Promise<PurchaseOrder[]>;

  /**
   * 発注を検索
   */
  queryPurchaseOrders(query: PurchaseOrderQuery): Promise<PurchaseOrder[]>;

  /**
   * 発注ステータスを更新
   */
  updatePurchaseOrderStatus(orderNumber: string, status: PurchaseOrderStatus): Promise<void>;
}
