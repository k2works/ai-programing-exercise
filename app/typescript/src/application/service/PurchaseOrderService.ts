import {
  PurchaseOrder,
  PurchaseOrderDetail,
  PurchaseOrderStatus,
} from '../../domain/model/purchase/PurchaseOrder';
import {
  CreatePurchaseOrderCommand,
  PurchaseOrderQuery,
  PurchaseOrderUseCase,
} from '../port/in/PurchaseOrderUseCase';
import { PurchaseOrderRepository } from '../port/out/PurchaseOrderRepository';

/**
 * 発注アプリケーションサービス
 */
export class PurchaseOrderService implements PurchaseOrderUseCase {
  constructor(private readonly purchaseOrderRepository: PurchaseOrderRepository) {}

  async createPurchaseOrder(command: CreatePurchaseOrderCommand): Promise<PurchaseOrder> {
    // コマンドからドメインモデルを構築
    const details = command.明細.map((detail) =>
      PurchaseOrderDetail.create({
        行番号: detail.行番号,
        品目コード: detail.品目コード,
        受入予定日: detail.受入予定日,
        発注単価: detail.発注単価,
        発注数量: detail.発注数量,
        製造オーダ番号: detail.製造オーダ番号,
        納入場所コード: detail.納入場所コード,
        回答納期: detail.回答納期,
        入荷済数量: detail.入荷済数量,
        検査済数量: detail.検査済数量,
        検収済数量: detail.検収済数量,
        消費税金額: detail.消費税金額,
        完了フラグ: detail.完了フラグ,
        明細備考: detail.明細備考,
      })
    );

    const purchaseOrder = PurchaseOrder.create({
      発注番号: command.発注番号,
      発注日: command.発注日,
      取引先コード: command.取引先コード,
      ステータス: command.ステータス,
      明細: details,
      発注担当者コード: command.発注担当者コード,
      発注部門コード: command.発注部門コード,
      備考: command.備考,
    });

    // リポジトリに保存
    return await this.purchaseOrderRepository.save(purchaseOrder);
  }

  async getPurchaseOrder(orderNumber: string): Promise<PurchaseOrder | null> {
    return await this.purchaseOrderRepository.findByOrderNumber(orderNumber);
  }

  async getPurchaseOrdersBySupplier(supplierCode: string): Promise<PurchaseOrder[]> {
    return await this.purchaseOrderRepository.findBySupplier(supplierCode);
  }

  async getPurchaseOrdersByStatus(status: PurchaseOrderStatus): Promise<PurchaseOrder[]> {
    return await this.purchaseOrderRepository.findByStatus(status);
  }

  async queryPurchaseOrders(query: PurchaseOrderQuery): Promise<PurchaseOrder[]> {
    // 取引先コードとステータスの両方が指定された場合
    if (query.取引先コード && query.ステータス) {
      const orders = await this.purchaseOrderRepository.findBySupplier(query.取引先コード);
      return orders.filter((order) => order.ステータス === query.ステータス);
    }

    // 取引先コードのみ指定
    if (query.取引先コード) {
      return await this.purchaseOrderRepository.findBySupplier(query.取引先コード);
    }

    // ステータスのみ指定
    if (query.ステータス) {
      return await this.purchaseOrderRepository.findByStatus(query.ステータス);
    }

    // 条件なしの場合は全件取得
    return await this.purchaseOrderRepository.findAll();
  }

  async updatePurchaseOrderStatus(
    orderNumber: string,
    status: PurchaseOrderStatus
  ): Promise<void> {
    await this.purchaseOrderRepository.updateStatus(orderNumber, status);
  }
}
