import { PrismaClient, PurchaseOrderStatus as PrismaPurchaseOrderStatus } from '@prisma/client';
import {
  PurchaseOrder,
  PurchaseOrderDetail,
  PurchaseOrderStatus,
} from '../../../domain/model/purchase/PurchaseOrder';
import { PurchaseOrderRepository } from '../../../application/port/out/PurchaseOrderRepository';

/**
 * Prisma を使用した発注リポジトリ実装
 */
export class PrismaPurchaseOrderRepository implements PurchaseOrderRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async save(purchaseOrder: PurchaseOrder): Promise<PurchaseOrder> {
    // 既存の発注を確認
    const existing = await this.prisma.purchaseOrder.findUnique({
      where: { orderNumber: purchaseOrder.発注番号 },
    });

    if (existing) {
      // 更新
      const updated = await this.prisma.purchaseOrder.update({
        where: { orderNumber: purchaseOrder.発注番号 },
        data: {
          orderDate: purchaseOrder.発注日,
          supplierCode: purchaseOrder.取引先コード,
          purchaserCode: purchaseOrder.発注担当者コード,
          departmentCode: purchaseOrder.発注部門コード,
          status: this.toDbStatus(purchaseOrder.ステータス),
          note: purchaseOrder.備考,
        },
        include: {
          details: true,
        },
      });

      // 既存の明細を削除
      await this.prisma.purchaseOrderDetail.deleteMany({
        where: { orderNumber: purchaseOrder.発注番号 },
      });

      // 新しい明細を作成
      await this.createDetails(purchaseOrder.発注番号, purchaseOrder.明細);

      // 更新後のデータを再取得
      return await this.findByOrderNumber(purchaseOrder.発注番号) as PurchaseOrder;
    } else {
      // 新規作成
      await this.prisma.purchaseOrder.create({
        data: {
          orderNumber: purchaseOrder.発注番号,
          orderDate: purchaseOrder.発注日,
          supplierCode: purchaseOrder.取引先コード,
          purchaserCode: purchaseOrder.発注担当者コード,
          departmentCode: purchaseOrder.発注部門コード,
          status: this.toDbStatus(purchaseOrder.ステータス),
          note: purchaseOrder.備考,
        },
      });

      // 明細を作成
      await this.createDetails(purchaseOrder.発注番号, purchaseOrder.明細);

      // 作成後のデータを取得
      return await this.findByOrderNumber(purchaseOrder.発注番号) as PurchaseOrder;
    }
  }

  async findByOrderNumber(orderNumber: string): Promise<PurchaseOrder | null> {
    const result = await this.prisma.purchaseOrder.findUnique({
      where: { orderNumber },
      include: {
        details: {
          orderBy: { lineNumber: 'asc' },
        },
      },
    });

    if (!result) {
      return null;
    }

    return this.toPurchaseOrder(result);
  }

  async findBySupplier(supplierCode: string): Promise<PurchaseOrder[]> {
    const results = await this.prisma.purchaseOrder.findMany({
      where: { supplierCode },
      include: {
        details: {
          orderBy: { lineNumber: 'asc' },
        },
      },
      orderBy: { orderDate: 'desc' },
    });

    return results.map((result) => this.toPurchaseOrder(result));
  }

  async findByStatus(status: PurchaseOrderStatus): Promise<PurchaseOrder[]> {
    const results = await this.prisma.purchaseOrder.findMany({
      where: { status: this.toDbStatus(status) },
      include: {
        details: {
          orderBy: { lineNumber: 'asc' },
        },
      },
      orderBy: { orderDate: 'desc' },
    });

    return results.map((result) => this.toPurchaseOrder(result));
  }

  async findAll(): Promise<PurchaseOrder[]> {
    const results = await this.prisma.purchaseOrder.findMany({
      include: {
        details: {
          orderBy: { lineNumber: 'asc' },
        },
      },
      orderBy: { orderDate: 'desc' },
    });

    return results.map((result) => this.toPurchaseOrder(result));
  }

  async updateStatus(orderNumber: string, status: PurchaseOrderStatus): Promise<void> {
    await this.prisma.purchaseOrder.update({
      where: { orderNumber },
      data: { status: this.toDbStatus(status) },
    });
  }

  /**
   * 明細を一括作成
   */
  private async createDetails(
    orderNumber: string,
    details: PurchaseOrderDetail[]
  ): Promise<void> {
    await this.prisma.purchaseOrderDetail.createMany({
      data: details.map((detail) => ({
        orderNumber,
        lineNumber: detail.行番号,
        productionOrderNumber: detail.製造オーダ番号,
        deliveryLocationCode: detail.納入場所コード,
        itemCode: detail.品目コード,
        scheduledReceiptDate: detail.受入予定日,
        confirmedDeliveryDate: detail.回答納期,
        orderUnitPrice: detail.発注単価,
        orderQuantity: detail.発注数量,
        receivedQuantity: detail.入荷済数量,
        inspectedQuantity: detail.検査済数量,
        acceptedQuantity: detail.検収済数量,
        orderAmount: detail.発注金額,
        taxAmount: detail.消費税金額,
        isCompleted: detail.完了フラグ,
        detailNote: detail.明細備考,
      })),
    });
  }

  /**
   * DB データをドメインモデルに変換
   */
  private toPurchaseOrder(data: any): PurchaseOrder {
    const details = data.details.map(
      (detail: any) =>
        new PurchaseOrderDetail(
          Number(detail.lineNumber),
          detail.itemCode,
          detail.scheduledReceiptDate,
          Number(detail.orderUnitPrice),
          Number(detail.orderQuantity),
          Number(detail.orderAmount),
          detail.productionOrderNumber || undefined,
          detail.deliveryLocationCode || undefined,
          detail.confirmedDeliveryDate || undefined,
          Number(detail.receivedQuantity),
          Number(detail.inspectedQuantity),
          Number(detail.acceptedQuantity),
          Number(detail.taxAmount),
          detail.isCompleted,
          detail.detailNote || undefined
        )
    );

    return new PurchaseOrder(
      data.orderNumber,
      data.orderDate,
      data.supplierCode,
      this.toDomainStatus(data.status),
      details,
      data.purchaserCode || undefined,
      data.departmentCode || undefined,
      data.note || undefined
    );
  }

  /**
   * ドメインステータスを DB ステータスに変換
   * データベースは英語のenum値を保存するため、直接文字列リテラルを返す
   */
  private toDbStatus(status: PurchaseOrderStatus): PrismaPurchaseOrderStatus {
    const statusMap: Record<PurchaseOrderStatus, PrismaPurchaseOrderStatus> = {
      [PurchaseOrderStatus.DRAFT]: 'DRAFT' as PrismaPurchaseOrderStatus,
      [PurchaseOrderStatus.ORDERED]: 'ORDERED' as PrismaPurchaseOrderStatus,
      [PurchaseOrderStatus.PARTIALLY_RECEIVED]: 'PARTIALLY_RECEIVED' as PrismaPurchaseOrderStatus,
      [PurchaseOrderStatus.FULLY_RECEIVED]: 'FULLY_RECEIVED' as PrismaPurchaseOrderStatus,
      [PurchaseOrderStatus.INSPECTED]: 'INSPECTED' as PrismaPurchaseOrderStatus,
      [PurchaseOrderStatus.CANCELLED]: 'CANCELLED' as PrismaPurchaseOrderStatus,
    };
    return statusMap[status];
  }

  /**
   * DB ステータスをドメインステータスに変換
   */
  private toDomainStatus(status: PrismaPurchaseOrderStatus): PurchaseOrderStatus {
    const statusMap: Record<PrismaPurchaseOrderStatus, PurchaseOrderStatus> = {
      [PrismaPurchaseOrderStatus.DRAFT]: PurchaseOrderStatus.DRAFT,
      [PrismaPurchaseOrderStatus.ORDERED]: PurchaseOrderStatus.ORDERED,
      [PrismaPurchaseOrderStatus.PARTIALLY_RECEIVED]: PurchaseOrderStatus.PARTIALLY_RECEIVED,
      [PrismaPurchaseOrderStatus.FULLY_RECEIVED]: PurchaseOrderStatus.FULLY_RECEIVED,
      [PrismaPurchaseOrderStatus.INSPECTED]: PurchaseOrderStatus.INSPECTED,
      [PrismaPurchaseOrderStatus.CANCELLED]: PurchaseOrderStatus.CANCELLED,
    };
    return statusMap[status];
  }
}
