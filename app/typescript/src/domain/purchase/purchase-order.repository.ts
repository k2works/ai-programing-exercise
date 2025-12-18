import { PrismaClient } from '@prisma/client'

// 発注ステータス
export const PurchaseOrderStatus = {
  DRAFT: 'DRAFT', // 作成中
  ORDERED: 'ORDERED', // 発注済
  PARTIALLY_RECEIVED: 'PARTIALLY_RECEIVED', // 一部入荷
  FULLY_RECEIVED: 'FULLY_RECEIVED', // 入荷完了
  INSPECTED: 'INSPECTED', // 検収完了
  CANCELLED: 'CANCELLED', // 取消
} as const

export type PurchaseOrderStatusValue =
  (typeof PurchaseOrderStatus)[keyof typeof PurchaseOrderStatus]

// 発注データの入力型
export interface CreatePurchaseOrderInput {
  orderNumber: string
  orderDate: Date
  supplierCode: string
  purchaserCode?: string
  departmentCode?: string
  status?: PurchaseOrderStatusValue
  note?: string
  createdBy?: string
}

// 発注データの出力型
export interface PurchaseOrder {
  id: number
  orderNumber: string
  orderDate: Date
  supplierCode: string
  purchaserCode: string | null
  departmentCode: string | null
  status: string
  note: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

// 発注明細の入力型
export interface CreatePurchaseOrderDetailInput {
  orderNumber: string
  lineNumber: number
  productionOrderNumber?: string
  deliveryLocationCode?: string
  itemCode: string
  isMiscellaneous?: boolean
  scheduledReceiptDate: Date
  confirmedDeliveryDate?: Date
  orderUnitPrice: number
  orderQuantity: number
  orderAmount?: number
  taxAmount?: number
  detailNote?: string
  createdBy?: string
}

// 発注明細の出力型
export interface PurchaseOrderDetail {
  id: number
  orderNumber: string
  lineNumber: number
  productionOrderNumber: string | null
  deliveryLocationCode: string | null
  itemCode: string
  isMiscellaneous: boolean
  scheduledReceiptDate: Date
  confirmedDeliveryDate: Date | null
  orderUnitPrice: number
  orderQuantity: number
  receivedQuantity: number
  inspectedQuantity: number
  acceptedQuantity: number
  orderAmount: number
  taxAmount: number
  isCompleted: boolean
  detailNote: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export class PurchaseOrderRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreatePurchaseOrderInput): Promise<PurchaseOrder> {
    const result = await this.prisma.$queryRaw<PurchaseOrder[]>`
      INSERT INTO purchase_orders (
        order_number, order_date, supplier_code, purchaser_code,
        department_code, status, note, created_by
      )
      VALUES (
        ${input.orderNumber}, ${input.orderDate}, ${input.supplierCode},
        ${input.purchaserCode ?? null}, ${input.departmentCode ?? null},
        ${input.status ?? PurchaseOrderStatus.DRAFT}::purchase_order_status,
        ${input.note ?? null}, ${input.createdBy ?? null}
      )
      RETURNING
        id,
        order_number as "orderNumber",
        order_date as "orderDate",
        supplier_code as "supplierCode",
        purchaser_code as "purchaserCode",
        department_code as "departmentCode",
        status,
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }

  async findByNumber(orderNumber: string): Promise<PurchaseOrder | null> {
    const result = await this.prisma.$queryRaw<PurchaseOrder[]>`
      SELECT
        id,
        order_number as "orderNumber",
        order_date as "orderDate",
        supplier_code as "supplierCode",
        purchaser_code as "purchaserCode",
        department_code as "departmentCode",
        status,
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM purchase_orders
      WHERE order_number = ${orderNumber}
    `
    return result.length > 0 ? result[0] : null
  }

  async updateStatus(
    id: number,
    status: PurchaseOrderStatusValue,
    updatedBy?: string
  ): Promise<PurchaseOrder> {
    const result = await this.prisma.$queryRaw<PurchaseOrder[]>`
      UPDATE purchase_orders
      SET
        status = ${status}::purchase_order_status,
        updated_by = ${updatedBy ?? null},
        updated_at = CURRENT_TIMESTAMP
      WHERE id = ${id}
      RETURNING
        id,
        order_number as "orderNumber",
        order_date as "orderDate",
        supplier_code as "supplierCode",
        purchaser_code as "purchaserCode",
        department_code as "departmentCode",
        status,
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }

  async createDetail(input: CreatePurchaseOrderDetailInput): Promise<PurchaseOrderDetail> {
    // 金額計算
    const orderAmount = input.orderAmount ?? input.orderUnitPrice * input.orderQuantity
    const taxAmount = input.taxAmount ?? 0

    const result = await this.prisma.$queryRaw<PurchaseOrderDetail[]>`
      INSERT INTO purchase_order_details (
        order_number, line_number, production_order_number,
        delivery_location_code, item_code, is_miscellaneous,
        scheduled_receipt_date, confirmed_delivery_date,
        order_unit_price, order_quantity, order_amount, tax_amount,
        detail_note, created_by
      )
      VALUES (
        ${input.orderNumber}, ${input.lineNumber},
        ${input.productionOrderNumber ?? null},
        ${input.deliveryLocationCode ?? null}, ${input.itemCode},
        ${input.isMiscellaneous ?? false},
        ${input.scheduledReceiptDate}, ${input.confirmedDeliveryDate ?? null},
        ${input.orderUnitPrice}, ${input.orderQuantity},
        ${orderAmount}, ${taxAmount},
        ${input.detailNote ?? null}, ${input.createdBy ?? null}
      )
      RETURNING
        id,
        order_number as "orderNumber",
        line_number as "lineNumber",
        production_order_number as "productionOrderNumber",
        delivery_location_code as "deliveryLocationCode",
        item_code as "itemCode",
        is_miscellaneous as "isMiscellaneous",
        scheduled_receipt_date as "scheduledReceiptDate",
        confirmed_delivery_date as "confirmedDeliveryDate",
        order_unit_price as "orderUnitPrice",
        order_quantity as "orderQuantity",
        received_quantity as "receivedQuantity",
        inspected_quantity as "inspectedQuantity",
        accepted_quantity as "acceptedQuantity",
        order_amount as "orderAmount",
        tax_amount as "taxAmount",
        is_completed as "isCompleted",
        detail_note as "detailNote",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }

  async findDetailsByOrderNumber(orderNumber: string): Promise<PurchaseOrderDetail[]> {
    const result = await this.prisma.$queryRaw<PurchaseOrderDetail[]>`
      SELECT
        id,
        order_number as "orderNumber",
        line_number as "lineNumber",
        production_order_number as "productionOrderNumber",
        delivery_location_code as "deliveryLocationCode",
        item_code as "itemCode",
        is_miscellaneous as "isMiscellaneous",
        scheduled_receipt_date as "scheduledReceiptDate",
        confirmed_delivery_date as "confirmedDeliveryDate",
        order_unit_price as "orderUnitPrice",
        order_quantity as "orderQuantity",
        received_quantity as "receivedQuantity",
        inspected_quantity as "inspectedQuantity",
        accepted_quantity as "acceptedQuantity",
        order_amount as "orderAmount",
        tax_amount as "taxAmount",
        is_completed as "isCompleted",
        detail_note as "detailNote",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM purchase_order_details
      WHERE order_number = ${orderNumber}
      ORDER BY line_number
    `
    return result
  }
}
