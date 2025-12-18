import { PrismaClient } from '@prisma/client'

// オーダ種別
export const OrderType = {
  PURCHASE: 'PURCHASE', // 購買
  MANUFACTURING: 'MANUFACTURING', // 製造
} as const

export type OrderTypeValue = (typeof OrderType)[keyof typeof OrderType]

// 計画ステータス（MPSと共通）
export const PlanStatus = {
  DRAFT: 'DRAFT', // 草案
  CONFIRMED: 'CONFIRMED', // 確定
  EXPANDED: 'EXPANDED', // 展開済
  CANCELLED: 'CANCELLED', // 取消
} as const

export type PlanStatusValue = (typeof PlanStatus)[keyof typeof PlanStatus]

// オーダの入力型
export interface CreateOrderInput {
  orderNumber: string
  orderType: OrderTypeValue
  itemCode: string
  startDate: Date
  dueDate: Date
  expirationDate?: Date
  plannedQuantity: number
  locationCode: string
  status?: PlanStatusValue
  mpsId?: number
  parentOrderId?: number
  createdBy?: string
}

// オーダの出力型
export interface Order {
  id: number
  orderNumber: string
  orderType: string
  itemCode: string
  startDate: Date
  dueDate: Date
  expirationDate: Date | null
  plannedQuantity: number
  locationCode: string
  status: string
  mpsId: number | null
  parentOrderId: number | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export class OrderRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateOrderInput): Promise<Order> {
    const result = await this.prisma.$queryRaw<Order[]>`
      INSERT INTO orders (
        order_number, order_type, item_code, start_date, due_date,
        expiration_date, planned_quantity, location_code, status,
        mps_id, parent_order_id, created_by
      )
      VALUES (
        ${input.orderNumber}, ${input.orderType}::order_type, ${input.itemCode},
        ${input.startDate}, ${input.dueDate}, ${input.expirationDate ?? null},
        ${input.plannedQuantity}, ${input.locationCode},
        ${input.status ?? PlanStatus.DRAFT}::plan_status,
        ${input.mpsId ?? null}, ${input.parentOrderId ?? null}, ${input.createdBy ?? null}
      )
      RETURNING
        id,
        order_number as "orderNumber",
        order_type as "orderType",
        item_code as "itemCode",
        start_date as "startDate",
        due_date as "dueDate",
        expiration_date as "expirationDate",
        planned_quantity as "plannedQuantity",
        location_code as "locationCode",
        status,
        mps_id as "mpsId",
        parent_order_id as "parentOrderId",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }

  async findByNumber(orderNumber: string): Promise<Order | null> {
    const result = await this.prisma.$queryRaw<Order[]>`
      SELECT
        id,
        order_number as "orderNumber",
        order_type as "orderType",
        item_code as "itemCode",
        start_date as "startDate",
        due_date as "dueDate",
        expiration_date as "expirationDate",
        planned_quantity as "plannedQuantity",
        location_code as "locationCode",
        status,
        mps_id as "mpsId",
        parent_order_id as "parentOrderId",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM orders
      WHERE order_number = ${orderNumber}
    `
    return result.length > 0 ? result[0] : null
  }

  async findByMpsId(mpsId: number): Promise<Order[]> {
    const result = await this.prisma.$queryRaw<Order[]>`
      SELECT
        id,
        order_number as "orderNumber",
        order_type as "orderType",
        item_code as "itemCode",
        start_date as "startDate",
        due_date as "dueDate",
        expiration_date as "expirationDate",
        planned_quantity as "plannedQuantity",
        location_code as "locationCode",
        status,
        mps_id as "mpsId",
        parent_order_id as "parentOrderId",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM orders
      WHERE mps_id = ${mpsId}
      ORDER BY due_date
    `
    return result
  }

  async findChildOrders(parentOrderId: number): Promise<Order[]> {
    const result = await this.prisma.$queryRaw<Order[]>`
      SELECT
        id,
        order_number as "orderNumber",
        order_type as "orderType",
        item_code as "itemCode",
        start_date as "startDate",
        due_date as "dueDate",
        expiration_date as "expirationDate",
        planned_quantity as "plannedQuantity",
        location_code as "locationCode",
        status,
        mps_id as "mpsId",
        parent_order_id as "parentOrderId",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM orders
      WHERE parent_order_id = ${parentOrderId}
      ORDER BY due_date
    `
    return result
  }

  async updateStatus(id: number, status: PlanStatusValue, updatedBy?: string): Promise<Order> {
    const result = await this.prisma.$queryRaw<Order[]>`
      UPDATE orders
      SET
        status = ${status}::plan_status,
        updated_by = ${updatedBy ?? null},
        updated_at = CURRENT_TIMESTAMP
      WHERE id = ${id}
      RETURNING
        id,
        order_number as "orderNumber",
        order_type as "orderType",
        item_code as "itemCode",
        start_date as "startDate",
        due_date as "dueDate",
        expiration_date as "expirationDate",
        planned_quantity as "plannedQuantity",
        location_code as "locationCode",
        status,
        mps_id as "mpsId",
        parent_order_id as "parentOrderId",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }
}
