import { PrismaClient } from '@prisma/client'

export const ReceivingCategory = {
  NORMAL: 'NORMAL', // 通常入荷
  SPLIT: 'SPLIT', // 分割入荷
  RETURN: 'RETURN', // 返品入荷
} as const

export type ReceivingCategoryValue = (typeof ReceivingCategory)[keyof typeof ReceivingCategory]

export interface CreateReceivingInput {
  receivingNumber: string
  orderNumber: string
  lineNumber: number
  receivingDate: Date
  receiverCode?: string
  receivingCategory?: ReceivingCategoryValue
  itemCode: string
  isMiscellaneous?: boolean
  receivedQuantity: number
  note?: string
  createdBy?: string
}

export interface ReceivingData {
  id: number
  receivingNumber: string
  orderNumber: string
  lineNumber: number
  receivingDate: Date
  receiverCode: string | null
  receivingCategory: string
  itemCode: string
  isMiscellaneous: boolean
  receivedQuantity: number
  note: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export class ReceivingRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateReceivingInput): Promise<ReceivingData> {
    const result = await this.prisma.$queryRaw<ReceivingData[]>`
      INSERT INTO receiving_data (
        receiving_number, order_number, line_number, receiving_date,
        receiver_code, receiving_category, item_code, is_miscellaneous,
        received_quantity, note, created_by
      )
      VALUES (
        ${input.receivingNumber}, ${input.orderNumber}, ${input.lineNumber},
        ${input.receivingDate}, ${input.receiverCode ?? null},
        ${input.receivingCategory ?? ReceivingCategory.NORMAL}::receiving_category,
        ${input.itemCode}, ${input.isMiscellaneous ?? false},
        ${input.receivedQuantity}, ${input.note ?? null}, ${input.createdBy ?? null}
      )
      RETURNING
        id,
        receiving_number as "receivingNumber",
        order_number as "orderNumber",
        line_number as "lineNumber",
        receiving_date as "receivingDate",
        receiver_code as "receiverCode",
        receiving_category as "receivingCategory",
        item_code as "itemCode",
        is_miscellaneous as "isMiscellaneous",
        received_quantity as "receivedQuantity",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }

  async findByNumber(receivingNumber: string): Promise<ReceivingData | null> {
    const result = await this.prisma.$queryRaw<ReceivingData[]>`
      SELECT
        id,
        receiving_number as "receivingNumber",
        order_number as "orderNumber",
        line_number as "lineNumber",
        receiving_date as "receivingDate",
        receiver_code as "receiverCode",
        receiving_category as "receivingCategory",
        item_code as "itemCode",
        is_miscellaneous as "isMiscellaneous",
        received_quantity as "receivedQuantity",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM receiving_data
      WHERE receiving_number = ${receivingNumber}
    `
    return result.length > 0 ? result[0] : null
  }

  async findByOrderDetail(orderNumber: string, lineNumber: number): Promise<ReceivingData[]> {
    const result = await this.prisma.$queryRaw<ReceivingData[]>`
      SELECT
        id,
        receiving_number as "receivingNumber",
        order_number as "orderNumber",
        line_number as "lineNumber",
        receiving_date as "receivingDate",
        receiver_code as "receiverCode",
        receiving_category as "receivingCategory",
        item_code as "itemCode",
        is_miscellaneous as "isMiscellaneous",
        received_quantity as "receivedQuantity",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM receiving_data
      WHERE order_number = ${orderNumber}
        AND line_number = ${lineNumber}
      ORDER BY receiving_date
    `
    return result
  }
}
