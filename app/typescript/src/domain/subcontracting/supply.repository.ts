import { PrismaClient } from '@prisma/client'

export const SupplyCategory = {
  PAID: 'PAID', // 有償支給
  FREE: 'FREE', // 無償支給
} as const

export type SupplyCategoryValue = (typeof SupplyCategory)[keyof typeof SupplyCategory]

export interface CreateSupplyInput {
  supplyNumber: string
  orderNumber: string
  lineNumber: number
  supplierCode: string
  supplyDate: Date
  supplierPersonCode?: string
  supplyCategory?: SupplyCategoryValue
  details: {
    itemCode: string
    supplyQuantity: number
    supplyUnitPrice: number
    supplyAmount: number
    note?: string
  }[]
  note?: string
  createdBy?: string
}

export interface SupplyData {
  id: number
  supplyNumber: string
  orderNumber: string
  lineNumber: number
  supplierCode: string
  supplyDate: Date
  supplierPersonCode: string | null
  supplyCategory: string
  note: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export interface SupplyDetailData {
  id: number
  supplyNumber: string
  lineNumber: number
  itemCode: string
  supplyQuantity: number
  supplyUnitPrice: number
  supplyAmount: number
  note: string | null
}

export class SupplyRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateSupplyInput): Promise<SupplyData & { details: SupplyDetailData[] }> {
    // トランザクション内で支給データと明細を作成
    return await this.prisma.$transaction(async (tx) => {
      // 支給データを作成
      const supplyDataResult = await tx.$queryRaw<SupplyData[]>`
        INSERT INTO supply_data (
          supply_number, order_number, line_number, supplier_code,
          supply_date, supplier_person_code, supply_category, note, created_by
        )
        VALUES (
          ${input.supplyNumber}, ${input.orderNumber}, ${input.lineNumber},
          ${input.supplierCode}, ${input.supplyDate}, ${input.supplierPersonCode ?? null},
          ${input.supplyCategory ?? SupplyCategory.FREE}::supply_category,
          ${input.note ?? null}, ${input.createdBy ?? null}
        )
        RETURNING
          id,
          supply_number as "supplyNumber",
          order_number as "orderNumber",
          line_number as "lineNumber",
          supplier_code as "supplierCode",
          supply_date as "supplyDate",
          supplier_person_code as "supplierPersonCode",
          supply_category as "supplyCategory",
          note,
          created_at as "createdAt",
          created_by as "createdBy",
          updated_at as "updatedAt",
          updated_by as "updatedBy"
      `
      const supplyData = supplyDataResult[0]

      // 明細データを作成
      const details: SupplyDetailData[] = []
      for (let i = 0; i < input.details.length; i++) {
        const detail = input.details[i]
        const detailResult = await tx.$queryRaw<SupplyDetailData[]>`
          INSERT INTO supply_detail_data (
            supply_number, line_number, item_code,
            supply_quantity, supply_unit_price, supply_amount, note
          )
          VALUES (
            ${input.supplyNumber}, ${i + 1}, ${detail.itemCode},
            ${detail.supplyQuantity}, ${detail.supplyUnitPrice},
            ${detail.supplyAmount}, ${detail.note ?? null}
          )
          RETURNING
            id,
            supply_number as "supplyNumber",
            line_number as "lineNumber",
            item_code as "itemCode",
            supply_quantity as "supplyQuantity",
            supply_unit_price as "supplyUnitPrice",
            supply_amount as "supplyAmount",
            note
        `
        details.push(detailResult[0])
      }

      return {
        ...supplyData,
        details,
      }
    })
  }

  async findByNumber(supplyNumber: string): Promise<(SupplyData & { details: SupplyDetailData[] }) | null> {
    // 支給ヘッダーを取得
    const supplyDataResult = await this.prisma.$queryRaw<SupplyData[]>`
      SELECT
        id,
        supply_number as "supplyNumber",
        order_number as "orderNumber",
        line_number as "lineNumber",
        supplier_code as "supplierCode",
        supply_date as "supplyDate",
        supplier_person_code as "supplierPersonCode",
        supply_category as "supplyCategory",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM supply_data
      WHERE supply_number = ${supplyNumber}
    `

    if (supplyDataResult.length === 0) {
      return null
    }

    const supplyData = supplyDataResult[0]

    // 明細を取得
    const details = await this.prisma.$queryRaw<SupplyDetailData[]>`
      SELECT
        id,
        supply_number as "supplyNumber",
        line_number as "lineNumber",
        item_code as "itemCode",
        supply_quantity as "supplyQuantity",
        supply_unit_price as "supplyUnitPrice",
        supply_amount as "supplyAmount",
        note
      FROM supply_detail_data
      WHERE supply_number = ${supplyNumber}
      ORDER BY line_number
    `

    return {
      ...supplyData,
      details,
    }
  }

  async findByOrderDetail(orderNumber: string, lineNumber: number): Promise<SupplyData[]> {
    const result = await this.prisma.$queryRaw<SupplyData[]>`
      SELECT
        id,
        supply_number as "supplyNumber",
        order_number as "orderNumber",
        line_number as "lineNumber",
        supplier_code as "supplierCode",
        supply_date as "supplyDate",
        supplier_person_code as "supplierPersonCode",
        supply_category as "supplyCategory",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM supply_data
      WHERE order_number = ${orderNumber}
        AND line_number = ${lineNumber}
      ORDER BY supply_date
    `
    return result
  }
}
