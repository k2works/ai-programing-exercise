import { PrismaClient } from '@prisma/client'

export interface CreateConsumptionInput {
  consumptionNumber: string
  receivingNumber: string
  consumptionDate: Date
  supplierCode: string
  details: {
    itemCode: string
    consumptionQuantity: number
    note?: string
  }[]
  note?: string
  createdBy?: string
}

export interface ConsumptionData {
  id: number
  consumptionNumber: string
  receivingNumber: string
  consumptionDate: Date
  supplierCode: string
  note: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export interface ConsumptionDetailData {
  id: number
  consumptionNumber: string
  lineNumber: number
  itemCode: string
  consumptionQuantity: number
  note: string | null
}

export class ConsumptionRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateConsumptionInput): Promise<ConsumptionData & { details: ConsumptionDetailData[] }> {
    // トランザクション内で消費データと明細を作成
    return await this.prisma.$transaction(async (tx) => {
      // 消費データを作成
      const consumptionDataResult = await tx.$queryRaw<ConsumptionData[]>`
        INSERT INTO consumption_data (
          consumption_number, receiving_number, consumption_date,
          supplier_code, note, created_by
        )
        VALUES (
          ${input.consumptionNumber}, ${input.receivingNumber}, ${input.consumptionDate},
          ${input.supplierCode}, ${input.note ?? null}, ${input.createdBy ?? null}
        )
        RETURNING
          id,
          consumption_number as "consumptionNumber",
          receiving_number as "receivingNumber",
          consumption_date as "consumptionDate",
          supplier_code as "supplierCode",
          note,
          created_at as "createdAt",
          created_by as "createdBy",
          updated_at as "updatedAt",
          updated_by as "updatedBy"
      `
      const consumptionData = consumptionDataResult[0]

      // 明細データを作成
      const details: ConsumptionDetailData[] = []
      for (let i = 0; i < input.details.length; i++) {
        const detail = input.details[i]
        const detailResult = await tx.$queryRaw<ConsumptionDetailData[]>`
          INSERT INTO consumption_detail_data (
            consumption_number, line_number, item_code,
            consumption_quantity, note
          )
          VALUES (
            ${input.consumptionNumber}, ${i + 1}, ${detail.itemCode},
            ${detail.consumptionQuantity}, ${detail.note ?? null}
          )
          RETURNING
            id,
            consumption_number as "consumptionNumber",
            line_number as "lineNumber",
            item_code as "itemCode",
            consumption_quantity as "consumptionQuantity",
            note
        `
        details.push(detailResult[0])
      }

      return {
        ...consumptionData,
        details,
      }
    })
  }

  async findByNumber(consumptionNumber: string): Promise<(ConsumptionData & { details: ConsumptionDetailData[] }) | null> {
    // 消費ヘッダーを取得
    const consumptionDataResult = await this.prisma.$queryRaw<ConsumptionData[]>`
      SELECT
        id,
        consumption_number as "consumptionNumber",
        receiving_number as "receivingNumber",
        consumption_date as "consumptionDate",
        supplier_code as "supplierCode",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM consumption_data
      WHERE consumption_number = ${consumptionNumber}
    `

    if (consumptionDataResult.length === 0) {
      return null
    }

    const consumptionData = consumptionDataResult[0]

    // 明細を取得
    const details = await this.prisma.$queryRaw<ConsumptionDetailData[]>`
      SELECT
        id,
        consumption_number as "consumptionNumber",
        line_number as "lineNumber",
        item_code as "itemCode",
        consumption_quantity as "consumptionQuantity",
        note
      FROM consumption_detail_data
      WHERE consumption_number = ${consumptionNumber}
      ORDER BY line_number
    `

    return {
      ...consumptionData,
      details,
    }
  }

  async findByReceiving(receivingNumber: string): Promise<ConsumptionData[]> {
    const result = await this.prisma.$queryRaw<ConsumptionData[]>`
      SELECT
        id,
        consumption_number as "consumptionNumber",
        receiving_number as "receivingNumber",
        consumption_date as "consumptionDate",
        supplier_code as "supplierCode",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM consumption_data
      WHERE receiving_number = ${receivingNumber}
      ORDER BY consumption_date
    `
    return result
  }

  async calculateConsumptionRate(supplyNumber: string, itemCode: string): Promise<number> {
    // 支給数量を取得
    const supplyResult = await this.prisma.$queryRaw<{ supplyQuantity: number }[]>`
      SELECT
        SUM(supply_quantity) as "supplyQuantity"
      FROM supply_detail_data
      WHERE supply_number = ${supplyNumber}
        AND item_code = ${itemCode}
    `

    if (!supplyResult || supplyResult.length === 0 || !supplyResult[0].supplyQuantity) {
      return 0
    }

    const supplyQuantity = Number(supplyResult[0].supplyQuantity)

    // 該当支給に紐づく消費数量を取得
    // 支給データ→発注明細→入荷データ→消費データの経路で集計
    const consumptionResult = await this.prisma.$queryRaw<{ consumptionQuantity: number }[]>`
      SELECT
        COALESCE(SUM(cd.consumption_quantity), 0) as "consumptionQuantity"
      FROM consumption_detail_data cd
      INNER JOIN consumption_data c ON cd.consumption_number = c.consumption_number
      INNER JOIN receiving_data r ON c.receiving_number = r.receiving_number
      INNER JOIN supply_data s ON r.order_number = s.order_number AND r.line_number = s.line_number
      WHERE s.supply_number = ${supplyNumber}
        AND cd.item_code = ${itemCode}
    `

    if (!consumptionResult || consumptionResult.length === 0) {
      return 0
    }

    const consumptionQuantity = Number(consumptionResult[0].consumptionQuantity)

    // 消費率を計算（消費数量 / 支給数量）
    return consumptionQuantity / supplyQuantity
  }
}
