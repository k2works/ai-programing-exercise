import { PrismaClient, Prisma } from '@prisma/client'
import { InventoryRepository } from './inventory.repository'

export type StocktakingStatus = 'ISSUED' | 'INPUTTED' | 'CONFIRMED'

export interface CreateStocktakingInput {
  stocktakingNumber: string
  stocktakingDate: Date
  locationCode: string
  createdBy?: string
}

export interface UpdateActualQuantityInput {
  stocktakingNumber: string
  detailId: number
  actualQuantity: number
}

export interface StocktakingDetailData {
  id: number
  stocktakingNumber: string
  itemCode: string
  bookQuantity: Prisma.Decimal
  actualQuantity: Prisma.Decimal | null
  differenceQuantity: Prisma.Decimal
  createdAt: Date
  updatedAt: Date
}

export interface StocktakingData {
  id: number
  stocktakingNumber: string
  stocktakingDate: Date
  locationCode: string
  status: StocktakingStatus
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
  details: StocktakingDetailData[]
}

export interface StockAdjustmentData {
  id: number
  adjustmentNumber: string
  stocktakingNumber: string
  adjustmentDate: Date
  itemCode: string
  locationCode: string
  adjustmentQuantity: Prisma.Decimal
  reasonCode: string
  note: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export class StocktakingRepository {
  private inventoryRepository: InventoryRepository

  constructor(private prisma: PrismaClient) {
    this.inventoryRepository = new InventoryRepository(prisma)
  }

  async issueStocktaking(input: CreateStocktakingInput): Promise<StocktakingData> {
    return await this.prisma.$transaction(async (tx) => {
      // 棚卸ヘッダーを作成
      const headerData = await tx.$queryRaw<StocktakingData[]>`
        INSERT INTO stocktaking_data (
          stocktaking_number, stocktaking_date, location_code, status, created_by
        )
        VALUES (
          ${input.stocktakingNumber}, ${input.stocktakingDate}, ${input.locationCode},
          'ISSUED', ${input.createdBy ?? null}
        )
        RETURNING
          id,
          stocktaking_number as "stocktakingNumber",
          stocktaking_date as "stocktakingDate",
          location_code as "locationCode",
          status,
          created_at as "createdAt",
          created_by as "createdBy",
          updated_at as "updatedAt",
          updated_by as "updatedBy"
      `
      const header = headerData[0]

      // 在庫情報から棚卸明細を作成
      const inventories = await tx.$queryRaw<Array<{ itemCode: string; stockQuantity: Prisma.Decimal }>>`
        SELECT
          item_code as "itemCode",
          stock_quantity as "stockQuantity"
        FROM inventories
        WHERE location_code = ${input.locationCode}
          AND stock_quantity > 0
        ORDER BY item_code
      `

      const details: StocktakingDetailData[] = []
      for (const inventory of inventories) {
        const detailData = await tx.$queryRaw<StocktakingDetailData[]>`
          INSERT INTO stocktaking_detail_data (
            stocktaking_number, item_code, book_quantity, difference_quantity
          )
          VALUES (
            ${input.stocktakingNumber}, ${inventory.itemCode}, ${inventory.stockQuantity}, 0
          )
          RETURNING
            id,
            stocktaking_number as "stocktakingNumber",
            item_code as "itemCode",
            book_quantity as "bookQuantity",
            actual_quantity as "actualQuantity",
            difference_quantity as "differenceQuantity",
            created_at as "createdAt",
            updated_at as "updatedAt"
        `
        details.push(detailData[0])
      }

      return {
        ...header,
        details,
      }
    })
  }

  async inputActualQuantity(input: UpdateActualQuantityInput): Promise<void> {
    await this.prisma.$executeRaw`
      UPDATE stocktaking_detail_data
      SET
        actual_quantity = ${input.actualQuantity},
        difference_quantity = ${input.actualQuantity} - book_quantity,
        updated_at = CURRENT_TIMESTAMP
      WHERE id = ${input.detailId}
        AND stocktaking_number = ${input.stocktakingNumber}
    `

    // すべての明細に実棚数が入力されたか確認
    const incompleteCount = await this.prisma.$queryRaw<Array<{ count: number }>>`
      SELECT COUNT(*) as count
      FROM stocktaking_detail_data
      WHERE stocktaking_number = ${input.stocktakingNumber}
        AND actual_quantity IS NULL
    `

    // すべて入力済みならステータスを更新
    if (Number(incompleteCount[0].count) === 0) {
      await this.prisma.$executeRaw`
        UPDATE stocktaking_data
        SET
          status = 'INPUTTED',
          updated_at = CURRENT_TIMESTAMP
        WHERE stocktaking_number = ${input.stocktakingNumber}
      `
    }
  }

  async confirmStocktaking(stocktakingNumber: string, createdBy?: string): Promise<StockAdjustmentData[]> {
    return await this.prisma.$transaction(async (tx) => {
      // 棚卸データを取得
      const stocktaking = await this.findByNumber(stocktakingNumber)
      if (!stocktaking) {
        throw new Error('棚卸データが見つかりません')
      }

      if (stocktaking.status !== 'INPUTTED') {
        throw new Error('棚卸データのステータスが入力済みではありません')
      }

      const adjustments: StockAdjustmentData[] = []

      // 差異がある明細について在庫調整データを作成
      for (const detail of stocktaking.details) {
        const difference = Number(detail.differenceQuantity)
        if (difference !== 0) {
          const adjustmentNumber = `ADJ-${stocktakingNumber}-${detail.itemCode}`
          const reasonCode = difference > 0 ? 'SURPLUS' : 'SHORTAGE'

          const adjustmentData = await tx.$queryRaw<StockAdjustmentData[]>`
            INSERT INTO stock_adjustment_data (
              adjustment_number, stocktaking_number, adjustment_date,
              item_code, location_code, adjustment_quantity, reason_code, created_by
            )
            VALUES (
              ${adjustmentNumber}, ${stocktakingNumber}, ${stocktaking.stocktakingDate},
              ${detail.itemCode}, ${stocktaking.locationCode}, ${difference},
              ${reasonCode}, ${createdBy ?? null}
            )
            RETURNING
              id,
              adjustment_number as "adjustmentNumber",
              stocktaking_number as "stocktakingNumber",
              adjustment_date as "adjustmentDate",
              item_code as "itemCode",
              location_code as "locationCode",
              adjustment_quantity as "adjustmentQuantity",
              reason_code as "reasonCode",
              note,
              created_at as "createdAt",
              created_by as "createdBy",
              updated_at as "updatedAt",
              updated_by as "updatedBy"
          `
          adjustments.push(adjustmentData[0])

          // 在庫を調整
          const inventoryRepo = new InventoryRepository(tx as unknown as PrismaClient)
          if (difference > 0) {
            // 実棚が多い場合は増加
            await inventoryRepo.increase({
              locationCode: stocktaking.locationCode,
              itemCode: detail.itemCode,
              quantity: Math.abs(difference),
              status: 'accepted',
            })
          } else {
            // 実棚が少ない場合は減少
            await inventoryRepo.decrease({
              locationCode: stocktaking.locationCode,
              itemCode: detail.itemCode,
              quantity: Math.abs(difference),
              status: 'accepted',
            })
          }
        }
      }

      // 棚卸ステータスを確定に更新
      await tx.$executeRaw`
        UPDATE stocktaking_data
        SET
          status = 'CONFIRMED',
          updated_at = CURRENT_TIMESTAMP,
          updated_by = ${createdBy ?? null}
        WHERE stocktaking_number = ${stocktakingNumber}
      `

      return adjustments
    })
  }

  async findByNumber(stocktakingNumber: string): Promise<StocktakingData | null> {
    const headers = await this.prisma.$queryRaw<StocktakingData[]>`
      SELECT
        id,
        stocktaking_number as "stocktakingNumber",
        stocktaking_date as "stocktakingDate",
        location_code as "locationCode",
        status,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM stocktaking_data
      WHERE stocktaking_number = ${stocktakingNumber}
    `

    if (headers.length === 0) {
      return null
    }

    const details = await this.prisma.$queryRaw<StocktakingDetailData[]>`
      SELECT
        id,
        stocktaking_number as "stocktakingNumber",
        item_code as "itemCode",
        book_quantity as "bookQuantity",
        actual_quantity as "actualQuantity",
        difference_quantity as "differenceQuantity",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM stocktaking_detail_data
      WHERE stocktaking_number = ${stocktakingNumber}
      ORDER BY item_code
    `

    return {
      ...headers[0],
      details,
    }
  }

  async findByLocation(locationCode: string): Promise<StocktakingData[]> {
    const headers = await this.prisma.$queryRaw<StocktakingData[]>`
      SELECT
        id,
        stocktaking_number as "stocktakingNumber",
        stocktaking_date as "stocktakingDate",
        location_code as "locationCode",
        status,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM stocktaking_data
      WHERE location_code = ${locationCode}
      ORDER BY stocktaking_date DESC
    `

    const results: StocktakingData[] = []

    for (const header of headers) {
      const details = await this.prisma.$queryRaw<StocktakingDetailData[]>`
        SELECT
          id,
          stocktaking_number as "stocktakingNumber",
          item_code as "itemCode",
          book_quantity as "bookQuantity",
          actual_quantity as "actualQuantity",
          difference_quantity as "differenceQuantity",
          created_at as "createdAt",
          updated_at as "updatedAt"
        FROM stocktaking_detail_data
        WHERE stocktaking_number = ${header.stocktakingNumber}
        ORDER BY item_code
      `

      results.push({
        ...header,
        details,
      })
    }

    return results
  }
}
