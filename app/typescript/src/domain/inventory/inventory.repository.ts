import { PrismaClient, Prisma } from '@prisma/client'

export type StockStatus = 'accepted' | 'defect' | 'uninspected'

export interface CreateInventoryInput {
  locationCode: string
  itemCode: string
  stockQuantity: number
  acceptedQuantity?: number
  defectQuantity?: number
  uninspectedQuantity?: number
}

export interface IncreaseStockInput {
  locationCode: string
  itemCode: string
  quantity: number
  status: StockStatus
}

export interface DecreaseStockInput {
  locationCode: string
  itemCode: string
  quantity: number
  status: StockStatus
}

export interface ChangeStatusInput {
  locationCode: string
  itemCode: string
  quantity: number
  fromStatus: StockStatus
  toStatus: StockStatus
}

export interface InventoryData {
  id: number
  locationCode: string
  itemCode: string
  stockQuantity: Prisma.Decimal
  acceptedQuantity: Prisma.Decimal
  defectQuantity: Prisma.Decimal
  uninspectedQuantity: Prisma.Decimal
  createdAt: Date
  updatedAt: Date
}

export class InventoryRepository {
  constructor(private prisma: PrismaClient) {}

  async findByLocation(locationCode: string, itemCode: string): Promise<InventoryData | null> {
    const result = await this.prisma.$queryRaw<InventoryData[]>`
      SELECT
        id,
        location_code as "locationCode",
        item_code as "itemCode",
        stock_quantity as "stockQuantity",
        accepted_quantity as "acceptedQuantity",
        defect_quantity as "defectQuantity",
        uninspected_quantity as "uninspectedQuantity",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM inventories
      WHERE location_code = ${locationCode}
        AND item_code = ${itemCode}
    `

    if (result.length === 0) {
      // 在庫が存在しない場合は0を返す
      return {
        id: 0,
        locationCode,
        itemCode,
        stockQuantity: new Prisma.Decimal(0),
        acceptedQuantity: new Prisma.Decimal(0),
        defectQuantity: new Prisma.Decimal(0),
        uninspectedQuantity: new Prisma.Decimal(0),
        createdAt: new Date(),
        updatedAt: new Date(),
      }
    }

    return result[0]
  }

  async increase(input: IncreaseStockInput): Promise<void> {
    const statusField = this.getStatusField(input.status)

    await this.prisma.$executeRaw`
      INSERT INTO inventories (
        location_code, item_code, stock_quantity,
        accepted_quantity, defect_quantity, uninspected_quantity
      )
      VALUES (
        ${input.locationCode}, ${input.itemCode}, ${input.quantity},
        ${input.status === 'accepted' ? input.quantity : 0},
        ${input.status === 'defect' ? input.quantity : 0},
        ${input.status === 'uninspected' ? input.quantity : 0}
      )
      ON CONFLICT (location_code, item_code)
      DO UPDATE SET
        stock_quantity = inventories.stock_quantity + ${input.quantity},
        ${Prisma.raw(statusField)} = inventories.${Prisma.raw(statusField)} + ${input.quantity},
        updated_at = CURRENT_TIMESTAMP
    `
  }

  async decrease(input: DecreaseStockInput): Promise<void> {
    const current = await this.findByLocation(input.locationCode, input.itemCode)
    const statusField = this.getStatusField(input.status)

    if (!current || current.id === 0) {
      throw new Error('在庫が不足しています')
    }

    const currentQuantity = Number(current[this.getStatusFieldCamelCase(input.status)])

    if (currentQuantity < input.quantity) {
      throw new Error('在庫が不足しています')
    }

    await this.prisma.$executeRaw`
      UPDATE inventories
      SET
        stock_quantity = stock_quantity - ${input.quantity},
        ${Prisma.raw(statusField)} = ${Prisma.raw(statusField)} - ${input.quantity},
        updated_at = CURRENT_TIMESTAMP
      WHERE location_code = ${input.locationCode}
        AND item_code = ${input.itemCode}
    `
  }

  async changeStatus(input: ChangeStatusInput): Promise<void> {
    const current = await this.findByLocation(input.locationCode, input.itemCode)

    if (!current || current.id === 0) {
      throw new Error(`${input.fromStatus}の在庫が不足しています`)
    }

    const fromField = this.getStatusField(input.fromStatus)
    const toField = this.getStatusField(input.toStatus)
    const currentQuantity = Number(current[this.getStatusFieldCamelCase(input.fromStatus)])

    if (currentQuantity < input.quantity) {
      throw new Error(`${input.fromStatus}の在庫が不足しています`)
    }

    await this.prisma.$executeRaw`
      UPDATE inventories
      SET
        ${Prisma.raw(fromField)} = ${Prisma.raw(fromField)} - ${input.quantity},
        ${Prisma.raw(toField)} = ${Prisma.raw(toField)} + ${input.quantity},
        updated_at = CURRENT_TIMESTAMP
      WHERE location_code = ${input.locationCode}
        AND item_code = ${input.itemCode}
    `
  }

  private getStatusField(status: StockStatus): string {
    switch (status) {
      case 'accepted':
        return 'accepted_quantity'
      case 'defect':
        return 'defect_quantity'
      case 'uninspected':
        return 'uninspected_quantity'
    }
  }

  private getStatusFieldCamelCase(status: StockStatus): keyof InventoryData {
    switch (status) {
      case 'accepted':
        return 'acceptedQuantity'
      case 'defect':
        return 'defectQuantity'
      case 'uninspected':
        return 'uninspectedQuantity'
    }
  }
}
