import { PrismaClient } from '@prisma/client'

// 単価マスタの入力型
export interface CreateUnitPriceInput {
  itemCode: string
  supplierCode: string
  lotUnitQuantity?: number
  effectiveFrom: Date
  effectiveTo?: Date
  unitPrice: number
  createdBy?: string
}

// 単価マスタの出力型
export interface UnitPrice {
  id: number
  itemCode: string
  supplierCode: string
  lotUnitQuantity: number
  effectiveFrom: Date
  effectiveTo: Date | null
  unitPrice: number
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export class UnitPriceRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateUnitPriceInput): Promise<UnitPrice> {
    const result = await this.prisma.$queryRaw<UnitPrice[]>`
      INSERT INTO unit_prices (
        item_code, supplier_code, lot_unit_quantity, effective_from,
        effective_to, unit_price, created_by
      )
      VALUES (
        ${input.itemCode}, ${input.supplierCode}, ${input.lotUnitQuantity ?? 1},
        ${input.effectiveFrom}, ${input.effectiveTo ?? null},
        ${input.unitPrice}, ${input.createdBy ?? null}
      )
      RETURNING
        id,
        item_code as "itemCode",
        supplier_code as "supplierCode",
        lot_unit_quantity as "lotUnitQuantity",
        effective_from as "effectiveFrom",
        effective_to as "effectiveTo",
        unit_price as "unitPrice",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }

  async findCurrentPrice(
    itemCode: string,
    supplierCode: string,
    lotUnitQuantity: number,
    targetDate: Date
  ): Promise<UnitPrice | null> {
    const result = await this.prisma.$queryRaw<UnitPrice[]>`
      SELECT
        id,
        item_code as "itemCode",
        supplier_code as "supplierCode",
        lot_unit_quantity as "lotUnitQuantity",
        effective_from as "effectiveFrom",
        effective_to as "effectiveTo",
        unit_price as "unitPrice",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM unit_prices
      WHERE item_code = ${itemCode}
        AND supplier_code = ${supplierCode}
        AND lot_unit_quantity = ${lotUnitQuantity}
        AND effective_from <= ${targetDate}
        AND (effective_to IS NULL OR effective_to >= ${targetDate})
      ORDER BY effective_from DESC
      LIMIT 1
    `
    return result.length > 0 ? result[0] : null
  }

  async findByItemAndSupplier(
    itemCode: string,
    supplierCode: string
  ): Promise<UnitPrice[]> {
    const result = await this.prisma.$queryRaw<UnitPrice[]>`
      SELECT
        id,
        item_code as "itemCode",
        supplier_code as "supplierCode",
        lot_unit_quantity as "lotUnitQuantity",
        effective_from as "effectiveFrom",
        effective_to as "effectiveTo",
        unit_price as "unitPrice",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM unit_prices
      WHERE item_code = ${itemCode}
        AND supplier_code = ${supplierCode}
      ORDER BY lot_unit_quantity, effective_from DESC
    `
    return result
  }
}
