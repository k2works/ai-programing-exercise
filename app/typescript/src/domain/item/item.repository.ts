import { PrismaClient } from '@prisma/client'

// 品目区分の型定義
export const ItemCategory = {
  PRODUCT: 'PRODUCT',
  SEMI_PRODUCT: 'SEMI_PRODUCT',
  INTERMEDIATE: 'INTERMEDIATE',
  PART: 'PART',
  MATERIAL: 'MATERIAL',
  RAW_MATERIAL: 'RAW_MATERIAL',
  SUPPLY: 'SUPPLY',
} as const

export type ItemCategory = (typeof ItemCategory)[keyof typeof ItemCategory]

// 品目の入力型
export interface CreateItemInput {
  itemCode: string
  itemName: string
  itemCategory: ItemCategory
  effectiveFrom: Date
  effectiveTo?: Date
  unitCode?: string
  leadTime?: number
  safetyLeadTime?: number
  safetyStock?: number
  yieldRate?: number
  minLotSize?: number
  lotIncrement?: number
  maxLotSize?: number
  shelfLife?: number
}

// 品目の出力型
export interface Item {
  id: number
  itemCode: string
  itemName: string
  itemCategory: ItemCategory
  effectiveFrom: Date
  effectiveTo: Date | null
  unitCode: string | null
  leadTime: number
  safetyLeadTime: number
  safetyStock: number
  yieldRate: number
  minLotSize: number
  lotIncrement: number
  maxLotSize: number | null
  shelfLife: number | null
  createdAt: Date
  updatedAt: Date
}

export class ItemRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateItemInput): Promise<Item> {
    const result = await this.prisma.$queryRaw<Item[]>`
      INSERT INTO items (
        item_code, item_name, item_category, effective_from, effective_to, unit_code,
        lead_time, safety_lead_time, safety_stock, yield_rate,
        min_lot_size, lot_increment, max_lot_size, shelf_life
      )
      VALUES (
        ${input.itemCode}, ${input.itemName}, ${input.itemCategory}::item_category,
        ${input.effectiveFrom}, ${input.effectiveTo ?? null}, ${input.unitCode ?? null},
        ${input.leadTime ?? 0}, ${input.safetyLeadTime ?? 0}, ${input.safetyStock ?? 0},
        ${input.yieldRate ?? 100}, ${input.minLotSize ?? 1}, ${input.lotIncrement ?? 1},
        ${input.maxLotSize ?? null}, ${input.shelfLife ?? null}
      )
      RETURNING
        id, item_code as "itemCode", item_name as "itemName",
        item_category as "itemCategory", effective_from as "effectiveFrom",
        effective_to as "effectiveTo", unit_code as "unitCode",
        lead_time as "leadTime", safety_lead_time as "safetyLeadTime",
        safety_stock as "safetyStock", yield_rate as "yieldRate",
        min_lot_size as "minLotSize", lot_increment as "lotIncrement",
        max_lot_size as "maxLotSize", shelf_life as "shelfLife",
        created_at as "createdAt", updated_at as "updatedAt"
    `
    return result[0]
  }
}
