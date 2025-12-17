import { PrismaClient } from '@prisma/client'

export interface CreateBomInput {
  parentItemCode: string
  childItemCode: string
  effectiveFrom: Date
  effectiveTo?: Date
  baseQuantity: number
  requiredQuantity: number
  defectRate?: number
  sequence?: number
}

export interface Bom {
  parentItemCode: string
  childItemCode: string
  effectiveFrom: Date
  effectiveTo: Date | null
  baseQuantity: number
  requiredQuantity: number
  defectRate: number
  sequence: number | null
}

export interface BomExplosion extends Bom {
  level: number
  totalQuantity: number
}

export class BomRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateBomInput): Promise<Bom> {
    const result = await this.prisma.$queryRaw<Bom[]>`
      INSERT INTO bom (
        parent_item_code, child_item_code, effective_from, effective_to,
        base_quantity, required_quantity, defect_rate, sequence
      )
      VALUES (
        ${input.parentItemCode}, ${input.childItemCode}, ${input.effectiveFrom},
        ${input.effectiveTo ?? null}, ${input.baseQuantity}, ${input.requiredQuantity},
        ${input.defectRate ?? 0}, ${input.sequence ?? null}
      )
      RETURNING
        parent_item_code as "parentItemCode",
        child_item_code as "childItemCode",
        effective_from as "effectiveFrom",
        effective_to as "effectiveTo",
        base_quantity as "baseQuantity",
        required_quantity as "requiredQuantity",
        defect_rate as "defectRate",
        sequence
    `
    return result[0]
  }

  async getChildren(parentItemCode: string): Promise<Bom[]> {
    return this.prisma.$queryRaw<Bom[]>`
      SELECT
        parent_item_code as "parentItemCode",
        child_item_code as "childItemCode",
        effective_from as "effectiveFrom",
        effective_to as "effectiveTo",
        base_quantity as "baseQuantity",
        required_quantity as "requiredQuantity",
        defect_rate as "defectRate",
        sequence
      FROM bom
      WHERE parent_item_code = ${parentItemCode}
        AND (effective_to IS NULL OR effective_to >= CURRENT_DATE)
      ORDER BY sequence, child_item_code
    `
  }

  async explode(itemCode: string, quantity: number = 1): Promise<BomExplosion[]> {
    // PostgreSQLの再帰CTEを使用して全階層を展開
    const result = await this.prisma.$queryRaw<BomExplosion[]>`
      WITH RECURSIVE bom_explosion AS (
        -- 基底: 直接の子品目
        SELECT
          parent_item_code,
          child_item_code,
          effective_from,
          effective_to,
          base_quantity,
          required_quantity,
          defect_rate,
          sequence,
          1 as level,
          CAST(required_quantity AS NUMERIC) as total_quantity
        FROM bom
        WHERE parent_item_code = ${itemCode}
          AND (effective_to IS NULL OR effective_to >= CURRENT_DATE)

        UNION ALL

        -- 再帰: 子品目の子品目
        SELECT
          b.parent_item_code,
          b.child_item_code,
          b.effective_from,
          b.effective_to,
          b.base_quantity,
          b.required_quantity,
          b.defect_rate,
          b.sequence,
          be.level + 1,
          CAST(be.total_quantity * b.required_quantity AS NUMERIC)
        FROM bom b
        INNER JOIN bom_explosion be ON b.parent_item_code = be.child_item_code
        WHERE (b.effective_to IS NULL OR b.effective_to >= CURRENT_DATE)
          AND be.level < 10  -- 無限ループ防止
      )
      SELECT
        parent_item_code as "parentItemCode",
        child_item_code as "childItemCode",
        effective_from as "effectiveFrom",
        effective_to as "effectiveTo",
        base_quantity as "baseQuantity",
        required_quantity as "requiredQuantity",
        defect_rate as "defectRate",
        sequence,
        level,
        total_quantity * ${quantity} as "totalQuantity"
      FROM bom_explosion
      ORDER BY level, parent_item_code, sequence, child_item_code
    `
    return result
  }
}
