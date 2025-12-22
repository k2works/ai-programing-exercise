import { PrismaClient } from '@prisma/client';
import { BomRepository } from '../../../application/port/out/BomRepository';
import { Bom, BomExplosion } from '../../../domain/model/bom/Bom';

/**
 * Prismaを使用したBOMリポジトリ実装
 */
export class PrismaBomRepository implements BomRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async save(bom: Bom): Promise<Bom> {
    const result = await this.prisma.bom.upsert({
      where: {
        parentItemCode_childItemCode_effectiveFrom: {
          parentItemCode: bom.親品目コード,
          childItemCode: bom.子品目コード,
          effectiveFrom: bom.適用開始日,
        },
      },
      update: {
        effectiveTo: bom.適用停止日 || null,
        baseQuantity: bom.基準量,
        requiredQuantity: bom.必要量,
        defectRate: bom.不良率 || 0,
        sequence: bom.工順 || null,
      },
      create: {
        parentItemCode: bom.親品目コード,
        childItemCode: bom.子品目コード,
        effectiveFrom: bom.適用開始日,
        effectiveTo: bom.適用停止日 || null,
        baseQuantity: bom.基準量,
        requiredQuantity: bom.必要量,
        defectRate: bom.不良率 || 0,
        sequence: bom.工順 || null,
      },
    });

    return this.toBom(result);
  }

  async findChildren(parentItemCode: string): Promise<Bom[]> {
    const results = await this.prisma.bom.findMany({
      where: {
        parentItemCode: parentItemCode,
        OR: [
          { effectiveTo: null },
          { effectiveTo: { gte: new Date() } }
        ],
      },
      orderBy: [
        { sequence: 'asc' },
        { childItemCode: 'asc' },
      ],
    });

    return results.map(this.toBom);
  }

  async explode(itemCode: string, quantity: number): Promise<BomExplosion[]> {
    // PostgreSQLの再帰CTEを使用して全階層を展開
    const results = await this.prisma.$queryRaw<any[]>`
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
        parent_item_code,
        child_item_code,
        effective_from,
        effective_to,
        base_quantity,
        required_quantity,
        defect_rate,
        sequence,
        level,
        total_quantity * ${quantity} as total_quantity
      FROM bom_explosion
      ORDER BY level, parent_item_code, sequence, child_item_code
    `;

    return results.map((r) => this.toBomExplosion(r));
  }

  async findParents(childItemCode: string): Promise<Bom[]> {
    const results = await this.prisma.bom.findMany({
      where: {
        childItemCode: childItemCode,
        OR: [
          { effectiveTo: null },
          { effectiveTo: { gte: new Date() } }
        ],
      },
      orderBy: [
        { parentItemCode: 'asc' },
      ],
    });

    return results.map(this.toBom);
  }

  async delete(
    parentItemCode: string,
    childItemCode: string,
    effectiveFrom: Date
  ): Promise<void> {
    await this.prisma.bom.delete({
      where: {
        parentItemCode_childItemCode_effectiveFrom: {
          parentItemCode,
          childItemCode,
          effectiveFrom,
        },
      },
    });
  }

  private toBom(data: any): Bom {
    return Bom.create({
      親品目コード: data.parentItemCode,
      子品目コード: data.childItemCode,
      適用開始日: data.effectiveFrom,
      基準量: Number(data.baseQuantity),
      必要量: Number(data.requiredQuantity),
      適用停止日: data.effectiveTo || undefined,
      不良率: Number(data.defectRate),
      工順: data.sequence || undefined,
    });
  }

  private toBomExplosion(data: any): BomExplosion {
    return BomExplosion.create({
      親品目コード: data.parent_item_code,
      子品目コード: data.child_item_code,
      適用開始日: data.effective_from,
      基準量: Number(data.base_quantity),
      必要量: Number(data.required_quantity),
      レベル: data.level,
      合計必要量: Number(data.total_quantity),
      適用停止日: data.effective_to || undefined,
      不良率: Number(data.defect_rate),
      工順: data.sequence || undefined,
    });
  }
}
