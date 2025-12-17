import { describe, test, expect } from 'vitest'
import { z } from 'zod'

describe('Zod バリデーション例', () => {
  test('品目マスタのスキーマ定義とバリデーション', () => {
    // 品目区分のEnum
    const ItemCategorySchema = z.enum([
      'PRODUCT',      // 製品
      'SEMI_PRODUCT', // 半製品
      'INTERMEDIATE', // 中間品
      'PART',         // 部品
      'MATERIAL',     // 材料
      'RAW_MATERIAL', // 原料
      'SUPPLY'        // 資材
    ])

    // 品目マスタスキーマ定義
    const ItemSchema = z.object({
      id: z.number().int().positive(),
      itemCode: z.string().min(1).max(20),
      effectiveFrom: z.date(),
      effectiveTo: z.date().optional(),
      itemName: z.string().min(1).max(100),
      itemCategory: ItemCategorySchema,
      leadTime: z.number().int().min(0).default(0),
      safetyStock: z.number().min(0).default(0),
    })

    // 正常なデータ
    const validData = {
      id: 1,
      itemCode: 'ITEM001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '製品A',
      itemCategory: 'PRODUCT' as const,
      leadTime: 5,
      safetyStock: 100,
    }

    const result = ItemSchema.safeParse(validData)
    expect(result.success).toBe(true)
    if (result.success) {
      expect(result.data.itemCode).toBe('ITEM001')
    }
  })

  test('バリデーションエラーの検出', () => {
    const ItemCategorySchema = z.enum([
      'PRODUCT', 'SEMI_PRODUCT', 'INTERMEDIATE',
      'PART', 'MATERIAL', 'RAW_MATERIAL', 'SUPPLY'
    ])

    const ItemSchema = z.object({
      itemCode: z.string().min(1).max(20),
      itemName: z.string().min(1),
      itemCategory: ItemCategorySchema,
    })

    // 不正なデータ（itemCategoryが不正）
    const invalidData = {
      itemCode: 'ITEM001',
      itemName: '製品A',
      itemCategory: 'INVALID_CATEGORY',
    }

    const result = ItemSchema.safeParse(invalidData)
    expect(result.success).toBe(false)
  })
})
