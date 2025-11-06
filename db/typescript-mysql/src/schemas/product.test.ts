// src/schemas/product.test.ts
import { describe, it, expect } from 'vitest'
import { CreateProductSchema, UpdateProductSchema } from './product'

describe('Product Schemas', () => {
  describe('CreateProductSchema', () => {
    it('正常な商品作成リクエストを検証できる', () => {
      const validData = {
        prodCode: 'PROD00001',
        fullname: '黒毛和牛サーロインステーキ 200g',
        name: 'サーロイン',
        kana: 'クロゲワギュウサーロイン',
        unitprice: 5000,
        primeCost: 3500,
        supCode: 'SUP00001'
      }

      const result = CreateProductSchema.safeParse(validData)
      expect(result.success).toBe(true)
    })

    it('負の単価を拒否する', () => {
      const invalidData = {
        prodCode: 'PROD00001',
        fullname: '黒毛和牛サーロインステーキ 200g',
        name: 'サーロイン',
        kana: 'クロゲワギュウサーロイン',
        unitprice: -100, // 負の値
        primeCost: 3500,
        supCode: 'SUP00001'
      }

      const result = CreateProductSchema.safeParse(invalidData)
      expect(result.success).toBe(false)
    })

    it('必須フィールド欠落を拒否する', () => {
      const invalidData = {
        prodCode: 'PROD00001',
        // fullname が欠落
        name: 'サーロイン',
        kana: 'クロゲワギュウサーロイン',
        unitprice: 5000,
        primeCost: 3500,
        supCode: 'SUP00001'
      }

      const result = CreateProductSchema.safeParse(invalidData)
      expect(result.success).toBe(false)
    })
  })

  describe('UpdateProductSchema', () => {
    it('正常な商品更新リクエストを検証できる', () => {
      const validData = {
        fullname: '黒毛和牛サーロインステーキ 250g',
        unitprice: 5500
      }

      const result = UpdateProductSchema.safeParse(validData)
      expect(result.success).toBe(true)
    })

    it('空のオブジェクトを許可する', () => {
      const result = UpdateProductSchema.safeParse({})
      expect(result.success).toBe(true)
    })
  })
})
