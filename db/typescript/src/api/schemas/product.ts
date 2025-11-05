// src/api/schemas/product.ts
import { z } from 'zod'

/**
 * 商品作成リクエストスキーマ
 */
export const CreateProductSchema = z.object({
  prodCode: z.string().min(1).max(16),
  fullname: z.string().min(1).max(40),
  name: z.string().min(1).max(10),
  kana: z.string().min(1).max(20),
  unitprice: z.number().int().nonnegative(),
  primeCost: z.number().int().nonnegative(),
  supCode: z.string().min(1).max(8)
})

/**
 * 商品更新リクエストスキーマ（すべてオプション）
 */
export const UpdateProductSchema = z.object({
  fullname: z.string().min(1).max(40).optional(),
  name: z.string().min(1).max(10).optional(),
  kana: z.string().min(1).max(20).optional(),
  unitprice: z.number().int().nonnegative().optional(),
  primeCost: z.number().int().nonnegative().optional(),
  supCode: z.string().min(1).max(8).optional()
})

/**
 * TypeScript 型の導出
 */
export type CreateProductRequest = z.infer<typeof CreateProductSchema>
export type UpdateProductRequest = z.infer<typeof UpdateProductSchema>
