import { describe, test, expect } from 'vitest'
import { z } from 'zod'

describe('Zod バリデーション例', () => {
  test('基本的なスキーマ定義とバリデーション', () => {
    // スキーマ定義
    const AccountSchema = z.object({
      id: z.number().int().positive(),
      code: z.string().min(1).max(20),
      name: z.string().min(1).max(100),
      accountType: z.enum(['ASSET', 'LIABILITY', 'EQUITY', 'REVENUE', 'EXPENSE']),
      balance: z.number().min(0)
    })

    // 正常なデータ
    const validData = {
      id: 1,
      code: '1000',
      name: '現金',
      accountType: 'ASSET' as const,
      balance: 50000
    }

    const result = AccountSchema.safeParse(validData)
    expect(result.success).toBe(true)
    if (result.success) {
      expect(result.data).toEqual(validData)
    }
  })

  test('バリデーションエラーの検出', () => {
    const AccountSchema = z.object({
      id: z.number().int().positive(),
      code: z.string().min(1).max(20),
      name: z.string().min(1),
      accountType: z.enum(['ASSET', 'LIABILITY', 'EQUITY', 'REVENUE', 'EXPENSE'])
    })

    // 不正なデータ（accountTypeが不正）
    const invalidData = {
      id: 1,
      code: '1000',
      name: '現金',
      accountType: 'INVALID_TYPE'
    }

    const result = AccountSchema.safeParse(invalidData)
    expect(result.success).toBe(false)
  })
})
