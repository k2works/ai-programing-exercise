import { describe, test, expect } from 'vitest'
import { z } from 'zod'

describe('Zod バリデーション例', () => {
  test('基本的なスキーマ定義とバリデーション', () => {
    // スキーマ定義
    const UserSchema = z.object({
      id: z.number().int().positive(),
      name: z.string().min(1).max(100),
      email: z.string().email(),
      age: z.number().int().min(0).max(150).optional()
    })

    // 正常なデータ
    const validData = {
      id: 1,
      name: 'John Doe',
      email: 'john@example.com',
      age: 30
    }

    const result = UserSchema.safeParse(validData)
    expect(result.success).toBe(true)
    if (result.success) {
      expect(result.data).toEqual(validData)
    }
  })

  test('バリデーションエラーの検出', () => {
    const UserSchema = z.object({
      id: z.number().int().positive(),
      name: z.string().min(1),
      email: z.string().email()
    })

    // 不正なデータ（emailが不正）
    const invalidData = {
      id: 1,
      name: 'John Doe',
      email: 'invalid-email'
    }

    const result = UserSchema.safeParse(invalidData)
    expect(result.success).toBe(false)
    if (!result.success) {
      expect(result.error.issues).toHaveLength(1)
      expect(result.error.issues[0].path).toEqual(['email'])
    }
  })

  test('Partial スキーマの使用', () => {
    const UserSchema = z.object({
      id: z.number(),
      name: z.string(),
      email: z.string().email()
    })

    // 部分的なデータのみを許可
    const PartialUserSchema = UserSchema.partial()

    const partialData = {
      name: 'John Doe'
    }

    const result = PartialUserSchema.safeParse(partialData)
    expect(result.success).toBe(true)
  })

  test('デフォルト値の設定', () => {
    const UserSchema = z.object({
      id: z.number(),
      name: z.string(),
      role: z.string().default('user'),
      isActive: z.boolean().default(true)
    })

    const dataWithoutDefaults = {
      id: 1,
      name: 'John Doe'
    }

    const result = UserSchema.parse(dataWithoutDefaults)
    expect(result.role).toBe('user')
    expect(result.isActive).toBe(true)
  })

  test('配列のバリデーション', () => {
    const TagSchema = z.object({
      id: z.number(),
      name: z.string()
    })

    const UserSchema = z.object({
      id: z.number(),
      name: z.string(),
      tags: z.array(TagSchema)
    })

    const userData = {
      id: 1,
      name: 'John Doe',
      tags: [
        { id: 1, name: 'admin' },
        { id: 2, name: 'developer' }
      ]
    }

    const result = UserSchema.safeParse(userData)
    expect(result.success).toBe(true)
  })

  test('Union 型のバリデーション', () => {
    const StatusSchema = z.union([z.literal('active'), z.literal('inactive'), z.literal('pending')])

    expect(StatusSchema.safeParse('active').success).toBe(true)
    expect(StatusSchema.safeParse('inactive').success).toBe(true)
    expect(StatusSchema.safeParse('invalid').success).toBe(false)
  })

  test('カスタムバリデーション', () => {
    const PasswordSchema = z
      .string()
      .min(8, '8文字以上である必要があります')
      .regex(/[A-Z]/, '大文字を含む必要があります')
      .regex(/[a-z]/, '小文字を含む必要があります')
      .regex(/[0-9]/, '数字を含む必要があります')

    expect(PasswordSchema.safeParse('Password123').success).toBe(true)
    expect(PasswordSchema.safeParse('pass').success).toBe(false)
    expect(PasswordSchema.safeParse('password').success).toBe(false)
    expect(PasswordSchema.safeParse('PASSWORD123').success).toBe(false)
  })

  test('変換（Transform）の使用', () => {
    const DateSchema = z.string().transform((str) => new Date(str))

    const result = DateSchema.parse('2024-01-01')
    expect(result).toBeInstanceOf(Date)
    expect(result.getFullYear()).toBe(2024)
  })
})
