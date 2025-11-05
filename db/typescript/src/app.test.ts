import { describe, test, expect, beforeAll, afterAll } from 'vitest'
import { PrismaClient } from '@prisma/client'

const prisma = new PrismaClient()

describe('Prisma接続テスト', () => {
  beforeAll(async () => {
    // データベース接続の確認
  })

  afterAll(async () => {
    await prisma.$disconnect()
  })

  test('データベースに接続できる', async () => {
    // 接続確認用の簡単なクエリ
    const result = await prisma.$queryRaw`SELECT 1 as result`
    expect(result).toBeDefined()
  })
})
