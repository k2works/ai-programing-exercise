/* eslint-disable no-undef */
import { describe, test, expect, beforeAll, afterAll } from 'vitest'
import { PrismaClient } from '@prisma/client'

/**
 * テスト実行時は TEST_DATABASE_URL を使用する
 */
const isTest = process.env.VITEST === 'true' || process.env.NODE_ENV === 'test'
const databaseUrl = isTest ? process.env.TEST_DATABASE_URL : process.env.DATABASE_URL

const prisma = new PrismaClient({
  datasources: {
    db: {
      url: databaseUrl
    }
  }
})

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
