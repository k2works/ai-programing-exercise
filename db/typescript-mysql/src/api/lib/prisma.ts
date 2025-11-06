// src/lib/prisma.ts
/* eslint-disable no-console */
import { PrismaClient } from '@prisma/client'

/**
 * テスト実行時（VITEST=true）は TEST_DATABASE_URL を使用し、
 * 通常実行時は DATABASE_URL を使用する
 */
const isTest = process.env.VITEST === 'true' || process.env.NODE_ENV === 'test'
const databaseUrl = isTest ? process.env.TEST_DATABASE_URL : process.env.DATABASE_URL

// デバッグ用ログ
if (process.env.DEBUG_PRISMA === 'true') {
  console.log('[Prisma] VITEST:', process.env.VITEST)
  console.log('[Prisma] NODE_ENV:', process.env.NODE_ENV)
  console.log('[Prisma] isTest:', isTest)
  console.log('[Prisma] Using DATABASE_URL:', databaseUrl)
}

export const prisma = new PrismaClient({
  datasources: {
    db: {
      url: databaseUrl
    }
  }
})
