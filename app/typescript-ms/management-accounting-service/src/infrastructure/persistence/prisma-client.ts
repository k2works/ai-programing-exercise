// src/infrastructure/persistence/prisma-client.ts

import { PrismaClient } from '@prisma/client'

let prisma: PrismaClient | null = null

/**
 * Prisma クライアントのシングルトンインスタンスを取得
 */
export function getPrismaClient(): PrismaClient {
  if (!prisma) {
    prisma = new PrismaClient({
      log: process.env.NODE_ENV === 'development' ? ['query', 'info', 'warn', 'error'] : ['error']
    })
  }
  return prisma
}

/**
 * Prisma クライアントを閉じる
 */
export async function closePrismaClient(): Promise<void> {
  if (prisma) {
    await prisma.$disconnect()
    prisma = null
  }
}
