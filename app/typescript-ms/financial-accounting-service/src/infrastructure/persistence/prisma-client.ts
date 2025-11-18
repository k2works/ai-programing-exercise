// src/infrastructure/persistence/prisma-client.ts

import { PrismaClient } from '@prisma/client'
import { config } from '../../config'

/**
 * Prisma Client のシングルトンインスタンス
 */
let prisma: PrismaClient

export function getPrismaClient(): PrismaClient {
  if (!prisma) {
    prisma = new PrismaClient({
      log: config.isDevelopment ? ['query', 'info', 'warn', 'error'] : ['warn', 'error']
    })
  }
  return prisma
}

/**
 * Prisma Client を閉じる
 */
export async function closePrismaClient(): Promise<void> {
  if (prisma) {
    await prisma.$disconnect()
  }
}
