import { PrismaClient } from '@prisma/client';

/**
 * Prisma Client のシングルトンインスタンス
 *
 * グローバルスコープに保持することで、
 * ホットリロード時にも接続が再利用されます。
 */
const globalForPrisma = global as unknown as { prisma: PrismaClient };

export const prisma =
  globalForPrisma.prisma ||
  new PrismaClient({
    log: ['query', 'error', 'warn'],
  });

if (process.env.NODE_ENV !== 'production') {
  globalForPrisma.prisma = prisma;
}
