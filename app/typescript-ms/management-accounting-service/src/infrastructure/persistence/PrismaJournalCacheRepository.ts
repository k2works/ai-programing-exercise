// src/infrastructure/persistence/PrismaJournalCacheRepository.ts

import { JournalCache, JournalCacheRepository } from '../../domain/models/journal-cache'
import { getPrismaClient } from './prisma-client'
import { Decimal } from '@prisma/client/runtime/library'

/**
 * Prisma を使用した JournalCache リポジトリの実装
 */
export class PrismaJournalCacheRepository implements JournalCacheRepository {
  private prisma = getPrismaClient()

  async save(journalCache: JournalCache): Promise<JournalCache> {
    const data = {
      journalId: journalCache.journalId,
      fiscalYear: journalCache.fiscalYear,
      journalDate: journalCache.journalDate,
      totalDebitAmount: new Decimal(journalCache.totalDebitAmount),
      totalCreditAmount: new Decimal(journalCache.totalCreditAmount),
      receivedAt: journalCache.receivedAt
    }

    // journalId をユニークキーとして upsert
    const saved = await this.prisma.journalCache.upsert({
      where: { journalId: journalCache.journalId },
      update: data,
      create: data
    })

    return this.toDomain(saved)
  }

  async findByFiscalYear(fiscalYear: number): Promise<JournalCache[]> {
    const caches = await this.prisma.journalCache.findMany({
      where: { fiscalYear },
      orderBy: { journalDate: 'asc' }
    })
    return caches.map((cache) => this.toDomain(cache))
  }

  async findAll(): Promise<JournalCache[]> {
    const caches = await this.prisma.journalCache.findMany({
      orderBy: { receivedAt: 'desc' }
    })
    return caches.map((cache) => this.toDomain(cache))
  }

  /**
   * Prisma モデルをドメインモデルに変換
   */
  private toDomain(prismaModel: any): JournalCache {
    return {
      journalId: prismaModel.journalId,
      fiscalYear: prismaModel.fiscalYear,
      journalDate: prismaModel.journalDate,
      totalDebitAmount: parseFloat(prismaModel.totalDebitAmount.toString()),
      totalCreditAmount: parseFloat(prismaModel.totalCreditAmount.toString()),
      receivedAt: prismaModel.receivedAt
    }
  }
}
