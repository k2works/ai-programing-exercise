// src/infrastructure/persistence/PrismaJournalRepository.ts

import { JournalRepository, Journal, JournalDetailItem } from '../../application/ports/JournalRepository'
import { getPrismaClient } from './prisma-client'
import { Decimal } from '@prisma/client/runtime/library'

/**
 * Prisma ベースの仕訳リポジトリ実装（Adapter）
 */
export class PrismaJournalRepository implements JournalRepository {
  private prisma = getPrismaClient()

  async save(journal: Journal): Promise<Journal> {
    if (journal.id) {
      // 更新の場合
      const updated = await this.prisma.journal.update({
        where: { id: journal.id },
        data: {
          journalDate: journal.journalDate,
          fiscalYear: journal.fiscalYear,
          description: journal.description,
          userId: journal.userId,
          userName: journal.userName,
          detailItems: {
            deleteMany: {},
            create: journal.detailItems.map((item) => ({
              accountCode: item.accountCode,
              debitAmount: new Decimal(item.debitAmount),
              creditAmount: new Decimal(item.creditAmount),
              description: item.description
            }))
          }
        },
        include: {
          detailItems: true
        }
      })

      return this.toDomain(updated)
    } else {
      // 新規作成の場合
      const created = await this.prisma.journal.create({
        data: {
          journalDate: journal.journalDate,
          fiscalYear: journal.fiscalYear,
          description: journal.description,
          userId: journal.userId,
          userName: journal.userName,
          detailItems: {
            create: journal.detailItems.map((item) => ({
              accountCode: item.accountCode,
              debitAmount: new Decimal(item.debitAmount),
              creditAmount: new Decimal(item.creditAmount),
              description: item.description
            }))
          }
        },
        include: {
          detailItems: true
        }
      })

      return this.toDomain(created)
    }
  }

  async findAll(): Promise<Journal[]> {
    const journals = await this.prisma.journal.findMany({
      include: {
        detailItems: true
      },
      orderBy: {
        journalDate: 'desc'
      }
    })

    return journals.map((journal) => this.toDomain(journal))
  }

  async findById(id: number): Promise<Journal | null> {
    const journal = await this.prisma.journal.findUnique({
      where: { id },
      include: {
        detailItems: true
      }
    })

    return journal ? this.toDomain(journal) : null
  }

  async findByFiscalYear(fiscalYear: number): Promise<Journal[]> {
    const journals = await this.prisma.journal.findMany({
      where: { fiscalYear },
      include: {
        detailItems: true
      },
      orderBy: {
        journalDate: 'desc'
      }
    })

    return journals.map((journal) => this.toDomain(journal))
  }

  async deleteById(id: number): Promise<void> {
    await this.prisma.journal.delete({
      where: { id }
    })
  }

  async deleteAll(): Promise<void> {
    await this.prisma.journal.deleteMany({})
  }

  /**
   * Prisma データモデルをドメインモデルに変換
   */
  private toDomain(data: any): Journal {
    return {
      id: data.id,
      journalDate: data.journalDate,
      fiscalYear: data.fiscalYear,
      description: data.description,
      userId: data.userId ?? undefined,
      userName: data.userName ?? undefined,
      detailItems: data.detailItems.map((item: any) => ({
        id: item.id,
        journalId: item.journalId,
        accountCode: item.accountCode,
        debitAmount: typeof item.debitAmount === 'object' ? item.debitAmount.toNumber() : item.debitAmount,
        creditAmount: typeof item.creditAmount === 'object' ? item.creditAmount.toNumber() : item.creditAmount,
        description: item.description ?? undefined
      }))
    }
  }
}
