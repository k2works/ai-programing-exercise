// src/infrastructure/persistence/JournalPersistenceAdapter.ts
import { PrismaClient } from '@prisma/client'
import { JournalRepository } from '../../application/port/out/JournalRepository'
import { JournalWithDetails } from '../../application/port/in/JournalUseCase'

/**
 * 仕訳永続化アダプター（Output Adapter）
 * Prisma を使用してデータベースアクセスを実装
 */
export class JournalPersistenceAdapter implements JournalRepository {
  constructor(private prisma: PrismaClient) {}

  async save(journal: JournalWithDetails): Promise<JournalWithDetails> {
    // 既存の仕訳を削除してから再作成（更新の場合）
    await this.prisma.journal.deleteMany({
      where: { voucherNo: journal.voucherNo }
    })

    // 新規作成
    const created = await this.prisma.journal.create({
      data: {
        voucherNo: journal.voucherNo,
        journalDate: journal.journalDate,
        inputDate: journal.inputDate,
        settlementFlag: journal.settlementFlag,
        singleFlag: journal.singleFlag,
        voucherType: journal.voucherType,
        recurringFlag: journal.recurringFlag,
        employeeCode: journal.employeeCode,
        departmentCode: journal.departmentCode,
        redSlipFlag: journal.redSlipFlag,
        redBlackVoucherNo: journal.redBlackVoucherNo,
        details: {
          create: journal.details.map((detail) => ({
            lineNo: detail.lineNo,
            lineSummary: detail.lineSummary,
            items: {
              create: detail.items.map((item) => ({
                debitCredit: item.debitCredit,
                currencyCode: item.currencyCode,
                exchangeRate: item.exchangeRate,
                departmentCode: item.departmentCode,
                projectCode: item.projectCode,
                accountCode: item.accountCode,
                subAccountCode: item.subAccountCode,
                amount: item.amount,
                baseAmount: item.baseAmount,
                taxType: item.taxType,
                taxRate: item.taxRate,
                taxCalcType: item.taxCalcType,
                dueDate: item.dueDate,
                cashFlowFlag: item.cashFlowFlag,
                segmentCode: item.segmentCode,
                offsetAccountCode: item.offsetAccountCode,
                offsetSubAccount: item.offsetSubAccount,
                noteCode: item.noteCode,
                noteContent: item.noteContent
              }))
            }
          }))
        }
      },
      include: {
        details: {
          include: {
            items: true
          },
          orderBy: {
            lineNo: 'asc'
          }
        }
      }
    })

    return created as JournalWithDetails
  }

  async findAll(): Promise<JournalWithDetails[]> {
    const journals = await this.prisma.journal.findMany({
      include: {
        details: {
          include: {
            items: true
          },
          orderBy: {
            lineNo: 'asc'
          }
        }
      },
      orderBy: {
        journalDate: 'desc'
      }
    })

    return journals as JournalWithDetails[]
  }

  async findByVoucherNo(voucherNo: string): Promise<JournalWithDetails | null> {
    const journal = await this.prisma.journal.findUnique({
      where: { voucherNo },
      include: {
        details: {
          include: {
            items: true
          },
          orderBy: {
            lineNo: 'asc'
          }
        }
      }
    })

    return journal as JournalWithDetails | null
  }

  async findByDateRange(startDate: Date, endDate: Date): Promise<JournalWithDetails[]> {
    const journals = await this.prisma.journal.findMany({
      where: {
        journalDate: {
          gte: startDate,
          lte: endDate
        }
      },
      include: {
        details: {
          include: {
            items: true
          },
          orderBy: {
            lineNo: 'asc'
          }
        }
      },
      orderBy: {
        journalDate: 'asc'
      }
    })

    return journals as JournalWithDetails[]
  }

  async findByDepartmentCode(departmentCode: string): Promise<JournalWithDetails[]> {
    const journals = await this.prisma.journal.findMany({
      where: { departmentCode },
      include: {
        details: {
          include: {
            items: true
          },
          orderBy: {
            lineNo: 'asc'
          }
        }
      },
      orderBy: {
        journalDate: 'desc'
      }
    })

    return journals as JournalWithDetails[]
  }

  async deleteByVoucherNo(voucherNo: string): Promise<void> {
    await this.prisma.journal.delete({
      where: { voucherNo }
    })
  }

  async deleteAll(): Promise<void> {
    await this.prisma.journal.deleteMany()
  }
}
