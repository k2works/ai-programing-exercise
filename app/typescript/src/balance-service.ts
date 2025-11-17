// src/balance-service.ts
import { PrismaClient } from '@prisma/client'

export interface JournalDetail {
  accountCode: string
  subAccountCode?: string
  departmentCode?: string
  projectCode?: string
  debitOrCredit: '借' | '貸'
  amount: number
}

type TransactionClient = Omit<
  PrismaClient,
  '$connect' | '$disconnect' | '$on' | '$transaction' | '$use' | '$extends'
>

export class BalanceService {
  constructor(private prisma: PrismaClient) {}

  /**
   * 仕訳エントリと同時に日次残高を更新
   */
  async createJournalWithBalance(
    entryNo: string,
    entryDate: Date,
    details: JournalDetail[]
  ): Promise<void> {
    await this.prisma.$transaction(async (tx) => {
      await this.insertJournal(tx, entryNo, entryDate)

      for (let i = 0; i < details.length; i++) {
        await this.insertJournalDetail(tx, entryNo, i + 1, details[i], entryDate)
      }
    })
  }

  private async insertJournal(tx: TransactionClient, entryNo: string, entryDate: Date) {
    await tx.$executeRaw`
      INSERT INTO "仕訳" (
        "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
        "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ", "赤黒伝票番号"
      ) VALUES
      (${entryNo}, ${entryDate}, ${entryDate}, 0, 0, 0, 0, 0, 0)
    `
  }

  private async insertJournalDetail(
    tx: TransactionClient,
    entryNo: string,
    lineNo: number,
    detail: JournalDetail,
    entryDate: Date
  ) {
    await tx.$executeRaw`
      INSERT INTO "仕訳明細" (
        "仕訳伝票番号", "仕訳行番号", "行摘要"
      ) VALUES
      (${entryNo}, ${lineNo}, '')
    `

    await tx.$executeRaw`
      INSERT INTO "仕訳貸借明細" (
        "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
        "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "仕訳金額", "通貨コード", "為替レート", "基軸換算仕訳金額"
      ) VALUES
      (${entryNo}, ${lineNo}, ${detail.debitOrCredit}, ${detail.accountCode},
       ${detail.subAccountCode || ''}, ${detail.departmentCode || ''},
       ${detail.projectCode || ''}, ${detail.amount}, 'JPY', 1.00, ${detail.amount})
    `

    await this.upsertDailyBalance(tx, entryDate, detail)
  }

  private async upsertDailyBalance(tx: TransactionClient, entryDate: Date, detail: JournalDetail) {
    const debitAmount = detail.debitOrCredit === '借' ? detail.amount : 0
    const creditAmount = detail.debitOrCredit === '貸' ? detail.amount : 0

    await tx.$executeRaw`
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      (${entryDate}, ${detail.accountCode}, ${detail.subAccountCode || ''},
       ${detail.departmentCode || ''}, ${detail.projectCode || ''}, 0,
       ${debitAmount}, ${creditAmount})
      ON CONFLICT ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
      DO UPDATE SET
        "借方金額" = "日次勘定科目残高"."借方金額" + EXCLUDED."借方金額",
        "貸方金額" = "日次勘定科目残高"."貸方金額" + EXCLUDED."貸方金額"
    `
  }

  /**
   * 月次残高を更新
   */
  async updateMonthlyBalance(
    fiscalYearMonth: string,
    accountCode: string,
    subAccountCode: string = '',
    departmentCode: string = '',
    projectCode: string = '',
    settlementFlag: number = 0
  ): Promise<void> {
    await this.prisma.$executeRaw`
      INSERT INTO "月次勘定科目残高" (
        "会計年月", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ",
        "月初残高", "借方金額", "貸方金額", "月末残高"
      )
      SELECT
        ${fiscalYearMonth},
        ${accountCode},
        ${subAccountCode},
        ${departmentCode},
        ${projectCode},
        ${settlementFlag},
        COALESCE(
          (SELECT "月末残高" FROM "月次勘定科目残高"
           WHERE "会計年月" = (
             SELECT MAX("会計年月") FROM "月次勘定科目残高"
             WHERE "会計年月" < ${fiscalYearMonth}
             AND "勘定科目コード" = ${accountCode}
             AND "補助科目コード" = ${subAccountCode}
             AND "部門コード" = ${departmentCode}
             AND "プロジェクトコード" = ${projectCode}
             AND "決算仕訳フラグ" = ${settlementFlag}
           )
          ), 0
        ) as "月初残高",
        COALESCE(SUM("借方金額"), 0) as "借方金額",
        COALESCE(SUM("貸方金額"), 0) as "貸方金額",
        COALESCE(
          (SELECT "月末残高" FROM "月次勘定科目残高"
           WHERE "会計年月" = (
             SELECT MAX("会計年月") FROM "月次勘定科目残高"
             WHERE "会計年月" < ${fiscalYearMonth}
             AND "勘定科目コード" = ${accountCode}
             AND "補助科目コード" = ${subAccountCode}
             AND "部門コード" = ${departmentCode}
             AND "プロジェクトコード" = ${projectCode}
             AND "決算仕訳フラグ" = ${settlementFlag}
           )
          ), 0
        ) + COALESCE(SUM("借方金額"), 0) - COALESCE(SUM("貸方金額"), 0) as "月末残高"
      FROM "日次勘定科目残高"
      WHERE TO_CHAR("起票日", 'YYYYMM') = ${fiscalYearMonth}
      AND "勘定科目コード" = ${accountCode}
      AND "補助科目コード" = ${subAccountCode}
      AND "部門コード" = ${departmentCode}
      AND "プロジェクトコード" = ${projectCode}
      AND "決算仕訳フラグ" = ${settlementFlag}
      ON CONFLICT ("会計年月", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
      DO UPDATE SET
        "借方金額" = EXCLUDED."借方金額",
        "貸方金額" = EXCLUDED."貸方金額",
        "月末残高" = EXCLUDED."月末残高"
    `
  }
}
