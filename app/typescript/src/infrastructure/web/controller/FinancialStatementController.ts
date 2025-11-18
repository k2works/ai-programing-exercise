// src/infrastructure/web/controller/FinancialStatementController.ts
import { FastifyRequest, FastifyReply } from 'fastify'
import { FinancialStatementUseCase } from '../../../application/port/in/FinancialStatementUseCase'
import { BalanceSheetDto, IncomeStatementDto } from '../dto/FinancialStatementResponseDto'

/**
 * 財務諸表コントローラー（Input Adapter）
 * Fastify を使用して HTTP リクエストを処理
 */
export class FinancialStatementController {
  constructor(private financialStatementUseCase: FinancialStatementUseCase) {}

  /**
   * GET /statements/balance-sheet
   * 貸借対照表を取得
   */
  async getBalanceSheet(
    request: FastifyRequest<{ Querystring: { asOfDate: string } }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const asOfDate = new Date(request.query.asOfDate)
      const balanceSheet = await this.financialStatementUseCase.generateBalanceSheet(asOfDate)

      const response: BalanceSheetDto = {
        asOfDate: balanceSheet.asOfDate.toISOString().split('T')[0],
        assets: balanceSheet.assets,
        liabilities: balanceSheet.liabilities,
        equity: balanceSheet.equity,
        totalAssets: balanceSheet.totalAssets,
        totalLiabilities: balanceSheet.totalLiabilities,
        totalEquity: balanceSheet.totalEquity
      }

      reply.send({
        success: true,
        data: { balanceSheet: response }
      })
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  /**
   * GET /statements/income-statement
   * 損益計算書を取得
   */
  async getIncomeStatement(
    request: FastifyRequest<{ Querystring: { fromDate: string; toDate: string } }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const { fromDate, toDate } = request.query
      const incomeStatement = await this.financialStatementUseCase.generateIncomeStatement(
        new Date(fromDate),
        new Date(toDate)
      )

      const response: IncomeStatementDto = {
        fromDate: incomeStatement.fromDate.toISOString().split('T')[0],
        toDate: incomeStatement.toDate.toISOString().split('T')[0],
        revenues: incomeStatement.revenues,
        expenses: incomeStatement.expenses,
        totalRevenues: incomeStatement.totalRevenues,
        totalExpenses: incomeStatement.totalExpenses,
        grossProfit: incomeStatement.grossProfit,
        operatingIncome: incomeStatement.operatingIncome,
        netIncome: incomeStatement.netIncome
      }

      reply.send({
        success: true,
        data: { incomeStatement: response }
      })
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  /**
   * エラーハンドリング
   */
  private handleError(error: unknown, reply: FastifyReply): void {
    if (error instanceof Error) {
      const message = error.message

      reply.status(400).send({
        success: false,
        error: message,
        code: 'BAD_REQUEST'
      })
    } else {
      reply.status(500).send({
        success: false,
        error: '予期しないエラーが発生しました',
        code: 'INTERNAL_SERVER_ERROR'
      })
    }
  }
}
