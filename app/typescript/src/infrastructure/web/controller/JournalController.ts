// src/infrastructure/web/controller/JournalController.ts
import { FastifyRequest, FastifyReply } from 'fastify'
import { JournalUseCase } from '../../../application/port/in/JournalUseCase'
import { CreateJournalRequestDto, UpdateJournalRequestDto } from '../dto/JournalRequestDto'
import { toJournalResponseDto } from '../dto/JournalResponseDto'

/**
 * 仕訳コントローラー（Input Adapter）
 * Fastify を使用して HTTP リクエストを処理
 */
export class JournalController {
  constructor(private journalUseCase: JournalUseCase) {}

  /**
   * POST /journals
   * 新しい仕訳を作成
   */
  async createJournal(
    request: FastifyRequest<{ Body: CreateJournalRequestDto }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const command = {
        ...request.body,
        journalDate: new Date(request.body.journalDate),
        inputDate: new Date(request.body.inputDate),
        details: request.body.details.map((detail) => ({
          ...detail,
          items: detail.items.map((item) => ({
            ...item,
            dueDate: item.dueDate ? new Date(item.dueDate) : undefined
          }))
        }))
      }

      const journal = await this.journalUseCase.createJournal(command)
      const response = toJournalResponseDto(journal)

      reply.status(201).send({
        success: true,
        data: { journal: response }
      })
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  /**
   * GET /journals
   * すべての仕訳を取得
   */
  async getAllJournals(request: FastifyRequest, reply: FastifyReply): Promise<void> {
    const journals = await this.journalUseCase.getAllJournals()
    const response = journals.map((j) => toJournalResponseDto(j))

    reply.send({
      success: true,
      data: { journals: response }
    })
  }

  /**
   * GET /journals/:voucherNo
   * 伝票番号で仕訳を取得
   */
  async getJournalByVoucherNo(
    request: FastifyRequest<{ Params: { voucherNo: string } }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const journal = await this.journalUseCase.getJournalByVoucherNo(request.params.voucherNo)
      const response = toJournalResponseDto(journal)

      reply.send({
        success: true,
        data: { journal: response }
      })
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  /**
   * GET /journals/date-range
   * 日付範囲で仕訳を取得
   */
  async getJournalsByDateRange(
    request: FastifyRequest<{
      Querystring: { startDate: string; endDate: string }
    }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const { startDate, endDate } = request.query
      const journals = await this.journalUseCase.getJournalsByDateRange(
        new Date(startDate),
        new Date(endDate)
      )
      const response = journals.map((j) => toJournalResponseDto(j))

      reply.send({
        success: true,
        data: { journals: response }
      })
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  /**
   * PUT /journals/:voucherNo
   * 仕訳を更新
   */
  async updateJournal(
    request: FastifyRequest<{
      Params: { voucherNo: string }
      Body: UpdateJournalRequestDto
    }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      const command = {
        voucherNo: request.params.voucherNo,
        ...request.body,
        journalDate: request.body.journalDate ? new Date(request.body.journalDate) : undefined,
        inputDate: request.body.inputDate ? new Date(request.body.inputDate) : undefined,
        details: request.body.details?.map((detail) => ({
          ...detail,
          items: detail.items.map((item) => ({
            ...item,
            dueDate: item.dueDate ? new Date(item.dueDate) : undefined
          }))
        }))
      }

      const journal = await this.journalUseCase.updateJournal(command)
      const response = toJournalResponseDto(journal)

      reply.send({
        success: true,
        data: { journal: response }
      })
    } catch (error) {
      this.handleError(error, reply)
    }
  }

  /**
   * DELETE /journals/:voucherNo
   * 仕訳を削除
   */
  async deleteJournal(
    request: FastifyRequest<{ Params: { voucherNo: string } }>,
    reply: FastifyReply
  ): Promise<void> {
    try {
      await this.journalUseCase.deleteJournal(request.params.voucherNo)

      reply.send({
        success: true,
        message: `伝票番号 ${request.params.voucherNo} の仕訳を削除しました`
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
      this.handleKnownError(error, reply)
    } else {
      this.handleUnknownError(reply)
    }
  }

  /**
   * 既知のエラーを処理
   */
  private handleKnownError(error: Error, reply: FastifyReply): void {
    const errorInfo = this.getErrorInfo(error.message)
    reply.status(errorInfo.status).send({
      success: false,
      error: error.message,
      code: errorInfo.code
    })
  }

  /**
   * エラーメッセージから適切なステータスコードとコードを取得
   */
  private getErrorInfo(message: string): { status: number; code: string } {
    if (message.includes('見つかりません')) {
      return { status: 404, code: 'JOURNAL_NOT_FOUND' }
    }
    if (message.includes('既に使用されています')) {
      return { status: 409, code: 'JOURNAL_ALREADY_EXISTS' }
    }
    if (message.includes('一致しません') || message.includes('存在しません')) {
      return { status: 400, code: 'INVALID_JOURNAL_DATA' }
    }
    return { status: 400, code: 'BAD_REQUEST' }
  }

  /**
   * 未知のエラーを処理
   */
  private handleUnknownError(reply: FastifyReply): void {
    reply.status(500).send({
      success: false,
      error: '予期しないエラーが発生しました',
      code: 'INTERNAL_SERVER_ERROR'
    })
  }
}
