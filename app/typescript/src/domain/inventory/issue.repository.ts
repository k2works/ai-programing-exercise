import { PrismaClient, Prisma } from '@prisma/client'
import { InventoryRepository } from './inventory.repository'

export interface CreateIssueInstructionInput {
  issueInstructionNumber: string
  workOrderNumber: string
  sequence: number
  locationCode: string
  issueDate: Date
  details: {
    issueLineNumber: number
    itemCode: string
    sequence: number
    issueQuantity: number
  }[]
}

export interface CreateIssueInput {
  issueNumber: string
  workOrderNumber: string
  sequence: number
  locationCode: string
  issueDate: Date
  issuerCode?: string
  createdBy?: string
  details: {
    issueLineNumber: number
    itemCode: string
    issueQuantity: number
  }[]
  stockStatus?: 'accepted' | 'defect' | 'uninspected'
}

export interface IssueInstructionDetailData {
  id: number
  issueInstructionNumber: string
  issueLineNumber: number
  itemCode: string
  sequence: number
  issueQuantity: Prisma.Decimal
  createdAt: Date
  updatedAt: Date
}

export interface IssueInstructionData {
  id: number
  issueInstructionNumber: string
  workOrderNumber: string
  sequence: number
  locationCode: string
  issueDate: Date
  createdAt: Date
  updatedAt: Date
  details: IssueInstructionDetailData[]
}

export interface IssueDetailData {
  id: number
  issueNumber: string
  issueLineNumber: number
  itemCode: string
  issueQuantity: Prisma.Decimal
  createdAt: Date
  updatedAt: Date
}

export interface IssueData {
  id: number
  issueNumber: string
  workOrderNumber: string
  sequence: number
  locationCode: string
  issueDate: Date
  issuerCode: string | null
  createdBy: string | null
  createdAt: Date
  updatedAt: Date
  details: IssueDetailData[]
}

export class IssueRepository {
  private inventoryRepository: InventoryRepository

  constructor(private prisma: PrismaClient) {
    this.inventoryRepository = new InventoryRepository(prisma)
  }

  async createInstruction(input: CreateIssueInstructionInput): Promise<IssueInstructionData> {
    return await this.prisma.$transaction(async (tx) => {
      // 払出指示ヘッダーを作成
      const headerData = await tx.$queryRaw<IssueInstructionData[]>`
        INSERT INTO issue_instruction_data (
          issue_instruction_number, work_order_number, sequence,
          location_code, issue_date
        )
        VALUES (
          ${input.issueInstructionNumber}, ${input.workOrderNumber}, ${input.sequence},
          ${input.locationCode}, ${input.issueDate}
        )
        RETURNING
          id,
          issue_instruction_number as "issueInstructionNumber",
          work_order_number as "workOrderNumber",
          sequence,
          location_code as "locationCode",
          issue_date as "issueDate",
          created_at as "createdAt",
          updated_at as "updatedAt"
      `
      const header = headerData[0]

      // 払出指示明細を作成
      const details: IssueInstructionDetailData[] = []
      for (const detail of input.details) {
        const detailData = await tx.$queryRaw<IssueInstructionDetailData[]>`
          INSERT INTO issue_instruction_detail_data (
            issue_instruction_number, issue_line_number, item_code,
            sequence, issue_quantity
          )
          VALUES (
            ${input.issueInstructionNumber}, ${detail.issueLineNumber}, ${detail.itemCode},
            ${detail.sequence}, ${detail.issueQuantity}
          )
          RETURNING
            id,
            issue_instruction_number as "issueInstructionNumber",
            issue_line_number as "issueLineNumber",
            item_code as "itemCode",
            sequence,
            issue_quantity as "issueQuantity",
            created_at as "createdAt",
            updated_at as "updatedAt"
        `
        details.push(detailData[0])
      }

      return {
        ...header,
        details,
      }
    })
  }

  async findInstructionByNumber(
    issueInstructionNumber: string
  ): Promise<IssueInstructionData | null> {
    const headers = await this.prisma.$queryRaw<IssueInstructionData[]>`
      SELECT
        id,
        issue_instruction_number as "issueInstructionNumber",
        work_order_number as "workOrderNumber",
        sequence,
        location_code as "locationCode",
        issue_date as "issueDate",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM issue_instruction_data
      WHERE issue_instruction_number = ${issueInstructionNumber}
    `

    if (headers.length === 0) {
      return null
    }

    const details = await this.prisma.$queryRaw<IssueInstructionDetailData[]>`
      SELECT
        id,
        issue_instruction_number as "issueInstructionNumber",
        issue_line_number as "issueLineNumber",
        item_code as "itemCode",
        sequence,
        issue_quantity as "issueQuantity",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM issue_instruction_detail_data
      WHERE issue_instruction_number = ${issueInstructionNumber}
      ORDER BY issue_line_number
    `

    return {
      ...headers[0],
      details,
    }
  }

  async findInstructionsByWorkOrder(
    workOrderNumber: string,
    sequence: number
  ): Promise<IssueInstructionData[]> {
    const headers = await this.prisma.$queryRaw<IssueInstructionData[]>`
      SELECT
        id,
        issue_instruction_number as "issueInstructionNumber",
        work_order_number as "workOrderNumber",
        sequence,
        location_code as "locationCode",
        issue_date as "issueDate",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM issue_instruction_data
      WHERE work_order_number = ${workOrderNumber}
        AND sequence = ${sequence}
      ORDER BY issue_date
    `

    const results: IssueInstructionData[] = []

    for (const header of headers) {
      const details = await this.prisma.$queryRaw<IssueInstructionDetailData[]>`
        SELECT
          id,
          issue_instruction_number as "issueInstructionNumber",
          issue_line_number as "issueLineNumber",
          item_code as "itemCode",
          sequence,
          issue_quantity as "issueQuantity",
          created_at as "createdAt",
          updated_at as "updatedAt"
        FROM issue_instruction_detail_data
        WHERE issue_instruction_number = ${header.issueInstructionNumber}
        ORDER BY issue_line_number
      `

      results.push({
        ...header,
        details,
      })
    }

    return results
  }

  async createIssue(input: CreateIssueInput): Promise<IssueData> {
    const stockStatus = input.stockStatus || 'accepted'

    return await this.prisma.$transaction(async (tx) => {
      // 払出ヘッダーを作成
      const headerData = await tx.$queryRaw<IssueData[]>`
        INSERT INTO issue_data (
          issue_number, work_order_number, sequence,
          location_code, issue_date, issuer_code, created_by
        )
        VALUES (
          ${input.issueNumber}, ${input.workOrderNumber}, ${input.sequence},
          ${input.locationCode}, ${input.issueDate}, ${input.issuerCode}, ${input.createdBy}
        )
        RETURNING
          id,
          issue_number as "issueNumber",
          work_order_number as "workOrderNumber",
          sequence,
          location_code as "locationCode",
          issue_date as "issueDate",
          issuer_code as "issuerCode",
          created_by as "createdBy",
          created_at as "createdAt",
          updated_at as "updatedAt"
      `
      const header = headerData[0]

      // 払出明細を作成し、在庫を減少
      const details: IssueDetailData[] = []
      for (const detail of input.details) {
        const detailData = await tx.$queryRaw<IssueDetailData[]>`
          INSERT INTO issue_detail_data (
            issue_number, issue_line_number, item_code, issue_quantity
          )
          VALUES (
            ${input.issueNumber}, ${detail.issueLineNumber}, ${detail.itemCode}, ${detail.issueQuantity}
          )
          RETURNING
            id,
            issue_number as "issueNumber",
            issue_line_number as "issueLineNumber",
            item_code as "itemCode",
            issue_quantity as "issueQuantity",
            created_at as "createdAt",
            updated_at as "updatedAt"
        `
        details.push(detailData[0])

        // 在庫を減少（トランザクション内で実行）
        const inventoryRepo = new InventoryRepository(tx as unknown as PrismaClient)
        await inventoryRepo.decrease({
          locationCode: input.locationCode,
          itemCode: detail.itemCode,
          quantity: detail.issueQuantity,
          status: stockStatus,
        })
      }

      return {
        ...header,
        details,
      }
    })
  }

  async findByNumber(issueNumber: string): Promise<IssueData | null> {
    const headers = await this.prisma.$queryRaw<IssueData[]>`
      SELECT
        id,
        issue_number as "issueNumber",
        work_order_number as "workOrderNumber",
        sequence,
        location_code as "locationCode",
        issue_date as "issueDate",
        issuer_code as "issuerCode",
        created_by as "createdBy",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM issue_data
      WHERE issue_number = ${issueNumber}
    `

    if (headers.length === 0) {
      return null
    }

    const details = await this.prisma.$queryRaw<IssueDetailData[]>`
      SELECT
        id,
        issue_number as "issueNumber",
        issue_line_number as "issueLineNumber",
        item_code as "itemCode",
        issue_quantity as "issueQuantity",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM issue_detail_data
      WHERE issue_number = ${issueNumber}
      ORDER BY issue_line_number
    `

    return {
      ...headers[0],
      details,
    }
  }

  async findByWorkOrder(workOrderNumber: string, sequence: number): Promise<IssueData[]> {
    const headers = await this.prisma.$queryRaw<IssueData[]>`
      SELECT
        id,
        issue_number as "issueNumber",
        work_order_number as "workOrderNumber",
        sequence,
        location_code as "locationCode",
        issue_date as "issueDate",
        issuer_code as "issuerCode",
        created_by as "createdBy",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM issue_data
      WHERE work_order_number = ${workOrderNumber}
        AND sequence = ${sequence}
      ORDER BY issue_date
    `

    const results: IssueData[] = []

    for (const header of headers) {
      const details = await this.prisma.$queryRaw<IssueDetailData[]>`
        SELECT
          id,
          issue_number as "issueNumber",
          issue_line_number as "issueLineNumber",
          item_code as "itemCode",
          issue_quantity as "issueQuantity",
          created_at as "createdAt",
          updated_at as "updatedAt"
        FROM issue_detail_data
        WHERE issue_number = ${header.issueNumber}
        ORDER BY issue_line_number
      `

      results.push({
        ...header,
        details,
      })
    }

    return results
  }
}
