import { PrismaClient } from '@prisma/client'

export interface CreateCompletionResultInput {
  completionNumber: string
  workOrderNumber: string
  completionDate: Date
  itemCode: string
  completedQuantity: number
  defectQuantity?: number
  reporterCode?: string
  note?: string
  createdBy?: string
  inspectionResults?: {
    defectCode: string
    defectQuantity: number
  }[]
  consumption?: {
    consumptionDate: Date
    note?: string
    details: {
      lineNumber: number
      itemCode: string
      consumptionQuantity: number
      note?: string
    }[]
  }
}

export interface CompletionResultData {
  id: number
  completionNumber: string
  workOrderNumber: string
  completionDate: Date
  itemCode: string
  completedQuantity: number
  defectQuantity: number
  reporterCode: string | null
  note: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export interface CompletionInspectionData {
  id: number
  completionNumber: string
  defectCode: string
  defectQuantity: number
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export interface CompletionConsumptionData {
  id: number
  completionNumber: string
  consumptionDate: Date
  note: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export interface CompletionConsumptionDetailData {
  id: number
  completionNumber: string
  lineNumber: number
  itemCode: string
  consumptionQuantity: number
  note: string | null
}

export class CompletionResultRepository {
  constructor(private prisma: PrismaClient) {}

  async create(
    input: CreateCompletionResultInput
  ): Promise<
    CompletionResultData & {
      inspectionResults: CompletionInspectionData[]
      consumption: CompletionConsumptionData & { details: CompletionConsumptionDetailData[] } | null
    }
  > {
    return await this.prisma.$transaction(async (tx) => {
      // 完成実績データを作成
      const resultData = await tx.$queryRaw<CompletionResultData[]>`
        INSERT INTO completion_result_data (
          completion_number, work_order_number, completion_date,
          item_code, completed_quantity, defect_quantity,
          reporter_code, note, created_by
        )
        VALUES (
          ${input.completionNumber}, ${input.workOrderNumber}, ${input.completionDate},
          ${input.itemCode}, ${input.completedQuantity}, ${input.defectQuantity ?? 0},
          ${input.reporterCode ?? null}, ${input.note ?? null}, ${input.createdBy ?? null}
        )
        RETURNING
          id,
          completion_number as "completionNumber",
          work_order_number as "workOrderNumber",
          completion_date as "completionDate",
          item_code as "itemCode",
          completed_quantity as "completedQuantity",
          defect_quantity as "defectQuantity",
          reporter_code as "reporterCode",
          note,
          created_at as "createdAt",
          created_by as "createdBy",
          updated_at as "updatedAt",
          updated_by as "updatedBy"
      `
      const result = resultData[0]

      // 完成検査結果データを作成
      const inspectionResults: CompletionInspectionData[] = []
      if (input.inspectionResults && input.inspectionResults.length > 0) {
        for (const inspection of input.inspectionResults) {
          const inspectionData = await tx.$queryRaw<CompletionInspectionData[]>`
            INSERT INTO completion_inspection_data (
              completion_number, defect_code, defect_quantity, created_by
            )
            VALUES (
              ${input.completionNumber}, ${inspection.defectCode},
              ${inspection.defectQuantity}, ${input.createdBy ?? null}
            )
            RETURNING
              id,
              completion_number as "completionNumber",
              defect_code as "defectCode",
              defect_quantity as "defectQuantity",
              created_at as "createdAt",
              created_by as "createdBy",
              updated_at as "updatedAt",
              updated_by as "updatedBy"
          `
          inspectionResults.push(inspectionData[0])
        }
      }

      // 完成実績消費データを作成
      let consumption: (CompletionConsumptionData & { details: CompletionConsumptionDetailData[] }) | null = null
      if (input.consumption) {
        const consumptionData = await tx.$queryRaw<CompletionConsumptionData[]>`
          INSERT INTO completion_consumption_data (
            completion_number, consumption_date, note, created_by
          )
          VALUES (
            ${input.completionNumber}, ${input.consumption.consumptionDate},
            ${input.consumption.note ?? null}, ${input.createdBy ?? null}
          )
          RETURNING
            id,
            completion_number as "completionNumber",
            consumption_date as "consumptionDate",
            note,
            created_at as "createdAt",
            created_by as "createdBy",
            updated_at as "updatedAt",
            updated_by as "updatedBy"
        `
        const consumptionHeader = consumptionData[0]

        // 消費明細を作成
        const details: CompletionConsumptionDetailData[] = []
        for (const detail of input.consumption.details) {
          const detailData = await tx.$queryRaw<CompletionConsumptionDetailData[]>`
            INSERT INTO completion_consumption_detail_data (
              completion_number, line_number, item_code,
              consumption_quantity, note
            )
            VALUES (
              ${input.completionNumber}, ${detail.lineNumber}, ${detail.itemCode},
              ${detail.consumptionQuantity}, ${detail.note ?? null}
            )
            RETURNING
              id,
              completion_number as "completionNumber",
              line_number as "lineNumber",
              item_code as "itemCode",
              consumption_quantity as "consumptionQuantity",
              note
          `
          details.push(detailData[0])
        }

        consumption = {
          ...consumptionHeader,
          details,
        }
      }

      return {
        ...result,
        inspectionResults,
        consumption,
      }
    })
  }

  async findByNumber(
    completionNumber: string
  ): Promise<
    | (CompletionResultData & {
        inspectionResults: CompletionInspectionData[]
        consumption: CompletionConsumptionData & { details: CompletionConsumptionDetailData[] } | null
      })
    | null
  > {
    // 完成実績ヘッダーを取得
    const resultData = await this.prisma.$queryRaw<CompletionResultData[]>`
      SELECT
        id,
        completion_number as "completionNumber",
        work_order_number as "workOrderNumber",
        completion_date as "completionDate",
        item_code as "itemCode",
        completed_quantity as "completedQuantity",
        defect_quantity as "defectQuantity",
        reporter_code as "reporterCode",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM completion_result_data
      WHERE completion_number = ${completionNumber}
    `

    if (resultData.length === 0) {
      return null
    }

    const result = resultData[0]

    // 検査結果を取得
    const inspectionResults = await this.prisma.$queryRaw<CompletionInspectionData[]>`
      SELECT
        id,
        completion_number as "completionNumber",
        defect_code as "defectCode",
        defect_quantity as "defectQuantity",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM completion_inspection_data
      WHERE completion_number = ${completionNumber}
    `

    // 消費データを取得
    const consumptionData = await this.prisma.$queryRaw<CompletionConsumptionData[]>`
      SELECT
        id,
        completion_number as "completionNumber",
        consumption_date as "consumptionDate",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM completion_consumption_data
      WHERE completion_number = ${completionNumber}
    `

    let consumption: (CompletionConsumptionData & { details: CompletionConsumptionDetailData[] }) | null = null
    if (consumptionData.length > 0) {
      const consumptionHeader = consumptionData[0]

      // 消費明細を取得
      const details = await this.prisma.$queryRaw<CompletionConsumptionDetailData[]>`
        SELECT
          id,
          completion_number as "completionNumber",
          line_number as "lineNumber",
          item_code as "itemCode",
          consumption_quantity as "consumptionQuantity",
          note
        FROM completion_consumption_detail_data
        WHERE completion_number = ${completionNumber}
        ORDER BY line_number
      `

      consumption = {
        ...consumptionHeader,
        details,
      }
    }

    return {
      ...result,
      inspectionResults,
      consumption,
    }
  }

  async findByWorkOrder(workOrderNumber: string): Promise<CompletionResultData[]> {
    const result = await this.prisma.$queryRaw<CompletionResultData[]>`
      SELECT
        id,
        completion_number as "completionNumber",
        work_order_number as "workOrderNumber",
        completion_date as "completionDate",
        item_code as "itemCode",
        completed_quantity as "completedQuantity",
        defect_quantity as "defectQuantity",
        reporter_code as "reporterCode",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM completion_result_data
      WHERE work_order_number = ${workOrderNumber}
      ORDER BY completion_date
    `
    return result
  }
}
