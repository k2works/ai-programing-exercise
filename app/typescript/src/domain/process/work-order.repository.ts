import { PrismaClient } from '@prisma/client'

export const WorkOrderStatus = {
  NOT_STARTED: 'NOT_STARTED',
  IN_PROGRESS: 'IN_PROGRESS',
  COMPLETED: 'COMPLETED',
  SUSPENDED: 'SUSPENDED',
} as const

export type WorkOrderStatusValue = (typeof WorkOrderStatus)[keyof typeof WorkOrderStatus]

export interface CreateWorkOrderInput {
  workOrderNumber: string
  productionOrderNumber: string
  workOrderDate: Date
  itemCode: string
  orderQuantity: number
  scheduledStartDate: Date
  scheduledEndDate: Date
  status?: WorkOrderStatusValue
  note?: string
  createdBy?: string
  details: {
    sequence: number
    processCode: string
    scheduledStartTime?: Date
    scheduledEndTime?: Date
  }[]
}

export interface WorkOrderData {
  id: number
  workOrderNumber: string
  productionOrderNumber: string
  workOrderDate: Date
  itemCode: string
  orderQuantity: number
  completedQuantity: number
  scheduledStartDate: Date
  scheduledEndDate: Date
  status: string
  note: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export interface WorkOrderDetailData {
  id: number
  workOrderNumber: string
  sequence: number
  processCode: string
  scheduledStartTime: Date | null
  scheduledEndTime: Date | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export class WorkOrderRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateWorkOrderInput): Promise<WorkOrderData & { details: WorkOrderDetailData[] }> {
    return await this.prisma.$transaction(async (tx) => {
      // 作業指示データを作成
      const workOrderResult = await tx.$queryRaw<WorkOrderData[]>`
        INSERT INTO work_order_data (
          work_order_number, production_order_number, work_order_date,
          item_code, order_quantity, scheduled_start_date, scheduled_end_date,
          status, note, created_by
        )
        VALUES (
          ${input.workOrderNumber}, ${input.productionOrderNumber}, ${input.workOrderDate},
          ${input.itemCode}, ${input.orderQuantity}, ${input.scheduledStartDate},
          ${input.scheduledEndDate}, ${input.status ?? WorkOrderStatus.NOT_STARTED}::work_order_status,
          ${input.note ?? null}, ${input.createdBy ?? null}
        )
        RETURNING
          id,
          work_order_number as "workOrderNumber",
          production_order_number as "productionOrderNumber",
          work_order_date as "workOrderDate",
          item_code as "itemCode",
          order_quantity as "orderQuantity",
          completed_quantity as "completedQuantity",
          scheduled_start_date as "scheduledStartDate",
          scheduled_end_date as "scheduledEndDate",
          status,
          note,
          created_at as "createdAt",
          created_by as "createdBy",
          updated_at as "updatedAt",
          updated_by as "updatedBy"
      `
      const workOrder = workOrderResult[0]

      // 明細データを作成
      const details: WorkOrderDetailData[] = []
      for (const detail of input.details) {
        const detailResult = await tx.$queryRaw<WorkOrderDetailData[]>`
          INSERT INTO work_order_detail_data (
            work_order_number, sequence, process_code,
            scheduled_start_time, scheduled_end_time
          )
          VALUES (
            ${input.workOrderNumber}, ${detail.sequence}, ${detail.processCode},
            ${detail.scheduledStartTime ?? null}, ${detail.scheduledEndTime ?? null}
          )
          RETURNING
            id,
            work_order_number as "workOrderNumber",
            sequence,
            process_code as "processCode",
            scheduled_start_time as "scheduledStartTime",
            scheduled_end_time as "scheduledEndTime",
            created_at as "createdAt",
            created_by as "createdBy",
            updated_at as "updatedAt",
            updated_by as "updatedBy"
        `
        details.push(detailResult[0])
      }

      return {
        ...workOrder,
        details,
      }
    })
  }

  async findByNumber(workOrderNumber: string): Promise<(WorkOrderData & { details: WorkOrderDetailData[] }) | null> {
    // 作業指示ヘッダーを取得
    const workOrderResult = await this.prisma.$queryRaw<WorkOrderData[]>`
      SELECT
        id,
        work_order_number as "workOrderNumber",
        production_order_number as "productionOrderNumber",
        work_order_date as "workOrderDate",
        item_code as "itemCode",
        order_quantity as "orderQuantity",
        completed_quantity as "completedQuantity",
        scheduled_start_date as "scheduledStartDate",
        scheduled_end_date as "scheduledEndDate",
        status,
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM work_order_data
      WHERE work_order_number = ${workOrderNumber}
    `

    if (workOrderResult.length === 0) {
      return null
    }

    const workOrder = workOrderResult[0]

    // 明細を取得
    const details = await this.prisma.$queryRaw<WorkOrderDetailData[]>`
      SELECT
        id,
        work_order_number as "workOrderNumber",
        sequence,
        process_code as "processCode",
        scheduled_start_time as "scheduledStartTime",
        scheduled_end_time as "scheduledEndTime",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM work_order_detail_data
      WHERE work_order_number = ${workOrderNumber}
      ORDER BY sequence
    `

    return {
      ...workOrder,
      details,
    }
  }

  async findByProductionOrder(productionOrderNumber: string): Promise<WorkOrderData[]> {
    const result = await this.prisma.$queryRaw<WorkOrderData[]>`
      SELECT
        id,
        work_order_number as "workOrderNumber",
        production_order_number as "productionOrderNumber",
        work_order_date as "workOrderDate",
        item_code as "itemCode",
        order_quantity as "orderQuantity",
        completed_quantity as "completedQuantity",
        scheduled_start_date as "scheduledStartDate",
        scheduled_end_date as "scheduledEndDate",
        status,
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM work_order_data
      WHERE production_order_number = ${productionOrderNumber}
      ORDER BY work_order_date
    `
    return result
  }

  async updateStatus(workOrderNumber: string, status: WorkOrderStatusValue): Promise<void> {
    await this.prisma.$executeRaw`
      UPDATE work_order_data
      SET status = ${status}::work_order_status, updated_at = CURRENT_TIMESTAMP
      WHERE work_order_number = ${workOrderNumber}
    `
  }

  async updateCompletedQuantity(workOrderNumber: string, completedQuantity: number): Promise<void> {
    await this.prisma.$executeRaw`
      UPDATE work_order_data
      SET completed_quantity = ${completedQuantity}, updated_at = CURRENT_TIMESTAMP
      WHERE work_order_number = ${workOrderNumber}
    `
  }
}
