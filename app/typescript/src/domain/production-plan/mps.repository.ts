import { PrismaClient } from '@prisma/client'

// 計画ステータス
export const PlanStatus = {
  DRAFT: 'DRAFT', // 草案
  CONFIRMED: 'CONFIRMED', // 確定
  EXPANDED: 'EXPANDED', // 展開済
  CANCELLED: 'CANCELLED', // 取消
} as const

export type PlanStatusValue = (typeof PlanStatus)[keyof typeof PlanStatus]

// 基準生産計画の入力型
export interface CreateMPSInput {
  mpsNumber: string
  planDate: Date
  itemCode: string
  plannedQuantity: number
  dueDate: Date
  status?: PlanStatusValue
  locationCode?: string
  note?: string
  createdBy?: string
}

// 基準生産計画の出力型
export interface MPS {
  id: number
  mpsNumber: string
  planDate: Date
  itemCode: string
  plannedQuantity: number
  dueDate: Date
  status: string
  locationCode: string | null
  note: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export class MPSRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateMPSInput): Promise<MPS> {
    const result = await this.prisma.$queryRaw<MPS[]>`
      INSERT INTO master_production_schedules (
        mps_number, plan_date, item_code, planned_quantity, due_date,
        status, location_code, note, created_by
      )
      VALUES (
        ${input.mpsNumber}, ${input.planDate}, ${input.itemCode},
        ${input.plannedQuantity}, ${input.dueDate},
        ${input.status ?? PlanStatus.DRAFT}::plan_status,
        ${input.locationCode ?? null}, ${input.note ?? null}, ${input.createdBy ?? null}
      )
      RETURNING
        id,
        mps_number as "mpsNumber",
        plan_date as "planDate",
        item_code as "itemCode",
        planned_quantity as "plannedQuantity",
        due_date as "dueDate",
        status,
        location_code as "locationCode",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }

  async findByNumber(mpsNumber: string): Promise<MPS | null> {
    const result = await this.prisma.$queryRaw<MPS[]>`
      SELECT
        id,
        mps_number as "mpsNumber",
        plan_date as "planDate",
        item_code as "itemCode",
        planned_quantity as "plannedQuantity",
        due_date as "dueDate",
        status,
        location_code as "locationCode",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM master_production_schedules
      WHERE mps_number = ${mpsNumber}
    `
    return result.length > 0 ? result[0] : null
  }

  async findByStatus(status: PlanStatusValue): Promise<MPS[]> {
    const result = await this.prisma.$queryRaw<MPS[]>`
      SELECT
        id,
        mps_number as "mpsNumber",
        plan_date as "planDate",
        item_code as "itemCode",
        planned_quantity as "plannedQuantity",
        due_date as "dueDate",
        status,
        location_code as "locationCode",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM master_production_schedules
      WHERE status = ${status}::plan_status
      ORDER BY due_date, mps_number
    `
    return result
  }

  async updateStatus(id: number, status: PlanStatusValue, updatedBy?: string): Promise<MPS> {
    const result = await this.prisma.$queryRaw<MPS[]>`
      UPDATE master_production_schedules
      SET
        status = ${status}::plan_status,
        updated_by = ${updatedBy ?? null},
        updated_at = CURRENT_TIMESTAMP
      WHERE id = ${id}
      RETURNING
        id,
        mps_number as "mpsNumber",
        plan_date as "planDate",
        item_code as "itemCode",
        planned_quantity as "plannedQuantity",
        due_date as "dueDate",
        status,
        location_code as "locationCode",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }
}
