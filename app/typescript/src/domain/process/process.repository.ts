import { PrismaClient } from '@prisma/client'

export interface CreateProcessMasterInput {
  processCode: string
  processName: string
  standardCycleTime?: number
  setupTime?: number
  createdBy?: string
}

export interface ProcessMasterData {
  processCode: string
  processName: string
  standardCycleTime: number | null
  setupTime: number | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export interface CreateRoutingInput {
  itemCode: string
  effectiveFrom: Date
  sequence: number
  processCode: string
  effectiveTo?: Date
  workTime?: number
  createdBy?: string
}

export interface RoutingData {
  itemCode: string
  effectiveFrom: Date
  sequence: number
  processCode: string
  effectiveTo: Date | null
  workTime: number | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export class ProcessMasterRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateProcessMasterInput): Promise<ProcessMasterData> {
    const result = await this.prisma.$queryRaw<ProcessMasterData[]>`
      INSERT INTO process_masters (
        process_code, process_name, standard_cycle_time,
        setup_time, created_by
      )
      VALUES (
        ${input.processCode}, ${input.processName}, ${input.standardCycleTime ?? null},
        ${input.setupTime ?? null}, ${input.createdBy ?? null}
      )
      RETURNING
        process_code as "processCode",
        process_name as "processName",
        standard_cycle_time as "standardCycleTime",
        setup_time as "setupTime",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }

  async findByCode(processCode: string): Promise<ProcessMasterData | null> {
    const result = await this.prisma.$queryRaw<ProcessMasterData[]>`
      SELECT
        process_code as "processCode",
        process_name as "processName",
        standard_cycle_time as "standardCycleTime",
        setup_time as "setupTime",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM process_masters
      WHERE process_code = ${processCode}
    `
    return result.length > 0 ? result[0] : null
  }

  async findAll(): Promise<ProcessMasterData[]> {
    const result = await this.prisma.$queryRaw<ProcessMasterData[]>`
      SELECT
        process_code as "processCode",
        process_name as "processName",
        standard_cycle_time as "standardCycleTime",
        setup_time as "setupTime",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM process_masters
      ORDER BY process_code
    `
    return result
  }
}

export class RoutingRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateRoutingInput): Promise<RoutingData> {
    const result = await this.prisma.$queryRaw<RoutingData[]>`
      INSERT INTO routings (
        item_code, effective_from, sequence, process_code,
        effective_to, work_time, created_by
      )
      VALUES (
        ${input.itemCode}, ${input.effectiveFrom}, ${input.sequence},
        ${input.processCode}, ${input.effectiveTo ?? null},
        ${input.workTime ?? null}, ${input.createdBy ?? null}
      )
      RETURNING
        item_code as "itemCode",
        effective_from as "effectiveFrom",
        sequence,
        process_code as "processCode",
        effective_to as "effectiveTo",
        work_time as "workTime",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }

  async findByItem(itemCode: string, effectiveDate?: Date): Promise<RoutingData[]> {
    const targetDate = effectiveDate ?? new Date()

    const result = await this.prisma.$queryRaw<RoutingData[]>`
      SELECT
        item_code as "itemCode",
        effective_from as "effectiveFrom",
        sequence,
        process_code as "processCode",
        effective_to as "effectiveTo",
        work_time as "workTime",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM routings
      WHERE item_code = ${itemCode}
        AND effective_from <= ${targetDate}
        AND (effective_to IS NULL OR effective_to >= ${targetDate})
      ORDER BY sequence
    `
    return result
  }

  async findByItemAndDate(itemCode: string, effectiveFrom: Date): Promise<RoutingData[]> {
    const result = await this.prisma.$queryRaw<RoutingData[]>`
      SELECT
        item_code as "itemCode",
        effective_from as "effectiveFrom",
        sequence,
        process_code as "processCode",
        effective_to as "effectiveTo",
        work_time as "workTime",
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM routings
      WHERE item_code = ${itemCode}
        AND effective_from = ${effectiveFrom}
      ORDER BY sequence
    `
    return result
  }
}
