import { PrismaClient } from '@prisma/client'

export interface CreateLaborHoursInput {
  laborHoursNumber: string
  workOrderNumber: string
  sequence: number
  processCode: string
  workDate: Date
  employeeCode: string
  startTime: Date
  endTime: Date
  workMinutes: number
  note?: string
  createdBy?: string
}

export interface LaborHoursData {
  id: number
  laborHoursNumber: string
  workOrderNumber: string
  sequence: number
  processCode: string
  workDate: Date
  employeeCode: string
  startTime: Date
  endTime: Date
  workMinutes: number
  note: string | null
  createdAt: Date
  createdBy: string | null
  updatedAt: Date
  updatedBy: string | null
}

export class LaborHoursRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateLaborHoursInput): Promise<LaborHoursData> {
    const result = await this.prisma.$queryRaw<LaborHoursData[]>`
      INSERT INTO labor_hours_data (
        labor_hours_number, work_order_number, sequence,
        process_code, work_date, employee_code,
        start_time, end_time, work_minutes, note, created_by
      )
      VALUES (
        ${input.laborHoursNumber}, ${input.workOrderNumber}, ${input.sequence},
        ${input.processCode}, ${input.workDate}, ${input.employeeCode},
        ${input.startTime}, ${input.endTime}, ${input.workMinutes},
        ${input.note ?? null}, ${input.createdBy ?? null}
      )
      RETURNING
        id,
        labor_hours_number as "laborHoursNumber",
        work_order_number as "workOrderNumber",
        sequence,
        process_code as "processCode",
        work_date as "workDate",
        employee_code as "employeeCode",
        start_time as "startTime",
        end_time as "endTime",
        work_minutes as "workMinutes",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
    `
    return result[0]
  }

  async findByNumber(laborHoursNumber: string): Promise<LaborHoursData | null> {
    const result = await this.prisma.$queryRaw<LaborHoursData[]>`
      SELECT
        id,
        labor_hours_number as "laborHoursNumber",
        work_order_number as "workOrderNumber",
        sequence,
        process_code as "processCode",
        work_date as "workDate",
        employee_code as "employeeCode",
        start_time as "startTime",
        end_time as "endTime",
        work_minutes as "workMinutes",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM labor_hours_data
      WHERE labor_hours_number = ${laborHoursNumber}
    `
    return result.length > 0 ? result[0] : null
  }

  async findByWorkOrder(workOrderNumber: string): Promise<LaborHoursData[]> {
    const result = await this.prisma.$queryRaw<LaborHoursData[]>`
      SELECT
        id,
        labor_hours_number as "laborHoursNumber",
        work_order_number as "workOrderNumber",
        sequence,
        process_code as "processCode",
        work_date as "workDate",
        employee_code as "employeeCode",
        start_time as "startTime",
        end_time as "endTime",
        work_minutes as "workMinutes",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM labor_hours_data
      WHERE work_order_number = ${workOrderNumber}
      ORDER BY work_date, sequence
    `
    return result
  }

  async findByEmployee(employeeCode: string, fromDate: Date, toDate: Date): Promise<LaborHoursData[]> {
    const result = await this.prisma.$queryRaw<LaborHoursData[]>`
      SELECT
        id,
        labor_hours_number as "laborHoursNumber",
        work_order_number as "workOrderNumber",
        sequence,
        process_code as "processCode",
        work_date as "workDate",
        employee_code as "employeeCode",
        start_time as "startTime",
        end_time as "endTime",
        work_minutes as "workMinutes",
        note,
        created_at as "createdAt",
        created_by as "createdBy",
        updated_at as "updatedAt",
        updated_by as "updatedBy"
      FROM labor_hours_data
      WHERE employee_code = ${employeeCode}
        AND work_date >= ${fromDate}
        AND work_date <= ${toDate}
      ORDER BY work_date, start_time
    `
    return result
  }

  async calculateTotalHours(workOrderNumber: string, sequence: number): Promise<number> {
    const result = await this.prisma.$queryRaw<{ totalMinutes: number }[]>`
      SELECT
        COALESCE(SUM(work_minutes), 0) as "totalMinutes"
      FROM labor_hours_data
      WHERE work_order_number = ${workOrderNumber}
        AND sequence = ${sequence}
    `

    if (!result || result.length === 0) {
      return 0
    }

    return Number(result[0].totalMinutes)
  }
}
