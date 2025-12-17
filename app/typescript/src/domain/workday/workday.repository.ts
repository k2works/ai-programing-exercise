import { PrismaClient } from '@prisma/client'

// 就業区分の型定義
export const WorkdayType = {
  WORKING: 'WORKING',
  HOLIDAY: 'HOLIDAY',
  HALF_DAY: 'HALF_DAY',
} as const

export type WorkdayType = (typeof WorkdayType)[keyof typeof WorkdayType]

// 就業日の入力型
export interface CreateWorkdayInput {
  date: Date
  workdayType: WorkdayType
  shiftCode?: string
  note?: string
}

// 就業日の出力型
export interface Workday {
  date: Date
  workdayType: WorkdayType
  shiftCode: string | null
  note: string | null
  updatedAt: Date
}

export class WorkdayRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateWorkdayInput): Promise<Workday> {
    const result = await this.prisma.$queryRaw<Workday[]>`
      INSERT INTO workdays (date, workday_type, shift_code, note)
      VALUES (${input.date}, ${input.workdayType}::workday_type, ${input.shiftCode ?? null}, ${input.note ?? null})
      RETURNING date, workday_type as "workdayType", shift_code as "shiftCode", note, updated_at as "updatedAt"
    `
    return result[0]
  }

  async bulkCreate(inputs: CreateWorkdayInput[]): Promise<number> {
    const values = inputs.map((input) => {
      return this.prisma.$queryRaw`
        (${input.date}, ${input.workdayType}::workday_type, ${input.shiftCode ?? null}, ${input.note ?? null})
      `
    })

    // バルクインサート用の値を構築
    const insertValues = inputs
      .map(
        (input) =>
          `('${input.date.toISOString().split('T')[0]}', '${input.workdayType}'::workday_type, ${input.shiftCode ? `'${input.shiftCode}'` : 'NULL'}, ${input.note ? `'${input.note}'` : 'NULL'})`
      )
      .join(', ')

    const result = await this.prisma.$executeRawUnsafe(`
      INSERT INTO workdays (date, workday_type, shift_code, note)
      VALUES ${insertValues}
    `)

    return result
  }

  async countWorkingDays(fromDate: Date, toDate: Date): Promise<number> {
    const result = await this.prisma.$queryRaw<[{ count: bigint }]>`
      SELECT COUNT(*) as count
      FROM workdays
      WHERE date >= ${fromDate}
        AND date <= ${toDate}
        AND workday_type = 'WORKING'::workday_type
    `
    return Number(result[0].count)
  }
}
