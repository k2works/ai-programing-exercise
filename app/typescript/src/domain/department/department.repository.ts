import { PrismaClient } from '@prisma/client'

// 部門の入力型
export interface CreateDepartmentInput {
  departmentCode: string
  effectiveFrom: Date
  effectiveTo?: Date
  parentCode?: string
  level: number
  departmentName: string
  shortName?: string
}

// 部門の出力型
export interface Department {
  departmentCode: string
  effectiveFrom: Date
  effectiveTo: Date | null
  parentCode: string | null
  level: number
  departmentName: string
  shortName: string | null
  updatedAt: Date
}

export class DepartmentRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateDepartmentInput): Promise<Department> {
    const result = await this.prisma.$queryRaw<Department[]>`
      INSERT INTO departments (
        department_code, effective_from, effective_to, parent_code,
        level, department_name, short_name
      )
      VALUES (
        ${input.departmentCode}, ${input.effectiveFrom}, ${input.effectiveTo ?? null},
        ${input.parentCode ?? null}, ${input.level}, ${input.departmentName},
        ${input.shortName ?? null}
      )
      RETURNING
        department_code as "departmentCode",
        effective_from as "effectiveFrom",
        effective_to as "effectiveTo",
        parent_code as "parentCode",
        level,
        department_name as "departmentName",
        short_name as "shortName",
        updated_at as "updatedAt"
    `
    return result[0]
  }
}
