import { PrismaClient } from '@prisma/client'

// 担当者の入力型
export interface CreateEmployeeInput {
  employeeCode: string
  effectiveFrom: Date
  effectiveTo?: Date
  employeeName: string
  employeeNameKana?: string
  departmentCode?: string
  email?: string
}

// 担当者の出力型
export interface Employee {
  employeeCode: string
  effectiveFrom: Date
  effectiveTo: Date | null
  employeeName: string
  employeeNameKana: string | null
  departmentCode: string | null
  email: string | null
  updatedAt: Date
}

export class EmployeeRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateEmployeeInput): Promise<Employee> {
    const result = await this.prisma.$queryRaw<Employee[]>`
      INSERT INTO employees (
        employee_code, effective_from, effective_to, employee_name,
        employee_name_kana, department_code, email
      )
      VALUES (
        ${input.employeeCode}, ${input.effectiveFrom}, ${input.effectiveTo ?? null},
        ${input.employeeName}, ${input.employeeNameKana ?? null},
        ${input.departmentCode ?? null}, ${input.email ?? null}
      )
      RETURNING
        employee_code as "employeeCode",
        effective_from as "effectiveFrom",
        effective_to as "effectiveTo",
        employee_name as "employeeName",
        employee_name_kana as "employeeNameKana",
        department_code as "departmentCode",
        email,
        updated_at as "updatedAt"
    `
    return result[0]
  }

  async findByCodeAndDate(employeeCode: string, date: Date): Promise<Employee | null> {
    const result = await this.prisma.$queryRaw<Employee[]>`
      SELECT
        employee_code as "employeeCode",
        effective_from as "effectiveFrom",
        effective_to as "effectiveTo",
        employee_name as "employeeName",
        employee_name_kana as "employeeNameKana",
        department_code as "departmentCode",
        email,
        updated_at as "updatedAt"
      FROM employees
      WHERE employee_code = ${employeeCode}
        AND effective_from <= ${date}
        AND (effective_to IS NULL OR effective_to >= ${date})
      ORDER BY effective_from DESC
      LIMIT 1
    `
    return result.length > 0 ? result[0] : null
  }
}
