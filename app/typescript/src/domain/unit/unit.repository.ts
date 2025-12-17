import { PrismaClient } from '@prisma/client'

// 単位の入力型
export interface CreateUnitInput {
  unitCode: string
  unitSymbol: string
  unitName: string
}

// 単位の出力型
export interface Unit {
  unitCode: string
  unitSymbol: string
  unitName: string
}

export class UnitRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateUnitInput): Promise<Unit> {
    const result = await this.prisma.$queryRaw<Unit[]>`
      INSERT INTO units (unit_code, unit_symbol, unit_name)
      VALUES (${input.unitCode}, ${input.unitSymbol}, ${input.unitName})
      RETURNING unit_code as "unitCode", unit_symbol as "unitSymbol", unit_name as "unitName"
    `
    return result[0]
  }
}
