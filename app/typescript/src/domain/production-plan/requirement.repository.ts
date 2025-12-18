import { PrismaClient } from '@prisma/client'

// 所要情報の入力型
export interface CreateRequirementInput {
  requirementNumber: string
  orderId: number
  itemCode: string
  dueDate: Date
  requiredQuantity: number
  locationCode: string
}

// 所要情報の出力型
export interface Requirement {
  id: number
  requirementNumber: string
  orderId: number
  itemCode: string
  dueDate: Date
  requiredQuantity: number
  allocatedQuantity: number
  shortageQuantity: number
  locationCode: string
  createdAt: Date
  updatedAt: Date
}

export class RequirementRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateRequirementInput): Promise<Requirement> {
    const result = await this.prisma.$queryRaw<Requirement[]>`
      INSERT INTO requirements (
        requirement_number, order_id, item_code, due_date,
        required_quantity, location_code
      )
      VALUES (
        ${input.requirementNumber}, ${input.orderId}, ${input.itemCode},
        ${input.dueDate}, ${input.requiredQuantity}, ${input.locationCode}
      )
      RETURNING
        id,
        requirement_number as "requirementNumber",
        order_id as "orderId",
        item_code as "itemCode",
        due_date as "dueDate",
        required_quantity as "requiredQuantity",
        allocated_quantity as "allocatedQuantity",
        shortage_quantity as "shortageQuantity",
        location_code as "locationCode",
        created_at as "createdAt",
        updated_at as "updatedAt"
    `
    return result[0]
  }

  async findByNumber(requirementNumber: string): Promise<Requirement | null> {
    const result = await this.prisma.$queryRaw<Requirement[]>`
      SELECT
        id,
        requirement_number as "requirementNumber",
        order_id as "orderId",
        item_code as "itemCode",
        due_date as "dueDate",
        required_quantity as "requiredQuantity",
        allocated_quantity as "allocatedQuantity",
        shortage_quantity as "shortageQuantity",
        location_code as "locationCode",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM requirements
      WHERE requirement_number = ${requirementNumber}
    `
    return result.length > 0 ? result[0] : null
  }

  async findByOrderId(orderId: number): Promise<Requirement[]> {
    const result = await this.prisma.$queryRaw<Requirement[]>`
      SELECT
        id,
        requirement_number as "requirementNumber",
        order_id as "orderId",
        item_code as "itemCode",
        due_date as "dueDate",
        required_quantity as "requiredQuantity",
        allocated_quantity as "allocatedQuantity",
        shortage_quantity as "shortageQuantity",
        location_code as "locationCode",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM requirements
      WHERE order_id = ${orderId}
      ORDER BY due_date
    `
    return result
  }

  async updateAllocated(
    id: number,
    allocatedQuantity: number,
    shortageQuantity: number
  ): Promise<Requirement> {
    const result = await this.prisma.$queryRaw<Requirement[]>`
      UPDATE requirements
      SET
        allocated_quantity = ${allocatedQuantity},
        shortage_quantity = ${shortageQuantity},
        updated_at = CURRENT_TIMESTAMP
      WHERE id = ${id}
      RETURNING
        id,
        requirement_number as "requirementNumber",
        order_id as "orderId",
        item_code as "itemCode",
        due_date as "dueDate",
        required_quantity as "requiredQuantity",
        allocated_quantity as "allocatedQuantity",
        shortage_quantity as "shortageQuantity",
        location_code as "locationCode",
        created_at as "createdAt",
        updated_at as "updatedAt"
    `
    return result[0]
  }
}
