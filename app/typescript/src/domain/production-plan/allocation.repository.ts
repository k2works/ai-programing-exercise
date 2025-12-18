import { PrismaClient } from '@prisma/client'

// 引当区分
export const AllocationCategory = {
  INVENTORY: 'INVENTORY', // 在庫
  PURCHASE_REMAIN: 'PURCHASE_REMAIN', // 発注残
  MANUFACTURING_REMAIN: 'MANUFACTURING_REMAIN', // 製造残
} as const

export type AllocationCategoryValue =
  (typeof AllocationCategory)[keyof typeof AllocationCategory]

// 引当情報の入力型
export interface CreateAllocationInput {
  requirementId: number
  allocationCategory: AllocationCategoryValue
  orderId?: number
  allocationDate: Date
  allocatedQuantity: number
  locationCode: string
}

// 引当情報の出力型
export interface Allocation {
  id: number
  requirementId: number
  allocationCategory: string
  orderId: number | null
  allocationDate: Date
  allocatedQuantity: number
  locationCode: string
  createdAt: Date
  updatedAt: Date
}

export class AllocationRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateAllocationInput): Promise<Allocation> {
    const result = await this.prisma.$queryRaw<Allocation[]>`
      INSERT INTO allocations (
        requirement_id, allocation_category, order_id,
        allocation_date, allocated_quantity, location_code
      )
      VALUES (
        ${input.requirementId}, ${input.allocationCategory}::allocation_category,
        ${input.orderId ?? null}, ${input.allocationDate},
        ${input.allocatedQuantity}, ${input.locationCode}
      )
      RETURNING
        id,
        requirement_id as "requirementId",
        allocation_category as "allocationCategory",
        order_id as "orderId",
        allocation_date as "allocationDate",
        allocated_quantity as "allocatedQuantity",
        location_code as "locationCode",
        created_at as "createdAt",
        updated_at as "updatedAt"
    `
    return result[0]
  }

  async findByRequirementId(requirementId: number): Promise<Allocation[]> {
    const result = await this.prisma.$queryRaw<Allocation[]>`
      SELECT
        id,
        requirement_id as "requirementId",
        allocation_category as "allocationCategory",
        order_id as "orderId",
        allocation_date as "allocationDate",
        allocated_quantity as "allocatedQuantity",
        location_code as "locationCode",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM allocations
      WHERE requirement_id = ${requirementId}
      ORDER BY allocation_date
    `
    return result
  }

  async findByCategory(category: AllocationCategoryValue): Promise<Allocation[]> {
    const result = await this.prisma.$queryRaw<Allocation[]>`
      SELECT
        id,
        requirement_id as "requirementId",
        allocation_category as "allocationCategory",
        order_id as "orderId",
        allocation_date as "allocationDate",
        allocated_quantity as "allocatedQuantity",
        location_code as "locationCode",
        created_at as "createdAt",
        updated_at as "updatedAt"
      FROM allocations
      WHERE allocation_category = ${category}::allocation_category
      ORDER BY allocation_date
    `
    return result
  }
}
