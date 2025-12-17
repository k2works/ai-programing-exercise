import { PrismaClient } from '@prisma/client'

// 場所区分
export const LocationType = {
  WAREHOUSE: 'WAREHOUSE', // 倉庫
  FACTORY: 'FACTORY', // 工場
  OFFICE: 'OFFICE', // 事務所
  EXTERNAL: 'EXTERNAL', // 外部倉庫
} as const

export type LocationTypeValue = (typeof LocationType)[keyof typeof LocationType]

// 場所の入力型
export interface CreateLocationInput {
  locationCode: string
  locationName: string
  locationType: LocationTypeValue
  departmentCode?: string
  supplierCode?: string
  postalCode?: string
  address?: string
  phone?: string
}

// 場所の出力型
export interface Location {
  locationCode: string
  locationName: string
  locationType: string
  departmentCode: string | null
  supplierCode: string | null
  postalCode: string | null
  address: string | null
  phone: string | null
  updatedAt: Date
}

export class LocationRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateLocationInput): Promise<Location> {
    const result = await this.prisma.$queryRaw<Location[]>`
      INSERT INTO locations (
        location_code, location_name, location_type,
        department_code, supplier_code, postal_code, address, phone
      )
      VALUES (
        ${input.locationCode}, ${input.locationName}, ${input.locationType}::location_type,
        ${input.departmentCode ?? null}, ${input.supplierCode ?? null},
        ${input.postalCode ?? null}, ${input.address ?? null}, ${input.phone ?? null}
      )
      RETURNING
        location_code as "locationCode",
        location_name as "locationName",
        location_type as "locationType",
        department_code as "departmentCode",
        supplier_code as "supplierCode",
        postal_code as "postalCode",
        address,
        phone,
        updated_at as "updatedAt"
    `
    return result[0]
  }

  async findByCode(locationCode: string): Promise<Location | null> {
    const result = await this.prisma.$queryRaw<Location[]>`
      SELECT
        location_code as "locationCode",
        location_name as "locationName",
        location_type as "locationType",
        department_code as "departmentCode",
        supplier_code as "supplierCode",
        postal_code as "postalCode",
        address,
        phone,
        updated_at as "updatedAt"
      FROM locations
      WHERE location_code = ${locationCode}
    `
    return result.length > 0 ? result[0] : null
  }

  async findByType(locationType: LocationTypeValue): Promise<Location[]> {
    const result = await this.prisma.$queryRaw<Location[]>`
      SELECT
        location_code as "locationCode",
        location_name as "locationName",
        location_type as "locationType",
        department_code as "departmentCode",
        supplier_code as "supplierCode",
        postal_code as "postalCode",
        address,
        phone,
        updated_at as "updatedAt"
      FROM locations
      WHERE location_type = ${locationType}::location_type
      ORDER BY location_code
    `
    return result
  }

  async findByDepartment(departmentCode: string): Promise<Location[]> {
    const result = await this.prisma.$queryRaw<Location[]>`
      SELECT
        location_code as "locationCode",
        location_name as "locationName",
        location_type as "locationType",
        department_code as "departmentCode",
        supplier_code as "supplierCode",
        postal_code as "postalCode",
        address,
        phone,
        updated_at as "updatedAt"
      FROM locations
      WHERE department_code = ${departmentCode}
      ORDER BY location_code
    `
    return result
  }
}
