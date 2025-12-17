import { PrismaClient } from '@prisma/client'

// 取引先区分
export const SupplierType = {
  CUSTOMER: 'CUSTOMER', // 得意先
  VENDOR: 'VENDOR', // 仕入先
  BOTH: 'BOTH', // 得意先・仕入先両方
} as const

export type SupplierTypeValue = (typeof SupplierType)[keyof typeof SupplierType]

// 取引先の入力型
export interface CreateSupplierInput {
  supplierCode: string
  supplierName: string
  supplierNameKana?: string
  supplierType: SupplierTypeValue
  postalCode?: string
  address?: string
  phone?: string
  fax?: string
  email?: string
}

// 取引先の出力型
export interface Supplier {
  supplierCode: string
  supplierName: string
  supplierNameKana: string | null
  supplierType: string
  postalCode: string | null
  address: string | null
  phone: string | null
  fax: string | null
  email: string | null
  updatedAt: Date
}

export class SupplierRepository {
  constructor(private prisma: PrismaClient) {}

  async create(input: CreateSupplierInput): Promise<Supplier> {
    const result = await this.prisma.$queryRaw<Supplier[]>`
      INSERT INTO suppliers (
        supplier_code, supplier_name, supplier_name_kana, supplier_type,
        postal_code, address, phone, fax, email
      )
      VALUES (
        ${input.supplierCode}, ${input.supplierName}, ${input.supplierNameKana ?? null},
        ${input.supplierType}::supplier_type,
        ${input.postalCode ?? null}, ${input.address ?? null},
        ${input.phone ?? null}, ${input.fax ?? null}, ${input.email ?? null}
      )
      RETURNING
        supplier_code as "supplierCode",
        supplier_name as "supplierName",
        supplier_name_kana as "supplierNameKana",
        supplier_type as "supplierType",
        postal_code as "postalCode",
        address,
        phone,
        fax,
        email,
        updated_at as "updatedAt"
    `
    return result[0]
  }

  async findByCode(supplierCode: string): Promise<Supplier | null> {
    const result = await this.prisma.$queryRaw<Supplier[]>`
      SELECT
        supplier_code as "supplierCode",
        supplier_name as "supplierName",
        supplier_name_kana as "supplierNameKana",
        supplier_type as "supplierType",
        postal_code as "postalCode",
        address,
        phone,
        fax,
        email,
        updated_at as "updatedAt"
      FROM suppliers
      WHERE supplier_code = ${supplierCode}
    `
    return result.length > 0 ? result[0] : null
  }

  async findByType(supplierType: SupplierTypeValue): Promise<Supplier[]> {
    const result = await this.prisma.$queryRaw<Supplier[]>`
      SELECT
        supplier_code as "supplierCode",
        supplier_name as "supplierName",
        supplier_name_kana as "supplierNameKana",
        supplier_type as "supplierType",
        postal_code as "postalCode",
        address,
        phone,
        fax,
        email,
        updated_at as "updatedAt"
      FROM suppliers
      WHERE supplier_type = ${supplierType}::supplier_type
        OR supplier_type = 'BOTH'::supplier_type
      ORDER BY supplier_code
    `
    return result
  }
}
