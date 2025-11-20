import { PrismaClient } from '@prisma/client';
import { ISupplierRepository } from '../../domain/inventory/ISupplierRepository';
import { Supplier } from '../../domain/inventory/Supplier';

export class PrismaSupplierRepository implements ISupplierRepository {
  constructor(private prisma: PrismaClient) {}

  async findById(id: number): Promise<Supplier | null> {
    const data = await this.prisma.supplier.findUnique({ where: { id } });
    if (!data) return null;

    return Supplier.reconstruct(
      data.id,
      data.code,
      data.name,
      data.phone,
      data.email,
      data.status,
      data.createdBy,
      data.createdAt,
      data.updatedAt
    );
  }

  async findByCode(code: string): Promise<Supplier | null> {
    const data = await this.prisma.supplier.findUnique({ where: { code } });
    if (!data) return null;

    return Supplier.reconstruct(
      data.id,
      data.code,
      data.name,
      data.phone,
      data.email,
      data.status,
      data.createdBy,
      data.createdAt,
      data.updatedAt
    );
  }

  async save(supplier: Supplier): Promise<void> {
    await this.prisma.supplier.upsert({
      where: { id: supplier.getId() },
      create: {
        id: supplier.getId(),
        code: supplier.getCode(),
        name: supplier.getName(),
        phone: supplier.getPhone(),
        email: supplier.getEmail(),
        status: supplier.getStatus(),
        createdBy: supplier.getCreatedBy(),
        createdAt: supplier.getCreatedAt(),
        updatedAt: supplier.getUpdatedAt(),
      },
      update: {
        status: supplier.getStatus(),
        updatedAt: supplier.getUpdatedAt(),
      },
    });
  }
}
