import { PrismaClient } from '@prisma/client';
import { IItemRepository } from '../../domain/inventory/IItemRepository';
import { Item } from '../../domain/inventory/Item';

export class PrismaItemRepository implements IItemRepository {
  constructor(private prisma: PrismaClient) {}

  async findById(id: number): Promise<Item | null> {
    const data = await this.prisma.item.findUnique({ where: { id } });
    if (!data) return null;

    return Item.reconstruct(
      data.id,
      data.code,
      data.name,
      data.supplierId,
      data.qualityDays,
      data.leadTime,
      data.purchaseUnitQty,
      Number(data.purchasePrice),
      data.createdBy,
      data.createdAt,
      data.updatedAt
    );
  }

  async findByCode(code: string): Promise<Item | null> {
    const data = await this.prisma.item.findUnique({ where: { code } });
    if (!data) return null;

    return Item.reconstruct(
      data.id,
      data.code,
      data.name,
      data.supplierId,
      data.qualityDays,
      data.leadTime,
      data.purchaseUnitQty,
      Number(data.purchasePrice),
      data.createdBy,
      data.createdAt,
      data.updatedAt
    );
  }

  async save(item: Item): Promise<void> {
    await this.prisma.item.upsert({
      where: { id: item.getId() },
      create: {
        id: item.getId(),
        code: item.getCode(),
        name: item.getName(),
        supplierId: item.getSupplierId(),
        qualityDays: item.getQualityDays(),
        leadTime: item.getLeadTime(),
        purchaseUnitQty: item.getPurchaseUnitQty(),
        purchasePrice: item.getPurchasePrice(),
        createdBy: item.getCreatedBy(),
        createdAt: item.getCreatedAt(),
        updatedAt: item.getUpdatedAt(),
      },
      update: {
        updatedAt: item.getUpdatedAt(),
      },
    });
  }
}
