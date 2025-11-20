import { PrismaClient } from '@prisma/client';
import { IInventoryRepository } from '../../domain/inventory/IInventoryRepository';
import { InventoryLot } from '../../domain/inventory/InventoryLot';

export class PrismaInventoryRepository implements IInventoryRepository {
  constructor(private prisma: PrismaClient) {}

  async findByItemId(itemId: number): Promise<InventoryLot[]> {
    const data = await this.prisma.inventory.findMany({
      where: { itemId },
      orderBy: { expirationDate: 'asc' },
    });

    return data.map((d) =>
      InventoryLot.reconstruct(
        d.id,
        d.itemId,
        d.lotNumber,
        d.arrivalDate,
        d.expirationDate,
        d.quantity,
        d.allocatedQty,
        d.availableQty,
        d.status,
        d.createdBy,
        d.createdAt,
        d.updatedAt
      )
    );
  }

  async findAvailableLots(itemId: number): Promise<InventoryLot[]> {
    const data = await this.prisma.inventory.findMany({
      where: {
        itemId,
        status: 'available',
        availableQty: { gt: 0 },
      },
      orderBy: { expirationDate: 'asc' },
    });

    return data.map((d) =>
      InventoryLot.reconstruct(
        d.id,
        d.itemId,
        d.lotNumber,
        d.arrivalDate,
        d.expirationDate,
        d.quantity,
        d.allocatedQty,
        d.availableQty,
        d.status,
        d.createdBy,
        d.createdAt,
        d.updatedAt
      )
    );
  }

  async save(lot: InventoryLot): Promise<void> {
    await this.prisma.inventory.upsert({
      where: { id: lot.getId() },
      create: {
        id: lot.getId(),
        itemId: lot.getItemId(),
        lotNumber: lot.getLotNumber(),
        arrivalDate: lot.getArrivalDate(),
        expirationDate: lot.getExpirationDate(),
        quantity: lot.getQuantity(),
        allocatedQty: lot.getAllocatedQty(),
        availableQty: lot.getAvailableQuantity(),
        status: lot.getStatus(),
        createdBy: lot.getCreatedBy(),
        createdAt: lot.getCreatedAt(),
        updatedAt: lot.getUpdatedAt(),
      },
      update: {
        allocatedQty: lot.getAllocatedQty(),
        availableQty: lot.getAvailableQuantity(),
        updatedAt: lot.getUpdatedAt(),
      },
    });
  }
}
