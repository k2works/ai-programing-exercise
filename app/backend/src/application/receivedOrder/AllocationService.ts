import { PrismaClient } from '@prisma/client';

export class AllocationService {
  constructor(private prisma: PrismaClient) {}

  async allocateInventory(productId: number, quantity: number): Promise<boolean> {
    // Get product composition
    const compositions = await this.prisma.productComposition.findMany({
      where: { productId },
    });

    if (compositions.length === 0) {
      throw new Error('商品構成が登録されていません');
    }

    // Check if sufficient inventory exists for all items
    for (const comp of compositions) {
      const requiredQty = comp.requiredQty * quantity;
      const availableInventory = await this.prisma.inventory.aggregate({
        where: {
          itemId: comp.itemId,
          status: 'available',
        },
        _sum: {
          availableQty: true,
        },
      });

      const available = availableInventory._sum.availableQty || 0;
      if (available < requiredQty) {
        return false;
      }
    }

    // Allocate inventory
    for (const comp of compositions) {
      const requiredQty = comp.requiredQty * quantity;
      await this.allocateItem(comp.itemId, requiredQty);
    }

    return true;
  }

  private async allocateItem(itemId: number, requiredQty: number): Promise<void> {
    let remaining = requiredQty;

    // Get available inventory lots ordered by expiration date (FIFO)
    const inventories = await this.prisma.inventory.findMany({
      where: {
        itemId,
        status: 'available',
        availableQty: { gt: 0 },
      },
      orderBy: {
        expirationDate: 'asc',
      },
    });

    for (const inventory of inventories) {
      if (remaining <= 0) break;

      const allocateQty = Math.min(remaining, inventory.availableQty);

      await this.prisma.inventory.update({
        where: { id: inventory.id },
        data: {
          allocatedQty: inventory.allocatedQty + allocateQty,
          availableQty: inventory.availableQty - allocateQty,
        },
      });

      remaining -= allocateQty;
    }

    if (remaining > 0) {
      throw new Error(`在庫引当に失敗しました: itemId=${itemId}, 不足数=${remaining}`);
    }
  }
}
