import { PrismaClient } from '@prisma/client';
import { IReceivedOrderRepository } from '../../domain/receivedOrder/IReceivedOrderRepository';
import { IInventoryRepository } from '../../domain/inventory/IInventoryRepository';
import { IProductCompositionRepository } from '../../domain/product/IProductCompositionRepository';

interface PickingListItem {
  itemId: number;
  itemName: string;
  requiredQuantity: number;
  lotNumber: string;
}

interface PickingList {
  receivedOrderId: number;
  items: PickingListItem[];
}

export class PickingService {
  constructor(
    private receivedOrderRepo: IReceivedOrderRepository,
    private inventoryRepo: IInventoryRepository,
    private compositionRepo: IProductCompositionRepository,
    private prisma: PrismaClient
  ) {}

  async generatePickingList(receivedOrderId: number): Promise<PickingList> {
    const receivedOrder = await this.receivedOrderRepo.findById(
      receivedOrderId
    );
    if (!receivedOrder) {
      throw new Error('受注が見つかりません');
    }

    // Get order with product info
    const order = await this.prisma.order.findUnique({
      where: { id: receivedOrder.getOrderId() },
    });

    if (!order) {
      throw new Error('注文が見つかりません');
    }

    // Get product compositions
    const compositions = await this.compositionRepo.findByProductId(
      order.productId
    );

    const items: PickingListItem[] = [];

    for (const comp of compositions) {
      const requiredQty = comp.requiredQty * order.quantity;

      // Get inventory for this item
      const inventories = await this.inventoryRepo.findByItemId(comp.itemId);

      if (inventories.length === 0) {
        throw new Error(`在庫が不足しています: Item ${comp.itemId}`);
      }

      // Get item name
      const item = await this.prisma.item.findUnique({
        where: { id: comp.itemId },
      });

      items.push({
        itemId: comp.itemId,
        itemName: item?.name || '',
        requiredQuantity: requiredQty,
        lotNumber: inventories[0].getLotNumber(),
      });
    }

    return {
      receivedOrderId,
      items,
    };
  }

  async confirmPicking(
    receivedOrderId: number,
    confirmedBy: string
  ): Promise<void> {
    const pickingList = await this.generatePickingList(receivedOrderId);

    for (const item of pickingList.items) {
      const inventories = await this.inventoryRepo.findByItemId(item.itemId);

      if (inventories.length === 0) {
        throw new Error(`在庫が見つかりません: Item ${item.itemId}`);
      }

      const inventory = inventories[0];
      inventory.allocate(item.requiredQuantity);
      await this.inventoryRepo.save(inventory);
    }
  }
}
