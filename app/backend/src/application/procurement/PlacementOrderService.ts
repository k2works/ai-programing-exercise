import { IPlacementOrderRepository } from '../../domain/procurement/IPlacementOrderRepository';
import { IItemRepository } from '../../domain/inventory/IItemRepository';
import { PlacementOrder, PlacementOrderLine } from '../../domain/procurement/PlacementOrder';

export class PlacementOrderService {
  constructor(
    private placementOrderRepository: IPlacementOrderRepository,
    private itemRepository: IItemRepository
  ) {}

  async createPlacementOrder(
    id: number,
    orderDate: Date,
    supplierId: number,
    desiredDeliveryDate: Date,
    lines: Array<{ itemId: number; orderQty: number }>,
    createdBy: string
  ): Promise<PlacementOrder> {
    // Get first item to determine lead time
    const firstItem = await this.itemRepository.findById(lines[0].itemId);
    if (!firstItem) {
      throw new Error('単品が見つかりません');
    }

    // Create order lines with purchase prices
    const orderLines: PlacementOrderLine[] = [];
    for (const line of lines) {
      const item = await this.itemRepository.findById(line.itemId);
      if (!item) {
        throw new Error(`単品が見つかりません: ${line.itemId}`);
      }

      // Validate purchase unit quantity
      if (line.orderQty % item.getPurchaseUnitQty() !== 0) {
        throw new Error(
          `発注数量は購入単位数量(${item.getPurchaseUnitQty()})の倍数である必要があります`
        );
      }

      orderLines.push(
        new PlacementOrderLine(
          await this.getNextLineId(),
          line.itemId,
          line.orderQty,
          item.getPurchasePrice()
        )
      );
    }

    const order = PlacementOrder.create(
      id,
      orderDate,
      supplierId,
      desiredDeliveryDate,
      orderLines,
      firstItem.getLeadTime(),
      createdBy
    );

    await this.placementOrderRepository.save(order);
    return order;
  }

  async cancelPlacementOrder(id: number): Promise<void> {
    const order = await this.placementOrderRepository.findById(id);
    if (!order) {
      throw new Error('発注が見つかりません');
    }

    order.cancel();
    await this.placementOrderRepository.save(order);
  }

  private async getNextLineId(): Promise<number> {
    return Math.floor(Math.random() * 1000000);
  }
}
