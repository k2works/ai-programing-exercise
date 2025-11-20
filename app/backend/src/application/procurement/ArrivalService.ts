import { IArrivalRepository } from '../../domain/procurement/IArrivalRepository';
import { IPlacementOrderRepository } from '../../domain/procurement/IPlacementOrderRepository';
import { IInventoryRepository } from '../../domain/inventory/IInventoryRepository';
import { IItemRepository } from '../../domain/inventory/IItemRepository';
import { Arrival, ArrivalLine } from '../../domain/procurement/Arrival';
import { InventoryLot } from '../../domain/inventory/InventoryLot';

export class ArrivalService {
  constructor(
    private arrivalRepository: IArrivalRepository,
    private placementOrderRepository: IPlacementOrderRepository,
    private inventoryRepository: IInventoryRepository,
    private itemRepository: IItemRepository
  ) {}

  async receiveItems(
    id: number,
    placementOrderId: number,
    arrivalDate: Date,
    lines: Array<{ placementOrderLineId: number; itemId: number; arrivedQty: number }>,
    createdBy: string
  ): Promise<Arrival> {
    // Verify placement order exists
    const placementOrder = await this.placementOrderRepository.findById(placementOrderId);
    if (!placementOrder) {
      throw new Error('発注が見つかりません');
    }

    // Create arrival lines
    const arrivalLines = await Promise.all(
      lines.map(async (line) =>
        new ArrivalLine(
          await this.getNextLineId(),
          line.placementOrderLineId,
          line.itemId,
          line.arrivedQty
        )
      )
    );

    const arrival = Arrival.create(id, placementOrderId, arrivalDate, arrivalLines, createdBy);
    await this.arrivalRepository.save(arrival);

    return arrival;
  }

  async inspectItems(
    arrivalId: number,
    lineInspections: Array<{ lineId: number; result: 'accepted' | 'rejected' }>
  ): Promise<void> {
    const arrival = await this.arrivalRepository.findById(arrivalId);
    if (!arrival) {
      throw new Error('入荷が見つかりません');
    }

    // Inspect each line
    for (const inspection of lineInspections) {
      const line = arrival.getLines().find((l) => l.id === inspection.lineId);
      if (line) {
        line.inspect(inspection.result);
      }
    }

    // Complete inspection
    arrival.inspect();

    // If inspected (not returned), add to inventory
    if (arrival.getInspectionStatus() === 'inspected') {
      await this.addToInventory(arrival);
    }

    await this.arrivalRepository.save(arrival);
  }

  private async addToInventory(arrival: Arrival): Promise<void> {
    for (const line of arrival.getLines()) {
      if (line.inspectionResult === 'accepted') {
        const item = await this.itemRepository.findById(line.itemId);
        if (!item) continue;

        // Calculate expiration date
        const expirationDate = new Date(arrival.getArrivalDate());
        expirationDate.setDate(expirationDate.getDate() + item.getQualityDays());

        // Create inventory lot
        const lot = InventoryLot.create(
          await this.getNextLotId(),
          line.itemId,
          `LOT-${arrival.getId()}-${line.id}`,
          arrival.getArrivalDate(),
          expirationDate,
          line.arrivedQty,
          arrival.getCreatedBy()
        );

        await this.inventoryRepository.save(lot);
      }
    }
  }

  private async getNextLineId(): Promise<number> {
    return Math.floor(Math.random() * 1000000);
  }

  private async getNextLotId(): Promise<number> {
    return Math.floor(Math.random() * 1000000);
  }
}
