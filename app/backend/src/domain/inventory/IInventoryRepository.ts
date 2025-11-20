import { InventoryLot } from './InventoryLot';

export interface IInventoryRepository {
  findByItemId(itemId: number): Promise<InventoryLot[]>;
  findAvailableLots(itemId: number): Promise<InventoryLot[]>;
  save(lot: InventoryLot): Promise<void>;
}
