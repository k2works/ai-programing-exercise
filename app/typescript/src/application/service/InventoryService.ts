import { InventoryUseCase, InventoryQuery } from '../port/in/InventoryUseCase';
import { InventoryRepository } from '../port/out/InventoryRepository';
import { Inventory } from '../../domain/model/inventory/Inventory';

/**
 * 在庫 Application Service
 * Use Caseを実装し、在庫照会のビジネスロジックを調整
 */
export class InventoryService implements InventoryUseCase {
  constructor(private readonly inventoryRepository: InventoryRepository) {}

  async getInventory(locationCode: string, itemCode: string): Promise<Inventory | null> {
    return await this.inventoryRepository.findByLocationAndItem(locationCode, itemCode);
  }

  async getInventoriesByLocation(locationCode: string): Promise<Inventory[]> {
    return await this.inventoryRepository.findByLocation(locationCode);
  }

  async getInventoriesByItem(itemCode: string): Promise<Inventory[]> {
    return await this.inventoryRepository.findByItem(itemCode);
  }

  async queryInventories(query: InventoryQuery): Promise<Inventory[]> {
    if (query.場所コード && query.品目コード) {
      const inventory = await this.inventoryRepository.findByLocationAndItem(
        query.場所コード,
        query.品目コード
      );
      return inventory ? [inventory] : [];
    }

    if (query.場所コード) {
      return await this.inventoryRepository.findByLocation(query.場所コード);
    }

    if (query.品目コード) {
      return await this.inventoryRepository.findByItem(query.品目コード);
    }

    return await this.inventoryRepository.findAll();
  }
}
