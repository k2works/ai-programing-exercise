import { Inventory } from '../../../domain/model/inventory/Inventory';

/**
 * 在庫リポジトリインターフェース (Output Port)
 */
export interface InventoryRepository {
  /**
   * 場所・品目別の在庫を照会
   */
  findByLocationAndItem(locationCode: string, itemCode: string): Promise<Inventory | null>;

  /**
   * 場所別の在庫一覧を照会
   */
  findByLocation(locationCode: string): Promise<Inventory[]>;

  /**
   * 品目別の在庫一覧を照会（全場所）
   */
  findByItem(itemCode: string): Promise<Inventory[]>;

  /**
   * 全在庫一覧を照会
   */
  findAll(): Promise<Inventory[]>;
}
