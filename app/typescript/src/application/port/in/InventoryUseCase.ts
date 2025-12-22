import { Inventory } from '../../../domain/model/inventory/Inventory';

/**
 * 在庫照会クエリパラメータ
 */
export interface InventoryQuery {
  場所コード?: string;
  品目コード?: string;
}

/**
 * 在庫ユースケース (Input Port)
 */
export interface InventoryUseCase {
  /**
   * 場所・品目別の在庫を照会
   */
  getInventory(locationCode: string, itemCode: string): Promise<Inventory | null>;

  /**
   * 場所別の在庫一覧を照会
   */
  getInventoriesByLocation(locationCode: string): Promise<Inventory[]>;

  /**
   * 品目別の在庫一覧を照会（全場所）
   */
  getInventoriesByItem(itemCode: string): Promise<Inventory[]>;

  /**
   * 在庫一覧を照会（クエリパラメータで絞り込み）
   */
  queryInventories(query: InventoryQuery): Promise<Inventory[]>;
}
