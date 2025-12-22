import { Item } from '../../../domain/model/item/Item';

/**
 * 品目登録コマンド
 */
export interface CreateItemCommand {
  品目コード: string;
  品名: string;
  品目区分: string;
  品目グループコード?: string;
  単位コード?: string;
  場所コード?: string;
  リードタイム?: number;
  安全在庫数?: number;
}

/**
 * 品目更新コマンド
 */
export interface UpdateItemCommand {
  品目コード: string;
  品名?: string;
  品目区分?: string;
  リードタイム?: number;
  安全在庫数?: number;
}

/**
 * 品目ユースケース（Input Port）
 */
export interface ItemUseCase {
  /**
   * 品目を登録する
   */
  createItem(command: CreateItemCommand): Promise<Item>;

  /**
   * 品目を更新する
   */
  updateItem(command: UpdateItemCommand): Promise<Item>;

  /**
   * すべての品目を取得する
   */
  getAllItems(): Promise<Item[]>;

  /**
   * 品目コードで品目を取得する
   */
  getItemByCode(itemCode: string): Promise<Item>;

  /**
   * 品目区分で品目を取得する
   */
  getItemsByCategory(category: string): Promise<Item[]>;

  /**
   * 品目を削除する
   */
  deleteItem(itemCode: string): Promise<void>;
}
