import { Item } from '../../../domain/model/item/Item';

/**
 * 品目リポジトリ（Output Port）
 * ドメイン層がデータアクセスに依存しないためのインターフェース
 */
export interface ItemRepository {
  /**
   * 品目を保存する
   */
  save(item: Item): Promise<Item>;

  /**
   * すべての品目を取得する
   */
  findAll(): Promise<Item[]>;

  /**
   * 品目コードで品目を検索する
   */
  findByCode(itemCode: string): Promise<Item | null>;

  /**
   * 品目区分で品目を取得する
   */
  findByCategory(category: string): Promise<Item[]>;

  /**
   * 品目を削除する
   */
  deleteByCode(itemCode: string): Promise<void>;
}
