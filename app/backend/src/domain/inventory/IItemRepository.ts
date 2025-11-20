import { Item } from './Item';

export interface IItemRepository {
  findById(id: number): Promise<Item | null>;
  findByCode(code: string): Promise<Item | null>;
  save(item: Item): Promise<void>;
}
