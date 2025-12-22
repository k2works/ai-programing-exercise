import { ItemUseCase, CreateItemCommand, UpdateItemCommand } from '../port/in/ItemUseCase';
import { ItemRepository } from '../port/out/ItemRepository';
import { Item } from '../../domain/model/item/Item';
import { ItemNotFoundException, DuplicateItemException } from '../../domain/exception/DomainException';

/**
 * 品目アプリケーションサービス
 */
export class ItemService implements ItemUseCase {
  constructor(private readonly itemRepository: ItemRepository) {}

  async createItem(command: CreateItemCommand): Promise<Item> {
    // 重複チェック
    const existing = await this.itemRepository.findByCode(command.品目コード);
    if (existing) {
      throw new DuplicateItemException(command.品目コード);
    }

    const item = Item.create({
      品目コード: command.品目コード,
      適用開始日: new Date(),
      品名: command.品名,
      品目区分: command.品目区分,
      品目グループコード: command.品目グループコード,
      単位コード: command.単位コード,
      場所コード: command.場所コード,
      リードタイム: command.リードタイム,
      安全在庫数: command.安全在庫数,
    });

    return await this.itemRepository.save(item);
  }

  async updateItem(command: UpdateItemCommand): Promise<Item> {
    const item = await this.itemRepository.findByCode(command.品目コード);
    if (!item) {
      throw new ItemNotFoundException(command.品目コード);
    }

    // 更新処理
    if (command.品名) item.品名 = command.品名;
    if (command.品目区分) item.品目区分 = command.品目区分;
    if (command.リードタイム !== undefined) item.リードタイム = command.リードタイム;
    if (command.安全在庫数 !== undefined) item.安全在庫数 = command.安全在庫数;

    return await this.itemRepository.save(item);
  }

  async getAllItems(): Promise<Item[]> {
    return await this.itemRepository.findAll();
  }

  async getItemByCode(itemCode: string): Promise<Item> {
    const item = await this.itemRepository.findByCode(itemCode);
    if (!item) {
      throw new ItemNotFoundException(itemCode);
    }
    return item;
  }

  async getItemsByCategory(category: string): Promise<Item[]> {
    return await this.itemRepository.findByCategory(category);
  }

  async deleteItem(itemCode: string): Promise<void> {
    const item = await this.itemRepository.findByCode(itemCode);
    if (!item) {
      throw new ItemNotFoundException(itemCode);
    }
    await this.itemRepository.deleteByCode(itemCode);
  }
}
