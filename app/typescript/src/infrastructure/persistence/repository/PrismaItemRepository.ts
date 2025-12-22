import { PrismaClient } from '@prisma/client';
import { ItemRepository } from '../../../application/port/out/ItemRepository';
import { Item } from '../../../domain/model/item/Item';

/**
 * Prisma を使用した品目リポジトリ実装
 */
export class PrismaItemRepository implements ItemRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async save(item: Item): Promise<Item> {
    const data = {
      itemCode: item.品目コード,
      effectiveFrom: item.適用開始日,
      itemName: item.品名,
      itemCategory: item.品目区分,
      unitCode: item.単位コード,
      leadTime: item.リードタイム ?? 0,
      safetyStock: item.安全在庫数 ?? 0,
    };

    const result = await this.prisma.item.upsert({
      where: {
        itemCode: item.品目コード,
      },
      update: data,
      create: data,
    });

    return this.toItem(result);
  }

  async findAll(): Promise<Item[]> {
    const results = await this.prisma.item.findMany({
      orderBy: { itemCode: 'asc' },
    });
    return results.map(this.toItem);
  }

  async findByCode(itemCode: string): Promise<Item | null> {
    const result = await this.prisma.item.findFirst({
      where: { itemCode: itemCode },
      orderBy: { effectiveFrom: 'desc' },
    });
    return result ? this.toItem(result) : null;
  }

  async findByCategory(category: string): Promise<Item[]> {
    const results = await this.prisma.item.findMany({
      where: { itemCategory: category },
      orderBy: { itemCode: 'asc' },
    });
    return results.map(this.toItem);
  }

  async deleteByCode(itemCode: string): Promise<void> {
    await this.prisma.item.deleteMany({
      where: { itemCode: itemCode },
    });
  }

  private toItem(data: any): Item {
    return Item.create({
      品目コード: data.itemCode,
      適用開始日: data.effectiveFrom,
      品名: data.itemName,
      品目区分: data.itemCategory,
      品目グループコード: data.itemGroupCode ?? undefined,
      単位コード: data.unitCode ?? undefined,
      場所コード: data.locationCode ?? undefined,
      リードタイム: data.leadTime ?? undefined,
      安全在庫数: data.safetyStockQuantity ?? undefined,
    });
  }
}
