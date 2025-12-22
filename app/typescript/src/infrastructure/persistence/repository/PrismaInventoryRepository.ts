import { PrismaClient } from '@prisma/client';
import { InventoryRepository } from '../../../application/port/out/InventoryRepository';
import { Inventory } from '../../../domain/model/inventory/Inventory';

/**
 * Prismaを使用した在庫リポジトリ実装
 */
export class PrismaInventoryRepository implements InventoryRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async findByLocationAndItem(
    locationCode: string,
    itemCode: string
  ): Promise<Inventory | null> {
    const result = await this.prisma.inventory.findUnique({
      where: {
        locationCode_itemCode: {
          locationCode,
          itemCode,
        },
      },
    });

    if (!result) {
      return null;
    }

    return this.toInventory(result);
  }

  async findByLocation(locationCode: string): Promise<Inventory[]> {
    const results = await this.prisma.inventory.findMany({
      where: {
        locationCode,
      },
      orderBy: {
        itemCode: 'asc',
      },
    });

    return results.map((r) => this.toInventory(r));
  }

  async findByItem(itemCode: string): Promise<Inventory[]> {
    const results = await this.prisma.inventory.findMany({
      where: {
        itemCode,
      },
      orderBy: {
        locationCode: 'asc',
      },
    });

    return results.map((r) => this.toInventory(r));
  }

  async findAll(): Promise<Inventory[]> {
    const results = await this.prisma.inventory.findMany({
      orderBy: [
        { locationCode: 'asc' },
        { itemCode: 'asc' },
      ],
    });

    return results.map((r) => this.toInventory(r));
  }

  private toInventory(data: any): Inventory {
    return Inventory.create({
      場所コード: data.locationCode,
      品目コード: data.itemCode,
      在庫数量: Number(data.stockQuantity),
      合格数: Number(data.acceptedQuantity),
      不良数: Number(data.defectQuantity),
      未検査数: Number(data.uninspectedQuantity),
      更新日時: data.updatedAt,
    });
  }
}
