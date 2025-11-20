import { PrismaClient } from '@prisma/client';
import { IProductCompositionRepository } from '../../domain/product/IProductCompositionRepository';

export class PrismaProductCompositionRepository implements IProductCompositionRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async saveCompositions(
    productId: number,
    items: Array<{ itemId: number; requiredQty: number }>
  ): Promise<void> {
    // Delete existing compositions
    await this.prisma.productComposition.deleteMany({
      where: { productId },
    });

    // Create new compositions
    if (items.length > 0) {
      await this.prisma.productComposition.createMany({
        data: items.map((item) => ({
          productId,
          itemId: item.itemId,
          requiredQty: item.requiredQty,
        })),
      });
    }
  }

  async findByProductId(productId: number): Promise<Array<{ itemId: number; requiredQty: number }>> {
    const compositions = await this.prisma.productComposition.findMany({
      where: { productId },
    });

    return compositions.map((c) => ({
      itemId: c.itemId,
      requiredQty: c.requiredQty,
    }));
  }
}
