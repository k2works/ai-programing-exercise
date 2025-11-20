import { PrismaClient } from '@prisma/client';
import { Product } from '../../domain/product/Product';
import { IProductRepository } from '../../domain/product/IProductRepository';

export class PrismaProductRepository implements IProductRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async findById(id: number): Promise<Product | null> {
    const productData = await this.prisma.product.findUnique({
      where: { id },
    });

    if (!productData) {
      return null;
    }

    return Product.reconstruct(
      productData.id,
      productData.code,
      productData.name,
      Number(productData.salesPrice),
      productData.salesStatus,
      productData.createdBy,
      productData.createdAt,
      productData.updatedAt
    );
  }

  async findByCode(code: string): Promise<Product | null> {
    const productData = await this.prisma.product.findUnique({
      where: { code },
    });

    if (!productData) {
      return null;
    }

    return Product.reconstruct(
      productData.id,
      productData.code,
      productData.name,
      Number(productData.salesPrice),
      productData.salesStatus,
      productData.createdBy,
      productData.createdAt,
      productData.updatedAt
    );
  }

  async findOnSale(): Promise<Product[]> {
    const products = await this.prisma.product.findMany({
      where: { salesStatus: 'on_sale' },
    });

    return products.map((p) =>
      Product.reconstruct(
        p.id,
        p.code,
        p.name,
        Number(p.salesPrice),
        p.salesStatus,
        p.createdBy,
        p.createdAt,
        p.updatedAt
      )
    );
  }

  async save(product: Product): Promise<void> {
    const data = product.toJSON();

    await this.prisma.product.upsert({
      where: { id: product.getId() },
      update: {
        name: data.name,
        salesPrice: data.salesPrice,
        salesStatus: data.salesStatus,
        updatedAt: new Date(),
      },
      create: {
        id: data.id,
        code: data.code,
        name: data.name,
        abbreviation: null,
        category: 'BQT',
        salesPrice: data.salesPrice,
        purchasePrice: 0,
        taxCategory: 'TAX10',
        salesStatus: data.salesStatus,
        createdBy: data.createdBy,
      },
    });
  }
}
