import { IProductRepository } from '../../domain/product/IProductRepository';
import { Product } from '../../domain/product/Product';

export class ProductManagementService {
  constructor(private readonly productRepository: IProductRepository) {}

  async registerProduct(
    id: number,
    code: string,
    name: string,
    salesPrice: number,
    createdBy: string
  ): Promise<void> {
    const existing = await this.productRepository.findByCode(code);
    if (existing) {
      throw new Error('商品コードが既に存在します');
    }

    const product = Product.create(id, code, name, salesPrice, createdBy);
    await this.productRepository.save(product);
  }

  async updateProduct(id: number, name: string, salesPrice: number): Promise<void> {
    const product = await this.productRepository.findById(id);
    if (!product) {
      throw new Error('商品が見つかりません');
    }

    const updated = Product.reconstruct(
      product.getId(),
      product.getCode(),
      name,
      salesPrice,
      product.getSalesStatus(),
      product.getCreatedBy(),
      product.getCreatedAt(),
      new Date()
    );

    await this.productRepository.save(updated);
  }

  async stopSales(id: number): Promise<void> {
    const product = await this.productRepository.findById(id);
    if (!product) {
      throw new Error('商品が見つかりません');
    }

    product.stopSales();
    await this.productRepository.save(product);
  }

  async resumeSales(id: number): Promise<void> {
    const product = await this.productRepository.findById(id);
    if (!product) {
      throw new Error('商品が見つかりません');
    }

    product.resumeSales();
    await this.productRepository.save(product);
  }

  async endSales(id: number): Promise<void> {
    const product = await this.productRepository.findById(id);
    if (!product) {
      throw new Error('商品が見つかりません');
    }

    product.endSales();
    await this.productRepository.save(product);
  }
}
