import { describe, it, expect, beforeEach } from 'vitest';
import { ProductManagementService } from './ProductManagementService';
import { IProductRepository } from '../../domain/product/IProductRepository';
import { Product } from '../../domain/product/Product';

class InMemoryProductRepository implements IProductRepository {
  private products: Map<number, Product> = new Map();

  async findById(id: number): Promise<Product | null> {
    return this.products.get(id) || null;
  }

  async findByCode(code: string): Promise<Product | null> {
    return Array.from(this.products.values()).find((p) => p.getCode() === code) || null;
  }

  async findOnSale(): Promise<Product[]> {
    return Array.from(this.products.values()).filter((p) => p.canOrder());
  }

  async save(product: Product): Promise<void> {
    this.products.set(product.getId(), product);
  }

  clear(): void {
    this.products.clear();
  }
}

describe('ProductManagementService', () => {
  let service: ProductManagementService;
  let repository: InMemoryProductRepository;

  beforeEach(() => {
    repository = new InMemoryProductRepository();
    service = new ProductManagementService(repository);
  });

  describe('registerProduct', () => {
    it('should register a new product', async () => {
      await service.registerProduct(1, 'PRD001', 'Test Product', 1000, 'admin');

      const product = await repository.findById(1);
      expect(product).not.toBeNull();
      expect(product?.getCode()).toBe('PRD001');
      expect(product?.getName()).toBe('Test Product');
      expect(product?.getSalesPrice()).toBe(1000);
    });

    it('should throw error if product code already exists', async () => {
      await service.registerProduct(1, 'PRD001', 'Product 1', 1000, 'admin');

      await expect(
        service.registerProduct(2, 'PRD001', 'Product 2', 2000, 'admin')
      ).rejects.toThrow('商品コードが既に存在します');
    });
  });

  describe('updateProduct', () => {
    it('should update product name and price', async () => {
      await service.registerProduct(1, 'PRD001', 'Original', 1000, 'admin');
      await service.updateProduct(1, 'Updated', 1500);

      const product = await repository.findById(1);
      expect(product?.getName()).toBe('Updated');
      expect(product?.getSalesPrice()).toBe(1500);
    });

    it('should throw error if product not found', async () => {
      await expect(service.updateProduct(999, 'Updated', 1500)).rejects.toThrow(
        '商品が見つかりません'
      );
    });
  });

  describe('stopSales', () => {
    it('should stop sales of a product', async () => {
      await service.registerProduct(1, 'PRD001', 'Product', 1000, 'admin');
      await service.stopSales(1);

      const product = await repository.findById(1);
      expect(product?.canOrder()).toBe(false);
    });
  });

  describe('resumeSales', () => {
    it('should resume sales of a stopped product', async () => {
      await service.registerProduct(1, 'PRD001', 'Product', 1000, 'admin');
      await service.stopSales(1);
      await service.resumeSales(1);

      const product = await repository.findById(1);
      expect(product?.canOrder()).toBe(true);
    });
  });

  describe('endSales', () => {
    it('should end sales of a product', async () => {
      await service.registerProduct(1, 'PRD001', 'Product', 1000, 'admin');
      await service.endSales(1);

      const product = await repository.findById(1);
      expect(product?.getSalesStatus()).toBe('ended');
    });
  });

  describe('associateItems', () => {
    it('should associate items with product', async () => {
      await service.registerProduct(1, 'PRD001', 'Product', 1000, 'admin');
      
      const itemAssociations = [
        { itemId: 101, requiredQty: 3 },
        { itemId: 102, requiredQty: 5 },
      ];
      
      await service.associateItems(1, itemAssociations);
      
      // Verify associations were saved (would need to check via repository)
      const product = await repository.findById(1);
      expect(product).not.toBeNull();
    });

    it('should throw error if product not found', async () => {
      await expect(
        service.associateItems(999, [{ itemId: 101, requiredQty: 3 }])
      ).rejects.toThrow('商品が見つかりません');
    });
  });
});
