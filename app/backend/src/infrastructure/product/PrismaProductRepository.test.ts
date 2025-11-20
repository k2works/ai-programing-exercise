import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { PrismaProductRepository } from './PrismaProductRepository';
import { Product } from '../../domain/product/Product';
import { setupTestDatabase, teardownTestDatabase, getPrismaClient } from '../../test/prisma-test-helper';

describe('PrismaProductRepository', () => {
  let repository: PrismaProductRepository;

  beforeAll(async () => {
    const prisma = await setupTestDatabase();
    repository = new PrismaProductRepository(prisma);
  }, 60000);

  afterAll(async () => {
    await teardownTestDatabase();
  });

  beforeEach(async () => {
    const prisma = getPrismaClient();
    await prisma.product.deleteMany({
      where: {
        code: {
          startsWith: 'TEST',
        },
      },
    });
  });

  describe('save and findById', () => {
    it('should save and retrieve product', async () => {
      const product = Product.create(1, 'TEST01', 'Test Product', 1000, 'system');
      await repository.save(product);

      const found = await repository.findById(1);
      expect(found).not.toBeNull();
      expect(found?.getCode()).toBe('TEST01');
      expect(found?.getName()).toBe('Test Product');
      expect(found?.getSalesPrice()).toBe(1000);
    });

    it('should return null for non-existent product', async () => {
      const found = await repository.findById(99999);
      expect(found).toBeNull();
    });
  });

  describe('findByCode', () => {
    it('should find product by code', async () => {
      const product = Product.create(2, 'TEST02', 'Test Product 2', 2000, 'system');
      await repository.save(product);

      const found = await repository.findByCode('TEST02');
      expect(found).not.toBeNull();
      expect(found?.getId()).toBe(2);
      expect(found?.getName()).toBe('Test Product 2');
    });

    it('should return null for non-existent code', async () => {
      const found = await repository.findByCode('NOEXST');
      expect(found).toBeNull();
    });
  });

  describe('findOnSale', () => {
    it('should find only on-sale products', async () => {
      const product1 = Product.create(3, 'TEST03', 'On Sale Product', 1000, 'system');
      const product2 = Product.create(4, 'TEST04', 'Stopped Product', 2000, 'system');
      product2.stopSales();

      await repository.save(product1);
      await repository.save(product2);

      const onSaleProducts = await repository.findOnSale();
      const codes = onSaleProducts.map((p) => p.getCode());

      expect(codes).toContain('TEST03');
      expect(codes).not.toContain('TEST04');
    });
  });
});
