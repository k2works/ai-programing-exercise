import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { PrismaProductCompositionRepository } from './PrismaProductCompositionRepository';
import { setupTestDatabase, teardownTestDatabase, getPrismaClient } from '../../test/prisma-test-helper';

describe('PrismaProductCompositionRepository', () => {
  let repository: PrismaProductCompositionRepository;

  beforeAll(async () => {
    const prisma = await setupTestDatabase();
    repository = new PrismaProductCompositionRepository(prisma);
  }, 60000);

  afterAll(async () => {
    await teardownTestDatabase();
  });

  beforeEach(async () => {
    const prisma = getPrismaClient();
    await prisma.productComposition.deleteMany({});
    await prisma.product.deleteMany({});
    await prisma.item.deleteMany({});
    await prisma.supplier.deleteMany({});
    
    // Create test supplier
    await prisma.supplier.create({
      data: {
        id: 1,
        code: 'SUP001',
        name: 'Test Supplier',
        status: 'active',
        createdBy: 'test',
      },
    });
    
    // Create test product
    await prisma.product.create({
      data: {
        id: 1,
        code: 'TEST01',
        name: 'Test Product',
        category: 'BQT',
        salesPrice: 1000,
        purchasePrice: 500,
        taxCategory: 'TAX10',
        salesStatus: 'on_sale',
        createdBy: 'test',
      },
    });
    
    // Create test items
    await prisma.item.createMany({
      data: [
        {
          id: 1,
          code: 'ITEM01',
          name: 'Test Item 1',
          supplierId: 1,
          qualityDays: 7,
          leadTime: 2,
          purchaseUnitQty: 10,
          purchasePrice: 100,
          createdBy: 'test',
        },
        {
          id: 2,
          code: 'ITEM02',
          name: 'Test Item 2',
          supplierId: 1,
          qualityDays: 7,
          leadTime: 2,
          purchaseUnitQty: 10,
          purchasePrice: 200,
          createdBy: 'test',
        },
      ],
    });
  });

  describe('saveCompositions', () => {
    it('should save product compositions', async () => {
      const items = [
        { itemId: 1, requiredQty: 3 },
        { itemId: 2, requiredQty: 5 },
      ];

      await repository.saveCompositions(1, items);

      const compositions = await repository.findByProductId(1);
      expect(compositions).toHaveLength(2);
      expect(compositions).toContainEqual({ itemId: 1, requiredQty: 3 });
      expect(compositions).toContainEqual({ itemId: 2, requiredQty: 5 });
    });

    it('should replace existing compositions', async () => {
      await repository.saveCompositions(1, [{ itemId: 1, requiredQty: 3 }]);
      await repository.saveCompositions(1, [{ itemId: 2, requiredQty: 5 }]);

      const compositions = await repository.findByProductId(1);
      expect(compositions).toHaveLength(1);
      expect(compositions[0]).toEqual({ itemId: 2, requiredQty: 5 });
    });

    it('should handle empty items array', async () => {
      await repository.saveCompositions(1, [{ itemId: 1, requiredQty: 3 }]);
      await repository.saveCompositions(1, []);

      const compositions = await repository.findByProductId(1);
      expect(compositions).toHaveLength(0);
    });
  });

  describe('findByProductId', () => {
    it('should return empty array for product with no compositions', async () => {
      const compositions = await repository.findByProductId(999);
      expect(compositions).toHaveLength(0);
    });
  });
});
