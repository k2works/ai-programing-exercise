import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { PrismaReturnRepository } from './PrismaReturnRepository';
import { Return } from '../../domain/shipment/Return';
import { setupTestDatabase, teardownTestDatabase, getPrismaClient } from '../../test/prisma-test-helper';

describe('PrismaReturnRepository', () => {
  let repository: PrismaReturnRepository;

  beforeAll(async () => {
    const prisma = await setupTestDatabase();
    repository = new PrismaReturnRepository(prisma);
  }, 60000);

  afterAll(async () => {
    await teardownTestDatabase();
  });

  beforeEach(async () => {
    const prisma = getPrismaClient();
    await prisma.return.deleteMany({});
    await prisma.order.deleteMany({});
    await prisma.product.deleteMany({});
    await prisma.customer.deleteMany({});

    await prisma.customer.create({
      data: {
        id: 1,
        code: 'CUST01',
        name: 'Test Customer',
        createdBy: 'test',
      },
    });

    await prisma.product.create({
      data: {
        id: 1,
        code: 'PROD01',
        name: 'Test Product',
        category: 'bouqu',
        purchasePrice: 5000,
        salesPrice: 10000,
        taxCategory: 'stand',
        salesStatus: 'available',
        createdBy: 'test',
      },
    });

    await prisma.order.create({
      data: {
        id: 100,
        customerId: 1,
        productId: 1,
        orderDate: new Date('2024-01-10'),
        quantity: 1,
        desiredDeliveryDate: new Date('2024-01-15'),
        deliveryAddress: 'Test Address',
        deliveryPhone: '000-0000-0000',
        status: 'confirmed',
        createdBy: 'test',
      },
    });
  });

  describe('save and findById', () => {
    it('should save and retrieve a return', async () => {
      const returnEntity = Return.create(
        1,
        100,
        new Date('2024-01-20'),
        new Date('2024-01-10'),
        'Damaged',
        10000,
        'user1'
      );

      await repository.save(returnEntity);

      const found = await repository.findById(1);
      expect(found).not.toBeNull();
      expect(found?.getId()).toBe(1);
      expect(found?.getOrderId()).toBe(100);
      expect(found?.getReason()).toBe('Damaged');
      expect(found?.getRefundAmount()).toBe(10000);
    });
  });

  describe('findByOrderId', () => {
    it('should find returns by order id', async () => {
      const return1 = Return.create(
        1,
        100,
        new Date('2024-01-20'),
        new Date('2024-01-10'),
        'Damaged',
        10000,
        'user1'
      );

      await repository.save(return1);

      const returns = await repository.findByOrderId(100);
      expect(returns).toHaveLength(1);
      expect(returns[0].getId()).toBe(1);
    });

    it('should return empty array if no returns found', async () => {
      const returns = await repository.findByOrderId(999);
      expect(returns).toHaveLength(0);
    });
  });
});
