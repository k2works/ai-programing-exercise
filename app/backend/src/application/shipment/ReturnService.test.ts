import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { ReturnService } from './ReturnService';
import { PrismaReturnRepository } from '../../infrastructure/shipment/PrismaReturnRepository';
import { setupTestDatabase, teardownTestDatabase, getPrismaClient } from '../../test/prisma-test-helper';

describe('ReturnService', () => {
  let service: ReturnService;

  beforeAll(async () => {
    const prisma = await setupTestDatabase();
    const returnRepo = new PrismaReturnRepository(prisma);
    service = new ReturnService(returnRepo, prisma);
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
        name: 'Test Bouquet',
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
        id: 1,
        customerId: 1,
        productId: 1,
        orderDate: new Date('2024-01-10'),
        quantity: 2,
        desiredDeliveryDate: new Date('2024-01-15'),
        deliveryAddress: 'Test Address',
        deliveryPhone: '000-0000-0000',
        status: 'confirmed',
        createdBy: 'test',
      },
    });
  });

  describe('processReturn', () => {
    it('should create return record within 30 days', async () => {
      await service.processReturn(
        1,
        new Date('2024-01-25'),
        'Damaged',
        10000,
        'user1'
      );

      const prisma = getPrismaClient();
      const returnRecord = await prisma.return.findFirst({
        where: { orderId: 1 },
      });

      expect(returnRecord).not.toBeNull();
      expect(returnRecord?.reason).toBe('Damaged');
      expect(Number(returnRecord?.refundAmount)).toBe(10000);
      expect(returnRecord?.status).toBe('pending');
    });

    it('should throw error if return is after 30 days', async () => {
      await expect(
        service.processReturn(
          1,
          new Date('2024-02-15'),
          'Damaged',
          10000,
          'user1'
        )
      ).rejects.toThrow('返品期限を過ぎています（注文日から30日以内）');
    });
  });
});
