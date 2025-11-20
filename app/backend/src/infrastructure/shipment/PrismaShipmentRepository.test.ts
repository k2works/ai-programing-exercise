import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { PrismaShipmentRepository } from './PrismaShipmentRepository';
import { Shipment } from '../../domain/shipment/Shipment';
import { setupTestDatabase, teardownTestDatabase, getPrismaClient } from '../../test/prisma-test-helper';

describe('PrismaShipmentRepository', () => {
  let repository: PrismaShipmentRepository;

  beforeAll(async () => {
    const prisma = await setupTestDatabase();
    repository = new PrismaShipmentRepository(prisma);
  }, 60000);

  afterAll(async () => {
    await teardownTestDatabase();
  });

  beforeEach(async () => {
    const prisma = getPrismaClient();
    await prisma.shipment.deleteMany({});
    await prisma.receivedOrder.deleteMany({});
    await prisma.order.deleteMany({});
    await prisma.product.deleteMany({});
    await prisma.customer.deleteMany({});

    await prisma.customer.create({
      data: {
        id: 1,
        code: 'CUST001',
        name: 'Test Customer',
        email: 'test@example.com',
        phone: '000-0000-0000',
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
        id: 1,
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

    await prisma.receivedOrder.create({
      data: {
        id: 100,
        orderId: 1,
        receivedDate: new Date('2024-01-10'),
        scheduledShipmentDate: new Date('2024-01-15'),
        totalAmount: 10000,
        totalTax: 1000,
        allocationStatus: 'allocated',
        createdBy: 'test',
      },
    });
  });

  describe('save and findById', () => {
    it('should save and retrieve a shipment', async () => {
      const shipment = Shipment.create(
        1,
        100,
        new Date('2024-01-15'),
        new Date('2024-01-15'),
        'Yamato',
        'TRACK123',
        'user1'
      );

      await repository.save(shipment);

      const found = await repository.findById(1);
      expect(found).not.toBeNull();
      expect(found?.getId()).toBe(1);
      expect(found?.getReceivedOrderId()).toBe(100);
      expect(found?.getCarrier()).toBe('Yamato');
      expect(found?.getTrackingNumber()).toBe('TRACK123');
    });
  });

  describe('findByReceivedOrderId', () => {
    it('should find shipment by received order id', async () => {
      const shipment = Shipment.create(
        1,
        100,
        new Date('2024-01-15'),
        new Date('2024-01-15'),
        'Yamato',
        'TRACK123',
        'user1'
      );

      await repository.save(shipment);

      const found = await repository.findByReceivedOrderId(100);
      expect(found).not.toBeNull();
      expect(found?.getId()).toBe(1);
      expect(found?.getReceivedOrderId()).toBe(100);
    });

    it('should return null if not found', async () => {
      const found = await repository.findByReceivedOrderId(999);
      expect(found).toBeNull();
    });
  });
});
