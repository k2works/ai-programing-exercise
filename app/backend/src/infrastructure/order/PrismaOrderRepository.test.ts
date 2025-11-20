import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { PrismaOrderRepository } from './PrismaOrderRepository';
import { Order } from '../../domain/order/Order';
import { setupTestDatabase, teardownTestDatabase, getPrismaClient } from '../../test/prisma-test-helper';

describe('PrismaOrderRepository', () => {
  let repository: PrismaOrderRepository;

  beforeAll(async () => {
    const prisma = await setupTestDatabase();
    repository = new PrismaOrderRepository(prisma);
  }, 60000);

  afterAll(async () => {
    await teardownTestDatabase();
  });

  beforeEach(async () => {
    const prisma = getPrismaClient();
    await prisma.order.deleteMany({});
    await prisma.customer.deleteMany({});
    await prisma.product.deleteMany({});

    // Create test customer
    await prisma.customer.create({
      data: {
        id: 1,
        code: 'CUST01',
        name: 'Test Customer',
        createdBy: 'test',
      },
    });

    // Create test product
    await prisma.product.create({
      data: {
        id: 1,
        code: 'PROD01',
        name: 'Test Product',
        category: 'BQT',
        salesPrice: 1000,
        purchasePrice: 500,
        taxCategory: 'TAX10',
        salesStatus: 'on_sale',
        createdBy: 'test',
      },
    });
  });

  describe('save and findById', () => {
    it('should save and retrieve order', async () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      const order = Order.create(
        1,
        orderDate,
        1,
        1,
        5,
        deliveryDate,
        'Tokyo',
        '03-1234-5678',
        'Test message',
        'system'
      );

      await repository.save(order);

      const found = await repository.findById(1);
      expect(found).not.toBeNull();
      expect(found?.getCustomerId()).toBe(1);
      expect(found?.getProductId()).toBe(1);
      expect(found?.getQuantity()).toBe(5);
      expect(found?.getStatus()).toBe('pending');
    });

    it('should return null for non-existent order', async () => {
      const found = await repository.findById(99999);
      expect(found).toBeNull();
    });
  });

  describe('findByCustomerId', () => {
    it('should find orders by customer', async () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      
      const order1 = Order.create(1, orderDate, 1, 1, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system');
      const order2 = Order.create(2, orderDate, 1, 1, 3, deliveryDate, 'Osaka', '06-1234-5678', null, 'system');

      await repository.save(order1);
      await repository.save(order2);

      const orders = await repository.findByCustomerId(1);
      expect(orders).toHaveLength(2);
    });
  });

  describe('findPendingOrders', () => {
    it('should find only pending orders', async () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      
      const order1 = Order.create(1, orderDate, 1, 1, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system');
      const order2 = Order.create(2, orderDate, 1, 1, 3, deliveryDate, 'Osaka', '06-1234-5678', null, 'system');
      order2.cancel();

      await repository.save(order1);
      await repository.save(order2);

      const orders = await repository.findPendingOrders();
      expect(orders).toHaveLength(1);
      expect(orders[0].getStatus()).toBe('pending');
    });
  });

  describe('update', () => {
    it('should update order', async () => {
      const orderDate = new Date('2025-01-01');
      const deliveryDate = new Date('2025-01-05');
      const order = Order.create(1, orderDate, 1, 1, 5, deliveryDate, 'Tokyo', '03-1234-5678', null, 'system');

      await repository.save(order);

      const newDeliveryDate = new Date('2025-01-10');
      order.changeDeliveryDate(newDeliveryDate);
      await repository.save(order);

      const found = await repository.findById(1);
      expect(found?.getDesiredDeliveryDate()).toEqual(newDeliveryDate);
    });
  });
});
