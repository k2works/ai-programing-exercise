import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { FastifyInstance } from 'fastify';
import { createServer } from '../../server';
import { setupTestDatabase, teardownTestDatabase, getPrismaClient } from '../../test/prisma-test-helper';

describe('Order API', () => {
  let server: FastifyInstance;
  let authToken: string;

  beforeAll(async () => {
    await setupTestDatabase();
    const testPrisma = getPrismaClient();
    server = await createServer(testPrisma);
    await server.ready();

    const loginResponse = await server.inject({
      method: 'POST',
      url: '/api/auth/login',
      payload: {
        userId: 'admin-001',
        password: 'admin123',
      },
    });
    authToken = JSON.parse(loginResponse.body).token;
  }, 60000);

  afterAll(async () => {
    await server.close();
    await teardownTestDatabase();
  });

  beforeEach(async () => {
    const prisma = getPrismaClient();
    await prisma.order.deleteMany({});
    await prisma.customer.deleteMany({});
    await prisma.product.deleteMany({});

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
        category: 'BQT',
        salesPrice: 1000,
        purchasePrice: 500,
        taxCategory: 'TAX10',
        salesStatus: 'on_sale',
        createdBy: 'test',
      },
    });
  });

  describe('POST /api/orders', () => {
    it('should create a new order', async () => {
      const response = await server.inject({
        method: 'POST',
        url: '/api/orders',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 1,
          orderDate: '2025-01-01',
          customerId: 1,
          productId: 1,
          quantity: 5,
          desiredDeliveryDate: '2025-01-05',
          deliveryAddress: 'Tokyo',
          deliveryPhone: '03-1234-5678',
          deliveryMessage: 'Test message',
        },
      });

      expect(response.statusCode).toBe(201);
    });

    it('should return 401 without auth', async () => {
      const response = await server.inject({
        method: 'POST',
        url: '/api/orders',
        payload: {
          id: 1,
          orderDate: '2025-01-01',
          customerId: 1,
          productId: 1,
          quantity: 5,
          desiredDeliveryDate: '2025-01-05',
          deliveryAddress: 'Tokyo',
          deliveryPhone: '03-1234-5678',
        },
      });

      expect(response.statusCode).toBe(401);
    });
  });

  describe('GET /api/orders/:id', () => {
    it('should get order by id', async () => {
      await server.inject({
        method: 'POST',
        url: '/api/orders',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 1,
          orderDate: '2025-01-01',
          customerId: 1,
          productId: 1,
          quantity: 5,
          desiredDeliveryDate: '2025-01-05',
          deliveryAddress: 'Tokyo',
          deliveryPhone: '03-1234-5678',
        },
      });

      const response = await server.inject({
        method: 'GET',
        url: '/api/orders/1',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      expect(response.statusCode).toBe(200);
      const order = JSON.parse(response.body);
      expect(order.customerId).toBe(1);
      expect(order.productId).toBe(1);
    });

    it('should return 404 for non-existent order', async () => {
      const response = await server.inject({
        method: 'GET',
        url: '/api/orders/99999',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      expect(response.statusCode).toBe(404);
    });
  });

  describe('PATCH /api/orders/:id/delivery-date', () => {
    it('should change delivery date', async () => {
      await server.inject({
        method: 'POST',
        url: '/api/orders',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 1,
          orderDate: '2025-01-01',
          customerId: 1,
          productId: 1,
          quantity: 5,
          desiredDeliveryDate: '2025-01-05',
          deliveryAddress: 'Tokyo',
          deliveryPhone: '03-1234-5678',
        },
      });

      const response = await server.inject({
        method: 'PATCH',
        url: '/api/orders/1/delivery-date',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          newDate: '2025-01-10',
        },
      });

      expect(response.statusCode).toBe(200);
    });
  });

  describe('PATCH /api/orders/:id/cancel', () => {
    it('should cancel order', async () => {
      await server.inject({
        method: 'POST',
        url: '/api/orders',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 1,
          orderDate: '2025-01-01',
          customerId: 1,
          productId: 1,
          quantity: 5,
          desiredDeliveryDate: '2025-01-05',
          deliveryAddress: 'Tokyo',
          deliveryPhone: '03-1234-5678',
        },
      });

      const response = await server.inject({
        method: 'PATCH',
        url: '/api/orders/1/cancel',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      expect(response.statusCode).toBe(200);
    });
  });

  describe('GET /api/customers/:customerId/orders', () => {
    it('should get customer orders', async () => {
      await server.inject({
        method: 'POST',
        url: '/api/orders',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 1,
          orderDate: '2025-01-01',
          customerId: 1,
          productId: 1,
          quantity: 5,
          desiredDeliveryDate: '2025-01-05',
          deliveryAddress: 'Tokyo',
          deliveryPhone: '03-1234-5678',
        },
      });

      const response = await server.inject({
        method: 'GET',
        url: '/api/customers/1/orders',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      expect(response.statusCode).toBe(200);
      const orders = JSON.parse(response.body);
      expect(Array.isArray(orders)).toBe(true);
      expect(orders.length).toBeGreaterThan(0);
    });
  });
});
