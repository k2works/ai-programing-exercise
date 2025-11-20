import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { FastifyInstance } from 'fastify';
import { createServer } from '../../server';
import { setupTestDatabase, teardownTestDatabase, getPrismaClient } from '../../test/prisma-test-helper';

describe('Product API', () => {
  let server: FastifyInstance;
  let authToken: string;

  beforeAll(async () => {
    await setupTestDatabase();
    const testPrisma = getPrismaClient();
    server = await createServer(testPrisma);
    await server.ready();

    // Login to get auth token
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
    await prisma.product.deleteMany({
      where: {
        code: {
          startsWith: 'TST',
        },
      },
    });
  });

  describe('POST /api/products', () => {
    it('should create a new product', async () => {
      const response = await server.inject({
        method: 'POST',
        url: '/api/products',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 100,
          code: 'TST001',
          name: 'Test Product',
          salesPrice: 1000,
        },
      });

      expect(response.statusCode).toBe(201);
    });

    it('should return 401 without auth', async () => {
      const response = await server.inject({
        method: 'POST',
        url: '/api/products',
        payload: {
          id: 101,
          code: 'TST002',
          name: 'Test',
          salesPrice: 1000,
        },
      });

      expect(response.statusCode).toBe(401);
    });
  });

  describe('GET /api/products/:id', () => {
    it('should get product by id', async () => {
      await server.inject({
        method: 'POST',
        url: '/api/products',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 100,
          code: 'TST001',
          name: 'Test Product',
          salesPrice: 1000,
        },
      });

      const response = await server.inject({
        method: 'GET',
        url: '/api/products/100',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      expect(response.statusCode).toBe(200);
      const product = JSON.parse(response.body);
      expect(product.code).toBe('TST001');
    });

    it('should return 404 for non-existent product', async () => {
      const response = await server.inject({
        method: 'GET',
        url: '/api/products/99999',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      expect(response.statusCode).toBe(404);
    });
  });

  describe('GET /api/products', () => {
    it('should get all on-sale products', async () => {
      const response = await server.inject({
        method: 'GET',
        url: '/api/products',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      expect(response.statusCode).toBe(200);
      const products = JSON.parse(response.body);
      expect(Array.isArray(products)).toBe(true);
    });
  });

  describe('PUT /api/products/:id', () => {
    it('should update product', async () => {
      await server.inject({
        method: 'POST',
        url: '/api/products',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 100,
          code: 'TST001',
          name: 'Test Product',
          salesPrice: 1000,
        },
      });

      const response = await server.inject({
        method: 'PUT',
        url: '/api/products/100',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          name: 'Updated Product',
          salesPrice: 1500,
        },
      });

      expect(response.statusCode).toBe(200);
    });
  });

  describe('PATCH /api/products/:id/stop-sales', () => {
    it('should stop sales', async () => {
      await server.inject({
        method: 'POST',
        url: '/api/products',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 100,
          code: 'TST001',
          name: 'Test Product',
          salesPrice: 1000,
        },
      });

      const response = await server.inject({
        method: 'PATCH',
        url: '/api/products/100/stop-sales',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      expect(response.statusCode).toBe(200);
    });
  });

  describe('PATCH /api/products/:id/resume-sales', () => {
    it('should resume sales', async () => {
      await server.inject({
        method: 'POST',
        url: '/api/products',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 100,
          code: 'TST001',
          name: 'Test Product',
          salesPrice: 1000,
        },
      });

      await server.inject({
        method: 'PATCH',
        url: '/api/products/100/stop-sales',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      const response = await server.inject({
        method: 'PATCH',
        url: '/api/products/100/resume-sales',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      expect(response.statusCode).toBe(200);
    });
  });

  describe('PATCH /api/products/:id/end-sales', () => {
    it('should end sales', async () => {
      await server.inject({
        method: 'POST',
        url: '/api/products',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
        payload: {
          id: 100,
          code: 'TST001',
          name: 'Test Product',
          salesPrice: 1000,
        },
      });

      const response = await server.inject({
        method: 'PATCH',
        url: '/api/products/100/end-sales',
        headers: {
          authorization: `Bearer ${authToken}`,
        },
      });

      expect(response.statusCode).toBe(200);
    });
  });
});
