import Fastify from 'fastify';
import cors from '@fastify/cors';
import jwt from '@fastify/jwt';
import swagger from '@fastify/swagger';
import swaggerUi from '@fastify/swagger-ui';
import { PrismaClient } from '@prisma/client';
import { authenticate } from './plugins/auth';
import { authRoutes } from './routes/auth';
import { userRoutes } from './routes/users';
import { productRoutes } from './api/routes/products';
import { orderRoutes } from './api/routes/orders';
import { itemRoutes } from './api/routes/items';
import { receivedOrderRoutes } from './api/routes/receivedOrders';
import { inventoryRoutes } from './api/routes/inventory';

export async function createServer(prismaClient?: PrismaClient) {
  const server = Fastify({
    logger: true,
  });

  // Register Prisma
  const prisma = prismaClient || new PrismaClient();
  server.decorate('prisma', prisma);

  // CORS
  await server.register(cors, {
    origin: true,
  });

  // JWT
  await server.register(jwt, {
    secret: process.env.JWT_SECRET || 'your-secret-key-change-in-production',
  });

  // Register authenticate decorator
  server.decorate('authenticate', authenticate);

  // Swagger
  await server.register(swagger, {
    openapi: {
      info: {
        title: 'Bouquet API',
        description: 'API documentation for Bouquet',
        version: '0.1.0',
      },
      servers: [
        {
          url: 'http://localhost:3000',
          description: 'Development server',
        },
      ],
      components: {
        securitySchemes: {
          bearerAuth: {
            type: 'http',
            scheme: 'bearer',
            bearerFormat: 'JWT',
          },
        },
      },
    },
  });

  await server.register(swaggerUi, {
    routePrefix: '/docs',
    uiConfig: {
      docExpansion: 'list',
      deepLinking: false,
    },
  });

  // Health check
  server.get('/health', async () => {
    return { status: 'ok' };
  });

  // Auth routes
  await server.register(authRoutes, { prefix: '/api/auth' });

  // User routes
  await server.register(userRoutes, { prefix: '/api/users' });

  // Product routes
  await server.register(productRoutes);

  // Order routes
  await server.register(orderRoutes);

  // Item routes
  await server.register(itemRoutes);

  // Received Order routes
  await server.register(receivedOrderRoutes);

  // Inventory routes
  await server.register(inventoryRoutes);

  return server;
}

export const buildServer = createServer;
