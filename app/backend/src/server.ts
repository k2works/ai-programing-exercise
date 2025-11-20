import Fastify from 'fastify';
import cors from '@fastify/cors';
import jwt from '@fastify/jwt';
import swagger from '@fastify/swagger';
import swaggerUi from '@fastify/swagger-ui';
import { authenticate } from './plugins/auth';
import { authRoutes } from './routes/auth';

export async function createServer() {
  const server = Fastify({
    logger: true,
  });

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

  return server;
}
