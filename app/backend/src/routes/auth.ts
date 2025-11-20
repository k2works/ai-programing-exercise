import { FastifyInstance } from 'fastify';
import { z } from 'zod';
import { AuthService } from '../application/auth/AuthService';
import { PrismaUserRepository } from '../infrastructure/auth/PrismaUserRepository';

const loginSchema = z.object({
  userId: z.string(),
  password: z.string(),
});

export async function authRoutes(fastify: FastifyInstance) {
  const authService = new AuthService(process.env.JWT_SECRET || 'your-secret-key-change-in-production');
  const userRepository = new PrismaUserRepository((fastify as any).prisma);

  // Login
  fastify.post('/login', {
    schema: {
      description: 'User login',
      tags: ['auth'],
      body: {
        type: 'object',
        required: ['userId', 'password'],
        properties: {
          userId: { 
            type: 'string',
            description: 'User ID (e.g., admin-001)',
          },
          password: { 
            type: 'string',
            description: 'User password',
          },
        },
        examples: [{
          userId: 'admin-001',
          password: 'admin123'
        }]
      },
      response: {
        200: {
          type: 'object',
          properties: {
            token: { type: 'string' },
            user: {
              type: 'object',
              properties: {
                id: { type: 'string' },
                firstName: { type: 'string' },
                lastName: { type: 'string' },
                roleName: { type: 'string' },
                userType: { type: 'string' },
              },
            },
          },
        },
      },
    },
  }, async (request, reply) => {
    const { userId, password } = loginSchema.parse(request.body);

    const user = await userRepository.findById(userId);
    if (!user) {
      return reply.code(401).send({ error: 'Invalid credentials' });
    }

    const isValid = await user.authenticate(password);
    if (!isValid) {
      return reply.code(401).send({ error: 'Invalid credentials' });
    }

    const token = authService.generateToken(user);

    return {
      token,
      user: user.toJSON(),
    };
  });

  // Get current user
  fastify.get('/me', {
    onRequest: [fastify.authenticateApiKeyOrJwt],
    schema: {
      security: [{ bearerAuth: [] }, { ApiKeyAuth: [] }],
      response: {
        200: {
          type: 'object',
          properties: {
            id: { type: 'string' },
            firstName: { type: 'string' },
            lastName: { type: 'string' },
            roleName: { type: 'string' },
            userType: { type: 'string' },
            status: { type: 'string' },
          },
        },
      },
    },
  }, async (request, reply) => {
    const userId = (request.user as any).userId;
    const user = await userRepository.findById(userId);

    if (!user) {
      return reply.code(404).send({ error: 'User not found' });
    }

    return user.toJSON();
  });

  // Logout (client-side token removal)
  fastify.post('/logout', {
    schema: {
      response: {
        200: {
          type: 'object',
          properties: {
            message: { type: 'string' },
          },
        },
      },
    },
  }, async () => {
    return { message: 'Logged out successfully' };
  });
}
