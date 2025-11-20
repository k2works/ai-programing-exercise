import { FastifyInstance } from 'fastify';
import { z } from 'zod';
import { AuthService } from '../application/auth/AuthService';
import { PrismaUserRepository } from '../infrastructure/auth/PrismaUserRepository';
import { prisma } from '../lib/prisma';

const loginSchema = z.object({
  userId: z.string(),
  password: z.string(),
});

export async function authRoutes(fastify: FastifyInstance) {
  const authService = new AuthService(process.env.JWT_SECRET || 'secret');
  const userRepository = new PrismaUserRepository(prisma);

  // Login
  fastify.post('/login', {
    schema: {
      body: {
        type: 'object',
        required: ['userId', 'password'],
        properties: {
          userId: { type: 'string' },
          password: { type: 'string' },
        },
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
    onRequest: [fastify.authenticate],
    schema: {
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
