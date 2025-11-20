import { FastifyInstance } from 'fastify';
import { z } from 'zod';
import { UserManagementService } from '../application/user/UserManagementService';
import { PrismaUserRepository } from '../infrastructure/auth/PrismaUserRepository';
import { prisma } from '../lib/prisma';

const createUserSchema = z.object({
  id: z.string(),
  firstName: z.string(),
  lastName: z.string(),
  password: z.string().min(8),
  roleName: z.string(),
  userType: z.string(),
});

const updateUserSchema = z.object({
  firstName: z.string().optional(),
  lastName: z.string().optional(),
});

export async function userRoutes(fastify: FastifyInstance) {
  const userRepository = new PrismaUserRepository(prisma);
  const userService = new UserManagementService(userRepository);

  // Create user
  fastify.post('/', {
    onRequest: [fastify.authenticate],
    schema: {
      body: {
        type: 'object',
        required: ['id', 'firstName', 'lastName', 'password', 'roleName', 'userType'],
        properties: {
          id: { type: 'string' },
          firstName: { type: 'string' },
          lastName: { type: 'string' },
          password: { type: 'string', minLength: 8 },
          roleName: { type: 'string' },
          userType: { type: 'string' },
        },
      },
    },
  }, async (request, reply) => {
    try {
      const data = createUserSchema.parse(request.body);
      const createdBy = (request.user as any).userId;

      const user = await userService.registerUser(
        data.id,
        data.firstName,
        data.lastName,
        data.password,
        data.roleName,
        data.userType,
        createdBy
      );

      return reply.code(201).send(user.toJSON());
    } catch (error) {
      return reply.code(400).send({ error: (error as Error).message });
    }
  });

  // Update user
  fastify.put('/:id', {
    onRequest: [fastify.authenticate],
    schema: {
      params: {
        type: 'object',
        properties: {
          id: { type: 'string' },
        },
      },
      body: {
        type: 'object',
        properties: {
          firstName: { type: 'string' },
          lastName: { type: 'string' },
        },
      },
    },
  }, async (request, reply) => {
    try {
      const { id } = request.params as { id: string };
      const data = updateUserSchema.parse(request.body);

      const user = await userService.updateUser(id, data.firstName, data.lastName);

      return user.toJSON();
    } catch (error) {
      const message = (error as Error).message;
      if (message === 'User not found') {
        return reply.code(404).send({ error: message });
      }
      return reply.code(400).send({ error: message });
    }
  });

  // Delete user
  fastify.delete('/:id', {
    onRequest: [fastify.authenticate],
    schema: {
      params: {
        type: 'object',
        properties: {
          id: { type: 'string' },
        },
      },
    },
  }, async (request, reply) => {
    try {
      const { id } = request.params as { id: string };
      await userService.deleteUser(id);

      return reply.code(204).send();
    } catch (error) {
      const message = (error as Error).message;
      if (message === 'User not found') {
        return reply.code(404).send({ error: message });
      }
      return reply.code(400).send({ error: message });
    }
  });

  // Deactivate user
  fastify.patch('/:id/deactivate', {
    onRequest: [fastify.authenticate],
    schema: {
      params: {
        type: 'object',
        properties: {
          id: { type: 'string' },
        },
      },
    },
  }, async (request, reply) => {
    try {
      const { id } = request.params as { id: string };
      await userService.deactivateUser(id);

      return { message: 'User deactivated successfully' };
    } catch (error) {
      const message = (error as Error).message;
      if (message === 'User not found') {
        return reply.code(404).send({ error: message });
      }
      return reply.code(400).send({ error: message });
    }
  });

  // Reactivate user
  fastify.patch('/:id/reactivate', {
    onRequest: [fastify.authenticate],
    schema: {
      params: {
        type: 'object',
        properties: {
          id: { type: 'string' },
        },
      },
    },
  }, async (request, reply) => {
    try {
      const { id } = request.params as { id: string };
      await userService.reactivateUser(id);

      return { message: 'User reactivated successfully' };
    } catch (error) {
      const message = (error as Error).message;
      if (message === 'User not found') {
        return reply.code(404).send({ error: message });
      }
      return reply.code(400).send({ error: message });
    }
  });
}
