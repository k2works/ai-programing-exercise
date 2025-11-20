import { FastifyInstance } from 'fastify';
import { z } from 'zod';
import { InventoryManagementService } from '../../application/inventory/InventoryManagementService';
import { PrismaSupplierRepository } from '../../infrastructure/inventory/PrismaSupplierRepository';
import { PrismaItemRepository } from '../../infrastructure/inventory/PrismaItemRepository';

const createItemSchema = z.object({
  id: z.number(),
  code: z.string(),
  name: z.string(),
  supplierId: z.number(),
  qualityDays: z.number(),
  leadTime: z.number(),
  purchaseUnitQty: z.number(),
  purchasePrice: z.number(),
});

const updateItemSchema = z.object({
  qualityDays: z.number(),
  leadTime: z.number(),
  purchaseUnitQty: z.number(),
  purchasePrice: z.number(),
});

const createSupplierSchema = z.object({
  id: z.number(),
  code: z.string(),
  name: z.string(),
  phone: z.string().nullable().optional(),
  email: z.string().nullable().optional(),
});

export async function inventoryRoutes(server: FastifyInstance) {
  const supplierRepo = new PrismaSupplierRepository((server as any).prisma);
  const itemRepo = new PrismaItemRepository((server as any).prisma);
  const service = new InventoryManagementService(supplierRepo, itemRepo);

  server.post(
    '/api/items',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['inventory'],
        security: [{ bearerAuth: [] }],
        body: {
          type: 'object',
          required: ['id', 'code', 'name', 'supplierId', 'qualityDays', 'leadTime', 'purchaseUnitQty', 'purchasePrice'],
          properties: {
            id: { type: 'number' },
            code: { type: 'string' },
            name: { type: 'string' },
            supplierId: { type: 'number' },
            qualityDays: { type: 'number' },
            leadTime: { type: 'number' },
            purchaseUnitQty: { type: 'number' },
            purchasePrice: { type: 'number' },
          },
        },
        response: {
          201: {
            type: 'object',
            properties: { message: { type: 'string' } },
          },
        },
      },
    },
    async (request, reply) => {
      const data = createItemSchema.parse(request.body);
      const userId = (request.user as any).userId;

      try {
        await service.registerItem(
          data.id,
          data.code,
          data.name,
          data.supplierId,
          data.qualityDays,
          data.leadTime,
          data.purchaseUnitQty,
          data.purchasePrice,
          userId
        );
        reply.code(201).send({ message: '単品を登録しました' });
      } catch (error: any) {
        reply.code(400).send({ error: error.message });
      }
    }
  );

  server.put(
    '/api/items/:id',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['inventory'],
        security: [{ bearerAuth: [] }],
        body: {
          type: 'object',
          required: ['qualityDays', 'leadTime', 'purchaseUnitQty', 'purchasePrice'],
          properties: {
            qualityDays: { type: 'number' },
            leadTime: { type: 'number' },
            purchaseUnitQty: { type: 'number' },
            purchasePrice: { type: 'number' },
          },
        },
        response: {
          200: {
            type: 'object',
            properties: { message: { type: 'string' } },
          },
        },
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      const data = updateItemSchema.parse(request.body);

      try {
        await service.updateItem(
          Number(id),
          data.qualityDays,
          data.leadTime,
          data.purchaseUnitQty,
          data.purchasePrice
        );
        reply.send({ message: '単品を更新しました' });
      } catch (error: any) {
        reply.code(400).send({ error: error.message });
      }
    }
  );

  server.get(
    '/api/items/:id',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['inventory'],
        security: [{ bearerAuth: [] }],
        response: {
          200: {
            type: 'object',
            properties: {
              id: { type: 'number' },
              code: { type: 'string' },
              name: { type: 'string' },
              supplierId: { type: 'number' },
              qualityDays: { type: 'number' },
              leadTime: { type: 'number' },
              purchaseUnitQty: { type: 'number' },
              purchasePrice: { type: 'number' },
              createdBy: { type: 'string' },
              createdAt: { type: 'string' },
              updatedAt: { type: 'string' },
            },
          },
        },
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      const item = await itemRepo.findById(Number(id));

      if (!item) {
        return reply.code(404).send({ error: '単品が見つかりません' });
      }

      reply.send(item.toJSON());
    }
  );

  server.post(
    '/api/suppliers',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['inventory'],
        security: [{ bearerAuth: [] }],
        body: {
          type: 'object',
          required: ['id', 'code', 'name'],
          properties: {
            id: { type: 'number' },
            code: { type: 'string' },
            name: { type: 'string' },
            phone: { type: 'string', nullable: true },
            email: { type: 'string', nullable: true },
          },
        },
        response: {
          201: {
            type: 'object',
            properties: { message: { type: 'string' } },
          },
        },
      },
    },
    async (request, reply) => {
      const data = createSupplierSchema.parse(request.body);
      const userId = (request.user as any).userId;

      try {
        await service.registerSupplier(
          data.id,
          data.code,
          data.name,
          data.phone || null,
          data.email || null,
          userId
        );
        reply.code(201).send({ message: '仕入先を登録しました' });
      } catch (error: any) {
        reply.code(400).send({ error: error.message });
      }
    }
  );

  server.patch(
    '/api/suppliers/:id/deactivate',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['inventory'],
        security: [{ bearerAuth: [] }],
        response: {
          200: {
            type: 'object',
            properties: { message: { type: 'string' } },
          },
        },
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };

      try {
        await service.deactivateSupplier(Number(id));
        reply.send({ message: '仕入先を無効化しました' });
      } catch (error: any) {
        reply.code(400).send({ error: error.message });
      }
    }
  );

  server.patch(
    '/api/suppliers/:id/activate',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['inventory'],
        security: [{ bearerAuth: [] }],
        response: {
          200: {
            type: 'object',
            properties: { message: { type: 'string' } },
          },
        },
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };

      try {
        await service.activateSupplier(Number(id));
        reply.send({ message: '仕入先を有効化しました' });
      } catch (error: any) {
        reply.code(400).send({ error: error.message });
      }
    }
  );

  server.get(
    '/api/inventory',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['inventory'],
        security: [{ bearerAuth: [] }],
      },
    },
    async (request, reply) => {
      const inventories = await (server as any).prisma.inventory.findMany({
        include: { item: true },
        orderBy: { id: 'asc' },
      });
      reply.send(inventories);
    }
  );
}
