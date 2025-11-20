import { FastifyInstance } from 'fastify';
import { z } from 'zod';
import { PlacementOrderService } from '../../application/procurement/PlacementOrderService';
import { PrismaPlacementOrderRepository } from '../../infrastructure/procurement/PrismaPlacementOrderRepository';
import { PrismaItemRepository } from '../../infrastructure/inventory/PrismaItemRepository';

const createPlacementOrderSchema = z.object({
  id: z.number(),
  orderDate: z.string(),
  supplierId: z.number(),
  desiredDeliveryDate: z.string(),
  lines: z.array(
    z.object({
      itemId: z.number(),
      orderQty: z.number(),
    })
  ),
});

export async function procurementRoutes(server: FastifyInstance) {
  const orderRepo = new PrismaPlacementOrderRepository((server as any).prisma);
  const itemRepo = new PrismaItemRepository((server as any).prisma);
  const service = new PlacementOrderService(orderRepo, itemRepo);

  server.post(
    '/api/placement-orders',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['procurement'],
        security: [{ bearerAuth: [] }],
        body: {
          type: 'object',
          required: ['id', 'orderDate', 'supplierId', 'desiredDeliveryDate', 'lines'],
          properties: {
            id: { type: 'number' },
            orderDate: { type: 'string' },
            supplierId: { type: 'number' },
            desiredDeliveryDate: { type: 'string' },
            lines: {
              type: 'array',
              items: {
                type: 'object',
                required: ['itemId', 'orderQty'],
                properties: {
                  itemId: { type: 'number' },
                  orderQty: { type: 'number' },
                },
              },
            },
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
      const data = createPlacementOrderSchema.parse(request.body);
      const userId = (request.user as any).userId;

      try {
        await service.createPlacementOrder(
          data.id,
          new Date(data.orderDate),
          data.supplierId,
          new Date(data.desiredDeliveryDate),
          data.lines,
          userId
        );
        reply.code(201).send({ message: '発注を作成しました' });
      } catch (error: any) {
        reply.code(400).send({ error: error.message });
      }
    }
  );

  server.delete(
    '/api/placement-orders/:id',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['procurement'],
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
        await service.cancelPlacementOrder(Number(id));
        reply.send({ message: '発注をキャンセルしました' });
      } catch (error: any) {
        reply.code(400).send({ error: error.message });
      }
    }
  );

  server.get(
    '/api/placement-orders/:id',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['procurement'],
        security: [{ bearerAuth: [] }],
        response: {
          200: {
            type: 'object',
            properties: {
              id: { type: 'number' },
              orderDate: { type: 'string' },
              supplierId: { type: 'number' },
              desiredDeliveryDate: { type: 'string' },
              totalAmount: { type: 'number' },
              totalTax: { type: 'number' },
              status: { type: 'string' },
              lines: {
                type: 'array',
                items: {
                  type: 'object',
                  properties: {
                    id: { type: 'number' },
                    itemId: { type: 'number' },
                    orderQty: { type: 'number' },
                    purchasePrice: { type: 'number' },
                    arrivedQty: { type: 'number' },
                  },
                },
              },
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

      const order = await orderRepo.findById(Number(id));
      if (!order) {
        return reply.code(404).send({ error: '発注が見つかりません' });
      }

      reply.send(order.toJSON());
    }
  );
}
