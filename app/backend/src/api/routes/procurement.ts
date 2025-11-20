import { FastifyInstance } from 'fastify';
import { z } from 'zod';
import { PlacementOrderService } from '../../application/procurement/PlacementOrderService';
import { ArrivalService } from '../../application/procurement/ArrivalService';
import { PrismaPlacementOrderRepository } from '../../infrastructure/procurement/PrismaPlacementOrderRepository';
import { PrismaArrivalRepository } from '../../infrastructure/procurement/PrismaArrivalRepository';
import { PrismaItemRepository } from '../../infrastructure/inventory/PrismaItemRepository';
import { PrismaInventoryRepository } from '../../infrastructure/inventory/PrismaInventoryRepository';

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

const createArrivalSchema = z.object({
  id: z.number(),
  placementOrderId: z.number(),
  arrivalDate: z.string(),
  lines: z.array(
    z.object({
      placementOrderLineId: z.number(),
      itemId: z.number(),
      arrivedQty: z.number(),
    })
  ),
});

const inspectArrivalSchema = z.object({
  lineInspections: z.array(
    z.object({
      lineId: z.number(),
      result: z.enum(['accepted', 'rejected']),
    })
  ),
});

export async function procurementRoutes(server: FastifyInstance) {
  const orderRepo = new PrismaPlacementOrderRepository((server as any).prisma);
  const arrivalRepo = new PrismaArrivalRepository((server as any).prisma);
  const itemRepo = new PrismaItemRepository((server as any).prisma);
  const inventoryRepo = new PrismaInventoryRepository((server as any).prisma);
  
  const placementOrderService = new PlacementOrderService(orderRepo, itemRepo);
  const arrivalService = new ArrivalService(arrivalRepo, orderRepo, inventoryRepo, itemRepo);

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
        await placementOrderService.createPlacementOrder(
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
        await placementOrderService.cancelPlacementOrder(Number(id));
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

  server.post(
    '/api/arrivals',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['procurement'],
        security: [{ bearerAuth: [] }],
        body: {
          type: 'object',
          required: ['id', 'placementOrderId', 'arrivalDate', 'lines'],
          properties: {
            id: { type: 'number' },
            placementOrderId: { type: 'number' },
            arrivalDate: { type: 'string' },
            lines: {
              type: 'array',
              items: {
                type: 'object',
                required: ['placementOrderLineId', 'itemId', 'arrivedQty'],
                properties: {
                  placementOrderLineId: { type: 'number' },
                  itemId: { type: 'number' },
                  arrivedQty: { type: 'number' },
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
      const data = createArrivalSchema.parse(request.body);
      const userId = (request.user as any).userId;

      try {
        await arrivalService.receiveItems(
          data.id,
          data.placementOrderId,
          new Date(data.arrivalDate),
          data.lines,
          userId
        );
        reply.code(201).send({ message: '入荷を記録しました' });
      } catch (error: any) {
        reply.code(400).send({ error: error.message });
      }
    }
  );

  server.post(
    '/api/arrivals/:id/inspect',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['procurement'],
        security: [{ bearerAuth: [] }],
        body: {
          type: 'object',
          required: ['lineInspections'],
          properties: {
            lineInspections: {
              type: 'array',
              items: {
                type: 'object',
                required: ['lineId', 'result'],
                properties: {
                  lineId: { type: 'number' },
                  result: { type: 'string', enum: ['accepted', 'rejected'] },
                },
              },
            },
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
      const data = inspectArrivalSchema.parse(request.body);

      try {
        await arrivalService.inspectItems(Number(id), data.lineInspections);
        reply.send({ message: '検収を完了しました' });
      } catch (error: any) {
        reply.code(400).send({ error: error.message });
      }
    }
  );
}
