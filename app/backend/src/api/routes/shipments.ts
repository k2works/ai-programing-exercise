import { FastifyInstance } from 'fastify';
import { PickingService } from '../../application/shipment/PickingService';
import { ShipmentService } from '../../application/shipment/ShipmentService';
import { ReturnService } from '../../application/shipment/ReturnService';
import { PrismaReceivedOrderRepository } from '../../infrastructure/receivedOrder/PrismaReceivedOrderRepository';
import { PrismaInventoryRepository } from '../../infrastructure/inventory/PrismaInventoryRepository';
import { PrismaProductCompositionRepository } from '../../infrastructure/product/PrismaProductCompositionRepository';
import { PrismaShipmentRepository } from '../../infrastructure/shipment/PrismaShipmentRepository';
import { PrismaReturnRepository } from '../../infrastructure/shipment/PrismaReturnRepository';

export async function shipmentRoutes(fastify: FastifyInstance) {
  const prisma = (fastify as any).prisma;

  const receivedOrderRepo = new PrismaReceivedOrderRepository(prisma);
  const inventoryRepo = new PrismaInventoryRepository(prisma);
  const compositionRepo = new PrismaProductCompositionRepository(prisma);
  const shipmentRepo = new PrismaShipmentRepository(prisma);
  const returnRepo = new PrismaReturnRepository(prisma);

  const pickingService = new PickingService(
    receivedOrderRepo,
    inventoryRepo,
    compositionRepo,
    prisma
  );
  const shipmentService = new ShipmentService(
    shipmentRepo,
    receivedOrderRepo,
    prisma
  );
  const returnService = new ReturnService(returnRepo, prisma);

  // Get picking list
  fastify.get('/picking-list', {
    onRequest: [fastify.authenticate],
    schema: {
      security: [{ bearerAuth: [] }],
      querystring: {
        type: 'object',
        required: ['receivedOrderId'],
        properties: {
          receivedOrderId: { type: 'number' },
        },
      },
      response: {
        200: {
          type: 'object',
          properties: {
            receivedOrderId: { type: 'number' },
            items: {
              type: 'array',
              items: {
                type: 'object',
                properties: {
                  itemId: { type: 'number' },
                  itemName: { type: 'string' },
                  requiredQuantity: { type: 'number' },
                  lotNumber: { type: 'string' },
                },
              },
            },
          },
        },
      },
    },
  }, async (request) => {
    const { receivedOrderId } = request.query as { receivedOrderId: number };
    return await pickingService.generatePickingList(receivedOrderId);
  });

  // Confirm picking
  fastify.post('/:id/confirm-picking', {
    onRequest: [fastify.authenticate],
    schema: {
      security: [{ bearerAuth: [] }],
      params: {
        type: 'object',
        properties: {
          id: { type: 'number' },
        },
      },
      response: {
        200: {
          type: 'object',
          properties: {
            message: { type: 'string' },
          },
        },
      },
    },
  }, async (request, reply) => {
    const { id } = request.params as { id: number };
    const userId = (request.user as any).userId;

    await pickingService.confirmPicking(id, userId);
    return { message: 'Picking confirmed successfully' };
  });

  // Create shipment
  fastify.post('/', {
    onRequest: [fastify.authenticate],
    schema: {
      security: [{ bearerAuth: [] }],
      body: {
        type: 'object',
        required: ['receivedOrderId', 'shipmentDate'],
        properties: {
          receivedOrderId: { type: 'number' },
          shipmentDate: { type: 'string', format: 'date' },
          carrier: { type: 'string' },
          trackingNumber: { type: 'string' },
        },
      },
      response: {
        201: {
          type: 'object',
          properties: {
            message: { type: 'string' },
          },
        },
      },
    },
  }, async (request, reply) => {
    const { receivedOrderId, shipmentDate, carrier, trackingNumber } =
      request.body as {
        receivedOrderId: number;
        shipmentDate: string;
        carrier?: string;
        trackingNumber?: string;
      };
    const userId = (request.user as any).userId;

    await shipmentService.executeShipment(
      receivedOrderId,
      new Date(shipmentDate),
      carrier || null,
      trackingNumber || null,
      userId
    );

    return reply.code(201).send({ message: 'Shipment created successfully' });
  });

  // Create return
  fastify.post('/returns', {
    onRequest: [fastify.authenticate],
    schema: {
      security: [{ bearerAuth: [] }],
      body: {
        type: 'object',
        required: ['orderId', 'returnDate', 'refundAmount'],
        properties: {
          orderId: { type: 'number' },
          returnDate: { type: 'string', format: 'date' },
          reason: { type: 'string' },
          refundAmount: { type: 'number' },
        },
      },
      response: {
        201: {
          type: 'object',
          properties: {
            message: { type: 'string' },
          },
        },
      },
    },
  }, async (request, reply) => {
    const { orderId, returnDate, reason, refundAmount } = request.body as {
      orderId: number;
      returnDate: string;
      reason?: string;
      refundAmount: number;
    };
    const userId = (request.user as any).userId;

    await returnService.processReturn(
      orderId,
      new Date(returnDate),
      reason || null,
      refundAmount,
      userId
    );

    return reply.code(201).send({ message: 'Return processed successfully' });
  });
}
