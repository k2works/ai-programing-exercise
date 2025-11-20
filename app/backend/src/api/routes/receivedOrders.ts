import { FastifyInstance } from 'fastify';
import { ReceivedOrderService } from '../../application/receivedOrder/ReceivedOrderService';
import { PrismaReceivedOrderRepository } from '../../infrastructure/receivedOrder/PrismaReceivedOrderRepository';
import { PrismaOrderRepository } from '../../infrastructure/order/PrismaOrderRepository';
import { PrismaProductRepository } from '../../infrastructure/product/PrismaProductRepository';
import { AllocationService } from '../../application/receivedOrder/AllocationService';

export async function receivedOrderRoutes(server: FastifyInstance) {
  const receivedOrderRepo = new PrismaReceivedOrderRepository((server as any).prisma);
  const orderRepo = new PrismaOrderRepository((server as any).prisma);
  const productRepo = new PrismaProductRepository((server as any).prisma);
  const allocationService = new AllocationService((server as any).prisma);
  const service = new ReceivedOrderService(
    receivedOrderRepo,
    orderRepo,
    productRepo,
    allocationService
  );

  server.get(
    '/api/staff/orders/pending',
    {
      onRequest: [server.authenticateApiKeyOrJwt],
      schema: {
        tags: ['staff-orders'],
        security: [{ bearerAuth: [] }, { ApiKeyAuth: [] }],
        response: {
          200: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                id: { type: 'number' },
                orderDate: { type: 'string' },
                customerId: { type: 'number' },
                productId: { type: 'number' },
                quantity: { type: 'number' },
                desiredDeliveryDate: { type: 'string' },
                deliveryAddress: { type: 'string' },
                deliveryPhone: { type: 'string' },
                deliveryMessage: { type: 'string', nullable: true },
                status: { type: 'string' },
                createdBy: { type: 'string' },
                createdAt: { type: 'string' },
                updatedAt: { type: 'string' },
              },
            },
          },
        },
      },
    },
    async (request, reply) => {
      const orders = await orderRepo.findPendingOrders();
      reply.send(orders.map((o) => o.toJSON()));
    }
  );

  server.post(
    '/api/staff/orders/:id/confirm',
    {
      onRequest: [server.authenticateApiKeyOrJwt],
      schema: {
        tags: ['staff-orders'],
        security: [{ bearerAuth: [] }, { ApiKeyAuth: [] }],
        response: {
          200: {
            type: 'object',
            properties: {
              message: { type: 'string' },
              receivedOrder: {
                type: 'object',
                properties: {
                  id: { type: 'number' },
                  orderId: { type: 'number' },
                  receivedDate: { type: 'string' },
                  scheduledShipmentDate: { type: 'string' },
                  allocationStatus: { type: 'string' },
                  totalAmount: { type: 'number' },
                  totalTax: { type: 'number' },
                  createdBy: { type: 'string' },
                  createdAt: { type: 'string' },
                  updatedAt: { type: 'string' },
                },
              },
            },
          },
        },
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      const userId = (request.user as any).userId;

      try {
        const receivedOrder = await service.confirmOrder(Number(id), userId);
        reply.send({
          message: '受注を確認しました',
          receivedOrder: receivedOrder.toJSON(),
        });
      } catch (error: any) {
        reply.code(400).send({ error: error.message });
      }
    }
  );

  server.get(
    '/api/staff/orders/:id',
    {
      onRequest: [server.authenticateApiKeyOrJwt],
      schema: {
        tags: ['staff-orders'],
        security: [{ bearerAuth: [] }, { ApiKeyAuth: [] }],
        response: {
          200: {
            type: 'object',
            properties: {
              order: {
                type: 'object',
                properties: {
                  id: { type: 'number' },
                  orderDate: { type: 'string' },
                  customerId: { type: 'number' },
                  productId: { type: 'number' },
                  quantity: { type: 'number' },
                  desiredDeliveryDate: { type: 'string' },
                  deliveryAddress: { type: 'string' },
                  deliveryPhone: { type: 'string' },
                  deliveryMessage: { type: 'string', nullable: true },
                  status: { type: 'string' },
                  createdBy: { type: 'string' },
                  createdAt: { type: 'string' },
                  updatedAt: { type: 'string' },
                },
              },
              receivedOrder: {
                type: 'object',
                nullable: true,
                properties: {
                  id: { type: 'number' },
                  orderId: { type: 'number' },
                  receivedDate: { type: 'string' },
                  scheduledShipmentDate: { type: 'string' },
                  allocationStatus: { type: 'string' },
                  totalAmount: { type: 'number' },
                  totalTax: { type: 'number' },
                  createdBy: { type: 'string' },
                  createdAt: { type: 'string' },
                  updatedAt: { type: 'string' },
                },
              },
            },
          },
        },
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };

      const order = await orderRepo.findById(Number(id));
      if (!order) {
        return reply.code(404).send({ error: '注文が見つかりません' });
      }

      const receivedOrder = await receivedOrderRepo.findByOrderId(Number(id));

      reply.send({
        order: order.toJSON(),
        receivedOrder: receivedOrder?.toJSON() || null,
      });
    }
  );
}
