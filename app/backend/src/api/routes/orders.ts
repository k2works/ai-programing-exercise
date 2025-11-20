import { FastifyInstance } from 'fastify';
import { z } from 'zod';
import { OrderManagementService } from '../../application/order/OrderManagementService';
import { PrismaOrderRepository } from '../../infrastructure/order/PrismaOrderRepository';

const createOrderSchema = z.object({
  id: z.number(),
  orderDate: z.string(),
  customerId: z.number(),
  productId: z.number(),
  quantity: z.number(),
  desiredDeliveryDate: z.string(),
  deliveryAddress: z.string(),
  deliveryPhone: z.string(),
  deliveryMessage: z.string().nullable().optional(),
});

const changeDeliveryDateSchema = z.object({
  newDate: z.string(),
});

export async function orderRoutes(server: FastifyInstance) {
  const repository = new PrismaOrderRepository((server as any).prisma);
  const service = new OrderManagementService(repository);

  server.post(
    '/api/orders',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['orders'],
        security: [{ bearerAuth: [] }],
      },
    },
    async (request, reply) => {
      const data = createOrderSchema.parse(request.body);
      const userId = (request.user as any).userId;

      await service.createOrder(
        data.id,
        new Date(data.orderDate),
        data.customerId,
        data.productId,
        data.quantity,
        new Date(data.desiredDeliveryDate),
        data.deliveryAddress,
        data.deliveryPhone,
        data.deliveryMessage || null,
        userId
      );

      reply.code(201).send({ message: '注文を作成しました' });
    }
  );

  server.get(
    '/api/orders/:id',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['orders'],
        security: [{ bearerAuth: [] }],
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      const order = await repository.findById(Number(id));

      if (!order) {
        return reply.code(404).send({ error: '注文が見つかりません' });
      }

      reply.send(order.toJSON());
    }
  );

  server.patch(
    '/api/orders/:id/delivery-date',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['orders'],
        security: [{ bearerAuth: [] }],
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      const { newDate } = changeDeliveryDateSchema.parse(request.body);

      await service.changeDeliveryDate(Number(id), new Date(newDate));
      reply.send({ message: '配送日を変更しました' });
    }
  );

  server.patch(
    '/api/orders/:id/cancel',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['orders'],
        security: [{ bearerAuth: [] }],
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };

      await service.cancelOrder(Number(id));
      reply.send({ message: '注文をキャンセルしました' });
    }
  );

  server.get(
    '/api/customers/:customerId/orders',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['orders'],
        security: [{ bearerAuth: [] }],
      },
    },
    async (request, reply) => {
      const { customerId } = request.params as { customerId: string };
      const orders = await service.getCustomerOrders(Number(customerId));

      reply.send(orders.map((o) => o.toJSON()));
    }
  );
}
