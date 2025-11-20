import { FastifyInstance } from 'fastify';
import { z } from 'zod';
import { OrderManagementService } from '../../application/order/OrderManagementService';
import { PrismaOrderRepository } from '../../infrastructure/order/PrismaOrderRepository';

const createOrderSchema = z.object({
  id: z.number(),
  orderDate: z.string(),
  customerId: z.number().optional(),
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
        body: {
          type: 'object',
          required: ['id', 'orderDate', 'productId', 'quantity', 'desiredDeliveryDate', 'deliveryAddress', 'deliveryPhone'],
          properties: {
            id: { type: 'number' },
            orderDate: { type: 'string', format: 'date' },
            customerId: { type: 'number', description: 'Optional - defaults to first customer if not provided' },
            productId: { type: 'number' },
            quantity: { type: 'number' },
            desiredDeliveryDate: { type: 'string', format: 'date' },
            deliveryAddress: { type: 'string' },
            deliveryPhone: { type: 'string' },
            deliveryMessage: { type: 'string', nullable: true },
          },
        },
      },
    },
    async (request, reply) => {
      const data = createOrderSchema.parse(request.body);
      const userId = (request.user as any).userId;

      try {
        // Get customer ID - use provided ID or get first customer for logged-in user
        let customerId = data.customerId;
        
        if (!customerId) {
          // Get first customer as default
          const customer = await (request.server as any).prisma.customer.findFirst();
          if (!customer) {
            return reply.code(400).send({ error: '顧客が見つかりません' });
          }
          customerId = customer.id;
        } else {
          // Verify customer exists
          const customer = await (request.server as any).prisma.customer.findUnique({
            where: { id: customerId },
          });

          if (!customer) {
            return reply.code(400).send({ 
              error: '顧客が見つかりません',
              customerId: customerId 
            });
          }
        }

        await service.createOrder(
          data.id,
          new Date(data.orderDate),
          customerId,
          data.productId,
          data.quantity,
          new Date(data.desiredDeliveryDate),
          data.deliveryAddress,
          data.deliveryPhone,
          data.deliveryMessage || null,
          userId
        );

        reply.code(201).send({ message: '注文を作成しました' });
      } catch (error: any) {
        request.log.error(error);
        reply.code(500).send({ 
          error: '注文の作成に失敗しました',
          details: error.message 
        });
      }
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
        body: {
          type: 'object',
          required: ['newDate'],
          properties: {
            newDate: { type: 'string', format: 'date' },
          },
        },
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
