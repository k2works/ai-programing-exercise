import { FastifyInstance } from 'fastify';
import { z } from 'zod';
import { ProductManagementService } from '../../application/product/ProductManagementService';
import { PrismaProductRepository } from '../../infrastructure/product/PrismaProductRepository';
import { PrismaProductCompositionRepository } from '../../infrastructure/product/PrismaProductCompositionRepository';

const createProductSchema = z.object({
  id: z.number(),
  code: z.string(),
  name: z.string(),
  salesPrice: z.number(),
});

const updateProductSchema = z.object({
  name: z.string(),
  salesPrice: z.number(),
});

const compositionSchema = z.object({
  items: z.array(
    z.object({
      itemId: z.number(),
      requiredQty: z.number(),
    })
  ),
});

export async function productRoutes(server: FastifyInstance) {
  const repository = new PrismaProductRepository((server as any).prisma);
  const compositionRepository = new PrismaProductCompositionRepository((server as any).prisma);
  const service = new ProductManagementService(repository, compositionRepository);

  server.post(
    '/api/products',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['products'],
        security: [{ bearerAuth: [] }],
        body: {
          type: 'object',
          required: ['id', 'code', 'name', 'salesPrice'],
          properties: {
            id: { type: 'number' },
            code: { type: 'string' },
            name: { type: 'string' },
            salesPrice: { type: 'number' },
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
    },
    async (request, reply) => {
      const { id, code, name, salesPrice } = createProductSchema.parse(request.body);
      const userId = (request.user as any).userId;

      await service.registerProduct(id, code, name, salesPrice, userId);
      reply.code(201).send({ message: '商品を登録しました' });
    }
  );

  server.get(
    '/api/products/:id',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['products'],
        security: [{ bearerAuth: [] }],
        response: {
          200: {
            type: 'object',
            properties: {
              id: { type: 'number' },
              code: { type: 'string' },
              name: { type: 'string' },
              salesPrice: { type: 'number' },
              salesStatus: { type: 'string' },
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
      const product = await repository.findById(Number(id));

      if (!product) {
        return reply.code(404).send({ error: '商品が見つかりません' });
      }

      reply.send(product.toJSON());
    }
  );

  server.get(
    '/api/products',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['products'],
        security: [{ bearerAuth: [] }],
        response: {
          200: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                id: { type: 'number' },
                code: { type: 'string' },
                name: { type: 'string' },
                salesPrice: { type: 'number' },
                salesStatus: { type: 'string' },
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
      const products = await repository.findOnSale();
      reply.send(products.map((p) => p.toJSON()));
    }
  );

  server.put(
    '/api/products/:id',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['products'],
        security: [{ bearerAuth: [] }],
        body: {
          type: 'object',
          required: ['name', 'salesPrice'],
          properties: {
            name: { type: 'string' },
            salesPrice: { type: 'number' },
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
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      const { name, salesPrice } = updateProductSchema.parse(request.body);

      await service.updateProduct(Number(id), name, salesPrice);
      reply.send({ message: '商品を更新しました' });
    }
  );

  server.patch(
    '/api/products/:id/stop-sales',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['products'],
        security: [{ bearerAuth: [] }],
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      await service.stopSales(Number(id));
      reply.send({ message: '販売を停止しました' });
    }
  );

  server.patch(
    '/api/products/:id/resume-sales',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['products'],
        security: [{ bearerAuth: [] }],
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      await service.resumeSales(Number(id));
      reply.send({ message: '販売を再開しました' });
    }
  );

  server.patch(
    '/api/products/:id/end-sales',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['products'],
        security: [{ bearerAuth: [] }],
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      await service.endSales(Number(id));
      reply.send({ message: '販売を終了しました' });
    }
  );

  server.post(
    '/api/products/:id/composition',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['products'],
        security: [{ bearerAuth: [] }],
        body: {
          type: 'object',
          required: ['items'],
          properties: {
            items: {
              type: 'array',
              items: {
                type: 'object',
                required: ['itemId', 'requiredQty'],
                properties: {
                  itemId: { type: 'number' },
                  requiredQty: { type: 'number' },
                },
              },
            },
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
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      const { items } = compositionSchema.parse(request.body);
      await service.associateItems(Number(id), items);
      reply.send({ message: '単品を関連付けました' });
    }
  );

  server.get(
    '/api/products/:id/composition',
    {
      onRequest: [server.authenticate],
      schema: {
        tags: ['products'],
        security: [{ bearerAuth: [] }],
        response: {
          200: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                productId: { type: 'number' },
                itemId: { type: 'number' },
                requiredQty: { type: 'number' },
              },
            },
          },
        },
      },
    },
    async (request, reply) => {
      const { id } = request.params as { id: string };
      const compositions = await compositionRepository.findByProductId(Number(id));
      reply.send(compositions);
    }
  );
}
