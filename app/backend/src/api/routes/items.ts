import { FastifyInstance } from 'fastify';

export async function itemRoutes(server: FastifyInstance) {
  server.get(
    '/api/items',
    {
      onRequest: [server.authenticateApiKeyOrJwt],
      schema: {
        tags: ['items'],
        security: [{ bearerAuth: [] }, { ApiKeyAuth: [] }],
        response: {
          200: {
            type: 'array',
            items: {
              type: 'object',
              properties: {
                id: { type: 'number' },
                code: { type: 'string' },
                name: { type: 'string' },
              },
            },
          },
        },
      },
    },
    async (request, reply) => {
      const items = await (server as any).prisma.item.findMany({
        select: {
          id: true,
          code: true,
          name: true,
        },
      });
      reply.send(items);
    }
  );
}
