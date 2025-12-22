import { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify';
import { MRPUseCase } from '../../../application/port/in/MRPUseCase';

/**
 * MRP Controller (Input Adapter)
 */
export class MRPController {
  constructor(private readonly mrpUseCase: MRPUseCase) {}

  /**
   * ルートを登録
   */
  registerRoutes(app: FastifyInstance): void {
    // MRP実行
    app.post(
      '/mrp/execute',
      {
        schema: {
          tags: ['mrp'],
          summary: 'MRPの実行',
          description:
            '基準生産計画（MPS）を基にMRP（資材所要量計画）を実行し、製造オーダ、購買オーダ、所要情報を生成します',
          body: {
            type: 'object',
            required: ['mpsNumber'],
            properties: {
              mpsNumber: {
                type: 'string',
                description: 'MPS番号',
              },
            },
          },
          response: {
            200: {
              description: 'MRP実行成功',
              type: 'object',
              properties: {
                mpsNumber: { type: 'string' },
                generatedManufacturingOrders: { type: 'number' },
                generatedPurchaseOrders: { type: 'number' },
                generatedRequirements: { type: 'number' },
                executedAt: { type: 'string', format: 'date-time' },
              },
            },
            400: {
              description: 'リクエストエラー',
              type: 'object',
              properties: {
                error: { type: 'string' },
                message: { type: 'string' },
              },
            },
            404: {
              description: 'MPSが見つからない',
              type: 'object',
              properties: {
                error: { type: 'string' },
                message: { type: 'string' },
              },
            },
            500: {
              description: 'サーバーエラー',
              type: 'object',
              properties: {
                error: { type: 'string' },
                message: { type: 'string' },
              },
            },
          },
        },
      },
      this.executeMRP.bind(this)
    );
  }

  private async executeMRP(
    request: FastifyRequest<{
      Body: {
        mpsNumber: string;
      };
    }>,
    reply: FastifyReply
  ) {
    try {
      const { mpsNumber } = request.body;

      const result = await this.mrpUseCase.execute(mpsNumber);

      return reply.status(200).send(result);
    } catch (error) {
      if (error instanceof Error) {
        if (error.message.includes('not found')) {
          return reply.status(404).send({
            error: 'Not Found',
            message: error.message,
          });
        }

        if (error.message.includes('status must be')) {
          return reply.status(400).send({
            error: 'Bad Request',
            message: error.message,
          });
        }
      }

      return reply.status(500).send({
        error: 'Internal Server Error',
        message: error instanceof Error ? error.message : 'Unknown error',
      });
    }
  }
}
