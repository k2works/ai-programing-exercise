import { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify';
import { z } from 'zod';
import { BomUseCase } from '../../../application/port/in/BomUseCase';

// リクエストスキーマ
const CreateBomSchema = z.object({
  親品目コード: z.string().min(1).max(20),
  子品目コード: z.string().min(1).max(20),
  適用開始日: z.string().optional().transform(str => str ? new Date(str) : undefined),
  基準量: z.number().positive(),
  必要量: z.number().positive(),
  適用停止日: z.string().optional().transform(str => str ? new Date(str) : undefined),
  不良率: z.number().min(0).max(100).optional(),
  工順: z.number().int().optional(),
});

const UpdateBomSchema = z.object({
  基準量: z.number().positive().optional(),
  必要量: z.number().positive().optional(),
  適用停止日: z.string().optional().transform(str => str ? new Date(str) : undefined),
  不良率: z.number().min(0).max(100).optional(),
  工順: z.number().int().optional(),
});

/**
 * BOM Controller (Input Adapter)
 */
export class BomController {
  constructor(private readonly bomUseCase: BomUseCase) {}

  /**
   * ルートを登録
   */
  registerRoutes(app: FastifyInstance): void {
    // BOMを登録
    app.post(
      '/bom',
      {
        schema: {
          tags: ['bom'],
          summary: 'BOMの登録',
          description: 'BOM（部品構成表）を登録します',
          body: {
            type: 'object',
            required: ['親品目コード', '子品目コード', '基準量', '必要量'],
            properties: {
              親品目コード: { type: 'string' },
              子品目コード: { type: 'string' },
              適用開始日: { type: 'string', format: 'date' },
              基準量: { type: 'number' },
              必要量: { type: 'number' },
              適用停止日: { type: 'string', format: 'date' },
              不良率: { type: 'number' },
              工順: { type: 'number' },
            },
          },
        },
      },
      this.createBom.bind(this)
    );

    // 親品目の子品目一覧を取得
    app.get(
      '/bom/:parentItemCode/children',
      {
        schema: {
          tags: ['bom'],
          summary: '子品目一覧の取得',
          description: '指定した親品目の子品目一覧を取得します',
          params: {
            type: 'object',
            properties: {
              parentItemCode: { type: 'string', description: '親品目コード' },
            },
            required: ['parentItemCode'],
          },
        },
      },
      this.getChildren.bind(this)
    );

    // BOMを展開（部品展開）
    app.get(
      '/bom/:itemCode/explode',
      {
        schema: {
          tags: ['bom'],
          summary: 'BOM展開（部品展開）',
          description: '指定した品目のBOMを全階層展開します',
          params: {
            type: 'object',
            properties: {
              itemCode: { type: 'string', description: '品目コード' },
            },
            required: ['itemCode'],
          },
          querystring: {
            type: 'object',
            properties: {
              quantity: { type: 'number', description: '展開数量', default: 1 },
            },
          },
        },
      },
      this.explodeBom.bind(this)
    );

    // 使用先照会（逆引き）
    app.get(
      '/bom/:childItemCode/where-used',
      {
        schema: {
          tags: ['bom'],
          summary: '使用先照会',
          description: '指定した子品目がどの親品目で使用されているかを照会します',
          params: {
            type: 'object',
            properties: {
              childItemCode: { type: 'string', description: '子品目コード' },
            },
            required: ['childItemCode'],
          },
        },
      },
      this.findWhereUsed.bind(this)
    );

    // BOMを更新
    app.put(
      '/bom/:parentItemCode/:childItemCode/:effectiveFrom',
      {
        schema: {
          tags: ['bom'],
          summary: 'BOMの更新',
        },
      },
      this.updateBom.bind(this)
    );

    // BOMを削除
    app.delete(
      '/bom/:parentItemCode/:childItemCode/:effectiveFrom',
      {
        schema: {
          tags: ['bom'],
          summary: 'BOMの削除',
        },
      },
      this.deleteBom.bind(this)
    );
  }

  private async createBom(request: FastifyRequest, reply: FastifyReply) {
    const validated = CreateBomSchema.parse(request.body);
    const bom = await this.bomUseCase.createBom(validated);
    return reply.status(201).send(bom);
  }

  private async getChildren(
    request: FastifyRequest<{ Params: { parentItemCode: string } }>,
    reply: FastifyReply
  ) {
    const { parentItemCode } = request.params;
    const children = await this.bomUseCase.getChildren(parentItemCode);
    return reply.send(children);
  }

  private async explodeBom(
    request: FastifyRequest<{
      Params: { itemCode: string };
      Querystring: { quantity?: string };
    }>,
    reply: FastifyReply
  ) {
    const { itemCode } = request.params;
    const quantity = request.query.quantity ? parseFloat(request.query.quantity) : 1;
    const explosion = await this.bomUseCase.explodeBom(itemCode, quantity);
    return reply.send(explosion);
  }

  private async findWhereUsed(
    request: FastifyRequest<{ Params: { childItemCode: string } }>,
    reply: FastifyReply
  ) {
    const { childItemCode } = request.params;
    const parents = await this.bomUseCase.findWherUsed(childItemCode);
    return reply.send(parents);
  }

  private async updateBom(
    request: FastifyRequest<{
      Params: {
        parentItemCode: string;
        childItemCode: string;
        effectiveFrom: string;
      };
    }>,
    reply: FastifyReply
  ) {
    const { parentItemCode, childItemCode, effectiveFrom } = request.params;
    const validated = UpdateBomSchema.parse(request.body);
    const bom = await this.bomUseCase.updateBom({
      親品目コード: parentItemCode,
      子品目コード: childItemCode,
      適用開始日: new Date(effectiveFrom),
      ...validated,
    });
    return reply.send(bom);
  }

  private async deleteBom(
    request: FastifyRequest<{
      Params: {
        parentItemCode: string;
        childItemCode: string;
        effectiveFrom: string;
      };
    }>,
    reply: FastifyReply
  ) {
    const { parentItemCode, childItemCode, effectiveFrom } = request.params;
    await this.bomUseCase.deleteBom(
      parentItemCode,
      childItemCode,
      new Date(effectiveFrom)
    );
    return reply.status(204).send();
  }
}
