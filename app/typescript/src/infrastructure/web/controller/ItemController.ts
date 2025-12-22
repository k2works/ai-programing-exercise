import { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify';
import { z } from 'zod';
import { ItemUseCase } from '../../../application/port/in/ItemUseCase';

// リクエストスキーマ
const CreateItemSchema = z.object({
  品目コード: z.string().min(1).max(20),
  品名: z.string().min(1).max(100),
  品目区分: z.enum(['PRODUCT', 'SEMI_PRODUCT', 'INTERMEDIATE', 'PART', 'MATERIAL', 'RAW_MATERIAL', 'SUPPLY']),
  品目グループコード: z.string().optional(),
  単位コード: z.string().optional(),
  場所コード: z.string().optional(),
  リードタイム: z.number().int().min(0).optional(),
  安全在庫数: z.number().int().min(0).optional(),
});

const UpdateItemSchema = z.object({
  品名: z.string().min(1).max(100).optional(),
  品目区分: z.enum(['PRODUCT', 'SEMI_PRODUCT', 'INTERMEDIATE', 'PART', 'MATERIAL', 'RAW_MATERIAL', 'SUPPLY']).optional(),
  リードタイム: z.number().int().min(0).optional(),
  安全在庫数: z.number().int().min(0).optional(),
});

/**
 * 品目 Controller（Input Adapter）
 */
export class ItemController {
  constructor(private readonly itemUseCase: ItemUseCase) {}

  /**
   * ルートを登録
   */
  registerRoutes(app: FastifyInstance): void {
    // 品目一覧を取得
    app.get(
      '/items',
      {
        schema: {
          tags: ['items'],
          summary: '品目一覧の取得',
          description: 'すべての品目を取得します。category クエリパラメータでフィルタリング可能です。',
          querystring: {
            type: 'object',
            properties: {
              category: { type: 'string', description: '品目区分でフィルタリング' },
            },
          },
          response: {
            200: {
              description: '成功',
              type: 'array',
              items: {
                type: 'object',
                properties: {
                  品目コード: { type: 'string' },
                  品名: { type: 'string' },
                  品目区分: { type: 'string' },
                  リードタイム: { type: 'number' },
                  安全在庫数: { type: 'number' },
                },
              },
            },
          },
        },
      },
      this.getAllItems.bind(this)
    );

    // 品目を取得
    app.get(
      '/items/:itemCode',
      {
        schema: {
          tags: ['items'],
          summary: '品目の取得',
          params: {
            type: 'object',
            properties: {
              itemCode: { type: 'string', description: '品目コード' },
            },
            required: ['itemCode'],
          },
        },
      },
      this.getItemByCode.bind(this)
    );

    // 品目を登録
    app.post(
      '/items',
      {
        schema: {
          tags: ['items'],
          summary: '品目の登録',
          body: {
            type: 'object',
            required: ['品目コード', '品名', '品目区分'],
            properties: {
              品目コード: { type: 'string' },
              品名: { type: 'string' },
              品目区分: { type: 'string' },
              品目グループコード: { type: 'string' },
              単位コード: { type: 'string' },
              場所コード: { type: 'string' },
              リードタイム: { type: 'number' },
              安全在庫数: { type: 'number' },
            },
          },
        },
      },
      this.createItem.bind(this)
    );

    // 品目を更新
    app.put('/items/:itemCode', {
      schema: {
        tags: ['items'],
        summary: '品目の更新',
      },
    }, this.updateItem.bind(this));

    // 品目を削除
    app.delete('/items/:itemCode', {
      schema: {
        tags: ['items'],
        summary: '品目の削除',
      },
    }, this.deleteItem.bind(this));
  }

  private async getAllItems(
    request: FastifyRequest<{ Querystring: { category?: string } }>,
    reply: FastifyReply
  ) {
    const { category } = request.query;

    if (category) {
      const items = await this.itemUseCase.getItemsByCategory(category);
      return reply.send(items);
    }

    const items = await this.itemUseCase.getAllItems();
    return reply.send(items);
  }

  private async getItemByCode(
    request: FastifyRequest<{ Params: { itemCode: string } }>,
    reply: FastifyReply
  ) {
    const { itemCode } = request.params;
    const item = await this.itemUseCase.getItemByCode(itemCode);
    return reply.send(item);
  }

  private async createItem(request: FastifyRequest, reply: FastifyReply) {
    const validated = CreateItemSchema.parse(request.body);
    const item = await this.itemUseCase.createItem(validated);
    return reply.status(201).send(item);
  }

  private async updateItem(
    request: FastifyRequest<{ Params: { itemCode: string } }>,
    reply: FastifyReply
  ) {
    const { itemCode } = request.params;
    const validated = UpdateItemSchema.parse(request.body);
    const item = await this.itemUseCase.updateItem({
      品目コード: itemCode,
      ...validated,
    });
    return reply.send(item);
  }

  private async deleteItem(
    request: FastifyRequest<{ Params: { itemCode: string } }>,
    reply: FastifyReply
  ) {
    const { itemCode } = request.params;
    await this.itemUseCase.deleteItem(itemCode);
    return reply.status(204).send();
  }
}
