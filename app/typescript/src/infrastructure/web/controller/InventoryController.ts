import { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify';
import { InventoryUseCase } from '../../../application/port/in/InventoryUseCase';

/**
 * 在庫 Controller (Input Adapter)
 */
export class InventoryController {
  constructor(private readonly inventoryUseCase: InventoryUseCase) {}

  /**
   * ルートを登録
   */
  registerRoutes(app: FastifyInstance): void {
    // 在庫一覧を照会
    app.get(
      '/inventory',
      {
        schema: {
          tags: ['inventory'],
          summary: '在庫一覧の照会',
          description: '在庫情報を照会します。クエリパラメータで場所や品目を指定できます',
          querystring: {
            type: 'object',
            properties: {
              場所コード: { type: 'string', description: '場所コード' },
              品目コード: { type: 'string', description: '品目コード' },
            },
          },
        },
      },
      this.queryInventories.bind(this)
    );

    // 場所・品目別の在庫を照会
    app.get(
      '/inventory/:locationCode/:itemCode',
      {
        schema: {
          tags: ['inventory'],
          summary: '在庫の照会',
          description: '指定した場所・品目の在庫情報を照会します',
          params: {
            type: 'object',
            properties: {
              locationCode: { type: 'string', description: '場所コード' },
              itemCode: { type: 'string', description: '品目コード' },
            },
            required: ['locationCode', 'itemCode'],
          },
        },
      },
      this.getInventory.bind(this)
    );

    // 場所別の在庫一覧を照会
    app.get(
      '/inventory/location/:locationCode',
      {
        schema: {
          tags: ['inventory'],
          summary: '場所別在庫一覧の照会',
          description: '指定した場所の在庫一覧を照会します',
          params: {
            type: 'object',
            properties: {
              locationCode: { type: 'string', description: '場所コード' },
            },
            required: ['locationCode'],
          },
        },
      },
      this.getInventoriesByLocation.bind(this)
    );

    // 品目別の在庫一覧を照会（全場所）
    app.get(
      '/inventory/item/:itemCode',
      {
        schema: {
          tags: ['inventory'],
          summary: '品目別在庫一覧の照会',
          description: '指定した品目の在庫一覧（全場所）を照会します',
          params: {
            type: 'object',
            properties: {
              itemCode: { type: 'string', description: '品目コード' },
            },
            required: ['itemCode'],
          },
        },
      },
      this.getInventoriesByItem.bind(this)
    );
  }

  private async queryInventories(
    request: FastifyRequest<{
      Querystring: { 場所コード?: string; 品目コード?: string };
    }>,
    reply: FastifyReply
  ) {
    const inventories = await this.inventoryUseCase.queryInventories(request.query);
    return reply.send(inventories.map((inv) => this.toResponse(inv)));
  }

  private async getInventory(
    request: FastifyRequest<{
      Params: { locationCode: string; itemCode: string };
    }>,
    reply: FastifyReply
  ) {
    const { locationCode, itemCode } = request.params;
    const inventory = await this.inventoryUseCase.getInventory(locationCode, itemCode);

    if (!inventory) {
      return reply.status(404).send({
        error: 'Not Found',
        message: `在庫が見つかりません: 場所=${locationCode}, 品目=${itemCode}`,
      });
    }

    return reply.send(this.toResponse(inventory));
  }

  private async getInventoriesByLocation(
    request: FastifyRequest<{ Params: { locationCode: string } }>,
    reply: FastifyReply
  ) {
    const { locationCode } = request.params;
    const inventories = await this.inventoryUseCase.getInventoriesByLocation(locationCode);
    return reply.send(inventories.map((inv) => this.toResponse(inv)));
  }

  private async getInventoriesByItem(
    request: FastifyRequest<{ Params: { itemCode: string } }>,
    reply: FastifyReply
  ) {
    const { itemCode } = request.params;
    const inventories = await this.inventoryUseCase.getInventoriesByItem(itemCode);
    return reply.send(inventories.map((inv) => this.toResponse(inv)));
  }

  /**
   * ドメインモデルをレスポンスオブジェクトに変換
   * (getterを含むすべてのプロパティを含める)
   */
  private toResponse(inventory: any) {
    return {
      場所コード: inventory.場所コード,
      品目コード: inventory.品目コード,
      在庫数量: inventory.在庫数量,
      合格数: inventory.合格数,
      不良数: inventory.不良数,
      未検査数: inventory.未検査数,
      更新日時: inventory.更新日時,
      利用可能数量: inventory.利用可能数量,
      合格率: inventory.合格率,
    };
  }
}
