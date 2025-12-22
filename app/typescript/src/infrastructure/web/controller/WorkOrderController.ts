import { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify';
import { WorkOrderUseCase } from '../../../application/port/in/WorkOrderUseCase';
import { WorkOrderStatus } from '../../../domain/model/work-order/WorkOrder';

/**
 * 作業指示 Controller (Input Adapter)
 */
export class WorkOrderController {
  constructor(private readonly workOrderUseCase: WorkOrderUseCase) {}

  /**
   * ルートを登録
   */
  registerRoutes(app: FastifyInstance): void {
    // 作業指示を作成
    app.post(
      '/work-orders',
      {
        schema: {
          tags: ['work-orders'],
          summary: '作業指示の作成',
          description: '新しい作業指示を作成します',
          body: {
            type: 'object',
            required: ['作業指示番号', '製造オーダ番号', '作業指示日', '品目コード', '指示数量', '明細'],
            properties: {
              作業指示番号: { type: 'string' },
              製造オーダ番号: { type: 'string' },
              作業指示日: { type: 'string', format: 'date' },
              品目コード: { type: 'string' },
              指示数量: { type: 'number' },
              ステータス: {
                type: 'string',
                enum: ['未着手', '作業中', '完了', '中断'],
              },
              完成数量: { type: 'number' },
              開始予定日: { type: 'string', format: 'date' },
              完了予定日: { type: 'string', format: 'date' },
              備考: { type: 'string' },
              明細: {
                type: 'array',
                items: {
                  type: 'object',
                  required: ['工順', '工程コード'],
                  properties: {
                    工順: { type: 'number' },
                    工程コード: { type: 'string' },
                    開始予定日時: { type: 'string', format: 'date-time' },
                    完了予定日時: { type: 'string', format: 'date-time' },
                  },
                },
              },
            },
          },
        },
      },
      this.createWorkOrder.bind(this)
    );

    // 作業指示番号で作業指示を取得
    app.get(
      '/work-orders/:workOrderNumber',
      {
        schema: {
          tags: ['work-orders'],
          summary: '作業指示の取得',
          description: '作業指示番号で作業指示を取得します',
          params: {
            type: 'object',
            properties: {
              workOrderNumber: { type: 'string', description: '作業指示番号' },
            },
            required: ['workOrderNumber'],
          },
        },
      },
      this.getWorkOrder.bind(this)
    );

    // 製造オーダ番号別の作業指示一覧を取得
    app.get(
      '/work-orders/production-order/:productionOrderNumber',
      {
        schema: {
          tags: ['work-orders'],
          summary: '製造オーダ別作業指示一覧の取得',
          description: '指定した製造オーダの作業指示一覧を取得します',
          params: {
            type: 'object',
            properties: {
              productionOrderNumber: { type: 'string', description: '製造オーダ番号' },
            },
            required: ['productionOrderNumber'],
          },
        },
      },
      this.getWorkOrdersByProductionOrder.bind(this)
    );

    // ステータス別の作業指示一覧を取得
    app.get(
      '/work-orders/status/:status',
      {
        schema: {
          tags: ['work-orders'],
          summary: 'ステータス別作業指示一覧の取得',
          description: '指定したステータスの作業指示一覧を取得します',
          params: {
            type: 'object',
            properties: {
              status: {
                type: 'string',
                enum: ['未着手', '作業中', '完了', '中断'],
                description: 'ステータス',
              },
            },
            required: ['status'],
          },
        },
      },
      this.getWorkOrdersByStatus.bind(this)
    );

    // 作業指示一覧を検索
    app.get(
      '/work-orders',
      {
        schema: {
          tags: ['work-orders'],
          summary: '作業指示一覧の検索',
          description: '作業指示を検索します。クエリパラメータで製造オーダやステータスを指定できます',
          querystring: {
            type: 'object',
            properties: {
              製造オーダ番号: { type: 'string', description: '製造オーダ番号' },
              ステータス: {
                type: 'string',
                enum: ['未着手', '作業中', '完了', '中断'],
                description: 'ステータス',
              },
            },
          },
        },
      },
      this.queryWorkOrders.bind(this)
    );

    // 作業指示ステータスを更新
    app.patch(
      '/work-orders/:workOrderNumber/status',
      {
        schema: {
          tags: ['work-orders'],
          summary: '作業指示ステータスの更新',
          description: '作業指示のステータスを更新します',
          params: {
            type: 'object',
            properties: {
              workOrderNumber: { type: 'string', description: '作業指示番号' },
            },
            required: ['workOrderNumber'],
          },
          body: {
            type: 'object',
            required: ['ステータス'],
            properties: {
              ステータス: {
                type: 'string',
                enum: ['未着手', '作業中', '完了', '中断'],
              },
            },
          },
        },
      },
      this.updateWorkOrderStatus.bind(this)
    );

    // 完成数量を更新
    app.patch(
      '/work-orders/:workOrderNumber/completed-quantity',
      {
        schema: {
          tags: ['work-orders'],
          summary: '完成数量の更新',
          description: '作業指示の完成数量を更新します',
          params: {
            type: 'object',
            properties: {
              workOrderNumber: { type: 'string', description: '作業指示番号' },
            },
            required: ['workOrderNumber'],
          },
          body: {
            type: 'object',
            required: ['完成数量'],
            properties: {
              完成数量: { type: 'number' },
            },
          },
        },
      },
      this.updateCompletedQuantity.bind(this)
    );
  }

  private async createWorkOrder(
    request: FastifyRequest<{
      Body: {
        作業指示番号: string;
        製造オーダ番号: string;
        作業指示日: string;
        品目コード: string;
        指示数量: number;
        ステータス?: string;
        完成数量?: number;
        開始予定日?: string;
        完了予定日?: string;
        備考?: string;
        明細: Array<{
          工順: number;
          工程コード: string;
          開始予定日時?: string;
          完了予定日時?: string;
        }>;
      };
    }>,
    reply: FastifyReply
  ) {
    const command = {
      作業指示番号: request.body.作業指示番号,
      製造オーダ番号: request.body.製造オーダ番号,
      作業指示日: new Date(request.body.作業指示日),
      品目コード: request.body.品目コード,
      指示数量: request.body.指示数量,
      ステータス: request.body.ステータス ? this.toStatus(request.body.ステータス) : undefined,
      完成数量: request.body.完成数量,
      開始予定日: request.body.開始予定日 ? new Date(request.body.開始予定日) : undefined,
      完了予定日: request.body.完了予定日 ? new Date(request.body.完了予定日) : undefined,
      備考: request.body.備考,
      明細: request.body.明細.map((detail) => ({
        工順: detail.工順,
        工程コード: detail.工程コード,
        開始予定日時: detail.開始予定日時 ? new Date(detail.開始予定日時) : undefined,
        完了予定日時: detail.完了予定日時 ? new Date(detail.完了予定日時) : undefined,
      })),
    };

    const workOrder = await this.workOrderUseCase.createWorkOrder(command);
    return reply.status(201).send(this.toResponse(workOrder));
  }

  private async getWorkOrder(
    request: FastifyRequest<{ Params: { workOrderNumber: string } }>,
    reply: FastifyReply
  ) {
    const { workOrderNumber } = request.params;
    const workOrder = await this.workOrderUseCase.getWorkOrder(workOrderNumber);

    if (!workOrder) {
      return reply.status(404).send({
        error: 'Not Found',
        message: `作業指示が見つかりません: 作業指示番号=${workOrderNumber}`,
      });
    }

    return reply.send(this.toResponse(workOrder));
  }

  private async getWorkOrdersByProductionOrder(
    request: FastifyRequest<{ Params: { productionOrderNumber: string } }>,
    reply: FastifyReply
  ) {
    const { productionOrderNumber } = request.params;
    const workOrders = await this.workOrderUseCase.getWorkOrdersByProductionOrder(
      productionOrderNumber
    );
    return reply.send(workOrders.map((wo) => this.toResponse(wo)));
  }

  private async getWorkOrdersByStatus(
    request: FastifyRequest<{ Params: { status: string } }>,
    reply: FastifyReply
  ) {
    const status = this.toStatus(request.params.status);
    const workOrders = await this.workOrderUseCase.getWorkOrdersByStatus(status);
    return reply.send(workOrders.map((wo) => this.toResponse(wo)));
  }

  private async queryWorkOrders(
    request: FastifyRequest<{
      Querystring: { 製造オーダ番号?: string; ステータス?: string };
    }>,
    reply: FastifyReply
  ) {
    const query = {
      製造オーダ番号: request.query.製造オーダ番号,
      ステータス: request.query.ステータス ? this.toStatus(request.query.ステータス) : undefined,
    };

    const workOrders = await this.workOrderUseCase.queryWorkOrders(query);
    return reply.send(workOrders.map((wo) => this.toResponse(wo)));
  }

  private async updateWorkOrderStatus(
    request: FastifyRequest<{
      Params: { workOrderNumber: string };
      Body: { ステータス: string };
    }>,
    reply: FastifyReply
  ) {
    const { workOrderNumber } = request.params;
    const status = this.toStatus(request.body.ステータス);

    await this.workOrderUseCase.updateWorkOrderStatus(workOrderNumber, status);
    return reply.status(204).send();
  }

  private async updateCompletedQuantity(
    request: FastifyRequest<{
      Params: { workOrderNumber: string };
      Body: { 完成数量: number };
    }>,
    reply: FastifyReply
  ) {
    const { workOrderNumber } = request.params;
    const { 完成数量 } = request.body;

    await this.workOrderUseCase.updateCompletedQuantity(workOrderNumber, 完成数量);
    return reply.status(204).send();
  }

  /**
   * ドメインモデルをレスポンスオブジェクトに変換
   * (getterを含むすべてのプロパティを含める)
   */
  private toResponse(workOrder: any) {
    return {
      作業指示番号: workOrder.作業指示番号,
      製造オーダ番号: workOrder.製造オーダ番号,
      作業指示日: workOrder.作業指示日,
      品目コード: workOrder.品目コード,
      指示数量: workOrder.指示数量,
      完成数量: workOrder.完成数量,
      ステータス: workOrder.ステータス,
      開始予定日: workOrder.開始予定日,
      完了予定日: workOrder.完了予定日,
      備考: workOrder.備考,
      明細: workOrder.明細.map((detail: any) => ({
        工順: detail.工順,
        工程コード: detail.工程コード,
        開始予定日時: detail.開始予定日時,
        完了予定日時: detail.完了予定日時,
      })),
      未完成数量: workOrder.未完成数量, // getter
      進捗率: workOrder.進捗率, // getter
      完了済み: workOrder.完了済み, // getter
    };
  }

  /**
   * 文字列をWorkOrderStatusに変換
   */
  private toStatus(status: string): WorkOrderStatus {
    const statusMap: Record<string, WorkOrderStatus> = {
      未着手: WorkOrderStatus.NOT_STARTED,
      作業中: WorkOrderStatus.IN_PROGRESS,
      完了: WorkOrderStatus.COMPLETED,
      中断: WorkOrderStatus.SUSPENDED,
    };
    return statusMap[status];
  }
}
