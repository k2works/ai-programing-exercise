import { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify';
import { PurchaseOrderUseCase } from '../../../application/port/in/PurchaseOrderUseCase';
import { PurchaseOrderStatus } from '../../../domain/model/purchase/PurchaseOrder';

/**
 * 発注 Controller (Input Adapter)
 */
export class PurchaseOrderController {
  constructor(private readonly purchaseOrderUseCase: PurchaseOrderUseCase) {}

  /**
   * ルートを登録
   */
  registerRoutes(app: FastifyInstance): void {
    // 発注を作成
    app.post(
      '/purchase-orders',
      {
        schema: {
          tags: ['purchase-orders'],
          summary: '発注の作成',
          description: '新しい発注を作成します',
          body: {
            type: 'object',
            required: ['発注番号', '発注日', '取引先コード', '明細'],
            properties: {
              発注番号: { type: 'string' },
              発注日: { type: 'string', format: 'date' },
              取引先コード: { type: 'string' },
              ステータス: {
                type: 'string',
                enum: [
                  '作成中',
                  '発注済',
                  '一部入荷',
                  '入荷完了',
                  '検収完了',
                  '取消',
                ],
              },
              発注担当者コード: { type: 'string' },
              発注部門コード: { type: 'string' },
              備考: { type: 'string' },
              明細: {
                type: 'array',
                items: {
                  type: 'object',
                  required: ['行番号', '品目コード', '受入予定日', '発注単価', '発注数量'],
                  properties: {
                    行番号: { type: 'number' },
                    品目コード: { type: 'string' },
                    受入予定日: { type: 'string', format: 'date' },
                    発注単価: { type: 'number' },
                    発注数量: { type: 'number' },
                    製造オーダ番号: { type: 'string' },
                    納入場所コード: { type: 'string' },
                    回答納期: { type: 'string', format: 'date' },
                    入荷済数量: { type: 'number' },
                    検査済数量: { type: 'number' },
                    検収済数量: { type: 'number' },
                    消費税金額: { type: 'number' },
                    完了フラグ: { type: 'boolean' },
                    明細備考: { type: 'string' },
                  },
                },
              },
            },
          },
        },
      },
      this.createPurchaseOrder.bind(this)
    );

    // 発注番号で発注を取得
    app.get(
      '/purchase-orders/:orderNumber',
      {
        schema: {
          tags: ['purchase-orders'],
          summary: '発注の取得',
          description: '発注番号で発注を取得します',
          params: {
            type: 'object',
            properties: {
              orderNumber: { type: 'string', description: '発注番号' },
            },
            required: ['orderNumber'],
          },
        },
      },
      this.getPurchaseOrder.bind(this)
    );

    // 取引先別の発注一覧を取得
    app.get(
      '/purchase-orders/supplier/:supplierCode',
      {
        schema: {
          tags: ['purchase-orders'],
          summary: '取引先別発注一覧の取得',
          description: '指定した取引先の発注一覧を取得します',
          params: {
            type: 'object',
            properties: {
              supplierCode: { type: 'string', description: '取引先コード' },
            },
            required: ['supplierCode'],
          },
        },
      },
      this.getPurchaseOrdersBySupplier.bind(this)
    );

    // ステータス別の発注一覧を取得
    app.get(
      '/purchase-orders/status/:status',
      {
        schema: {
          tags: ['purchase-orders'],
          summary: 'ステータス別発注一覧の取得',
          description: '指定したステータスの発注一覧を取得します',
          params: {
            type: 'object',
            properties: {
              status: {
                type: 'string',
                enum: [
                  '作成中',
                  '発注済',
                  '一部入荷',
                  '入荷完了',
                  '検収完了',
                  '取消',
                ],
                description: 'ステータス',
              },
            },
            required: ['status'],
          },
        },
      },
      this.getPurchaseOrdersByStatus.bind(this)
    );

    // 発注一覧を検索
    app.get(
      '/purchase-orders',
      {
        schema: {
          tags: ['purchase-orders'],
          summary: '発注一覧の検索',
          description: '発注を検索します。クエリパラメータで取引先やステータスを指定できます',
          querystring: {
            type: 'object',
            properties: {
              取引先コード: { type: 'string', description: '取引先コード' },
              ステータス: {
                type: 'string',
                enum: [
                  '作成中',
                  '発注済',
                  '一部入荷',
                  '入荷完了',
                  '検収完了',
                  '取消',
                ],
                description: 'ステータス',
              },
            },
          },
        },
      },
      this.queryPurchaseOrders.bind(this)
    );

    // 発注ステータスを更新
    app.patch(
      '/purchase-orders/:orderNumber/status',
      {
        schema: {
          tags: ['purchase-orders'],
          summary: '発注ステータスの更新',
          description: '発注のステータスを更新します',
          params: {
            type: 'object',
            properties: {
              orderNumber: { type: 'string', description: '発注番号' },
            },
            required: ['orderNumber'],
          },
          body: {
            type: 'object',
            required: ['ステータス'],
            properties: {
              ステータス: {
                type: 'string',
                enum: [
                  '作成中',
                  '発注済',
                  '一部入荷',
                  '入荷完了',
                  '検収完了',
                  '取消',
                ],
              },
            },
          },
        },
      },
      this.updatePurchaseOrderStatus.bind(this)
    );
  }

  private async createPurchaseOrder(
    request: FastifyRequest<{
      Body: {
        発注番号: string;
        発注日: string;
        取引先コード: string;
        ステータス?: string;
        発注担当者コード?: string;
        発注部門コード?: string;
        備考?: string;
        明細: Array<{
          行番号: number;
          品目コード: string;
          受入予定日: string;
          発注単価: number;
          発注数量: number;
          製造オーダ番号?: string;
          納入場所コード?: string;
          回答納期?: string;
          入荷済数量?: number;
          検査済数量?: number;
          検収済数量?: number;
          消費税金額?: number;
          完了フラグ?: boolean;
          明細備考?: string;
        }>;
      };
    }>,
    reply: FastifyReply
  ) {
    const command = {
      発注番号: request.body.発注番号,
      発注日: new Date(request.body.発注日),
      取引先コード: request.body.取引先コード,
      ステータス: request.body.ステータス
        ? this.toStatus(request.body.ステータス)
        : undefined,
      発注担当者コード: request.body.発注担当者コード,
      発注部門コード: request.body.発注部門コード,
      備考: request.body.備考,
      明細: request.body.明細.map((detail) => ({
        行番号: detail.行番号,
        品目コード: detail.品目コード,
        受入予定日: new Date(detail.受入予定日),
        発注単価: detail.発注単価,
        発注数量: detail.発注数量,
        製造オーダ番号: detail.製造オーダ番号,
        納入場所コード: detail.納入場所コード,
        回答納期: detail.回答納期 ? new Date(detail.回答納期) : undefined,
        入荷済数量: detail.入荷済数量,
        検査済数量: detail.検査済数量,
        検収済数量: detail.検収済数量,
        消費税金額: detail.消費税金額,
        完了フラグ: detail.完了フラグ,
        明細備考: detail.明細備考,
      })),
    };

    const purchaseOrder = await this.purchaseOrderUseCase.createPurchaseOrder(command);
    return reply.status(201).send(this.toResponse(purchaseOrder));
  }

  private async getPurchaseOrder(
    request: FastifyRequest<{ Params: { orderNumber: string } }>,
    reply: FastifyReply
  ) {
    const { orderNumber } = request.params;
    const purchaseOrder = await this.purchaseOrderUseCase.getPurchaseOrder(orderNumber);

    if (!purchaseOrder) {
      return reply.status(404).send({
        error: 'Not Found',
        message: `発注が見つかりません: 発注番号=${orderNumber}`,
      });
    }

    return reply.send(this.toResponse(purchaseOrder));
  }

  private async getPurchaseOrdersBySupplier(
    request: FastifyRequest<{ Params: { supplierCode: string } }>,
    reply: FastifyReply
  ) {
    const { supplierCode } = request.params;
    const purchaseOrders = await this.purchaseOrderUseCase.getPurchaseOrdersBySupplier(
      supplierCode
    );
    return reply.send(purchaseOrders.map((po) => this.toResponse(po)));
  }

  private async getPurchaseOrdersByStatus(
    request: FastifyRequest<{ Params: { status: string } }>,
    reply: FastifyReply
  ) {
    const status = this.toStatus(request.params.status);
    const purchaseOrders = await this.purchaseOrderUseCase.getPurchaseOrdersByStatus(status);
    return reply.send(purchaseOrders.map((po) => this.toResponse(po)));
  }

  private async queryPurchaseOrders(
    request: FastifyRequest<{
      Querystring: { 取引先コード?: string; ステータス?: string };
    }>,
    reply: FastifyReply
  ) {
    const query = {
      取引先コード: request.query.取引先コード,
      ステータス: request.query.ステータス
        ? this.toStatus(request.query.ステータス)
        : undefined,
    };

    const purchaseOrders = await this.purchaseOrderUseCase.queryPurchaseOrders(query);
    return reply.send(purchaseOrders.map((po) => this.toResponse(po)));
  }

  private async updatePurchaseOrderStatus(
    request: FastifyRequest<{
      Params: { orderNumber: string };
      Body: { ステータス: string };
    }>,
    reply: FastifyReply
  ) {
    const { orderNumber } = request.params;
    const status = this.toStatus(request.body.ステータス);

    await this.purchaseOrderUseCase.updatePurchaseOrderStatus(orderNumber, status);
    return reply.status(204).send();
  }

  /**
   * ドメインモデルをレスポンスオブジェクトに変換
   * (getterを含むすべてのプロパティを含める)
   */
  private toResponse(purchaseOrder: any) {
    return {
      発注番号: purchaseOrder.発注番号,
      発注日: purchaseOrder.発注日,
      取引先コード: purchaseOrder.取引先コード,
      ステータス: purchaseOrder.ステータス,
      発注担当者コード: purchaseOrder.発注担当者コード,
      発注部門コード: purchaseOrder.発注部門コード,
      備考: purchaseOrder.備考,
      明細: purchaseOrder.明細.map((detail: any) => ({
        行番号: detail.行番号,
        品目コード: detail.品目コード,
        受入予定日: detail.受入予定日,
        発注単価: detail.発注単価,
        発注数量: detail.発注数量,
        発注金額: detail.発注金額,
        製造オーダ番号: detail.製造オーダ番号,
        納入場所コード: detail.納入場所コード,
        回答納期: detail.回答納期,
        入荷済数量: detail.入荷済数量,
        検査済数量: detail.検査済数量,
        検収済数量: detail.検収済数量,
        消費税金額: detail.消費税金額,
        完了フラグ: detail.完了フラグ,
        明細備考: detail.明細備考,
        未入荷数量: detail.未入荷数量, // getter
      })),
      発注合計金額: purchaseOrder.発注合計金額, // getter
      消費税合計額: purchaseOrder.消費税合計額, // getter
      発注総額: purchaseOrder.発注総額, // getter
    };
  }

  /**
   * 文字列をPurchaseOrderStatusに変換
   */
  private toStatus(status: string): PurchaseOrderStatus {
    const statusMap: Record<string, PurchaseOrderStatus> = {
      作成中: PurchaseOrderStatus.DRAFT,
      発注済: PurchaseOrderStatus.ORDERED,
      一部入荷: PurchaseOrderStatus.PARTIALLY_RECEIVED,
      入荷完了: PurchaseOrderStatus.FULLY_RECEIVED,
      検収完了: PurchaseOrderStatus.INSPECTED,
      取消: PurchaseOrderStatus.CANCELLED,
    };
    return statusMap[status];
  }
}
