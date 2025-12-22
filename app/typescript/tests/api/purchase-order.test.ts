import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { FastifyInstance } from 'fastify';
import { TestDatabase } from '../../src/test-setup/database';
import { buildApp } from '../../src/infrastructure/web/application';

describe('Purchase Order API', () => {
  let testDb: TestDatabase;
  let app: FastifyInstance;

  beforeAll(async () => {
    testDb = new TestDatabase();
    await testDb.start();

    app = await buildApp(testDb.prisma!);
  }, 60000);

  afterAll(async () => {
    await app.close();
    await testDb.stop();
  });

  beforeEach(async () => {
    await testDb.cleanup();

    // テスト用データを作成
    // 取引先マスタ (Prisma enum mapping issue を回避するため生SQLを使用)
    await testDb.prisma!.$executeRaw`
      INSERT INTO suppliers (supplier_code, supplier_name, supplier_type)
      VALUES
        ('SUP-001', 'サプライヤーA', 'VENDOR'::supplier_type),
        ('SUP-002', 'サプライヤーB', 'VENDOR'::supplier_type)
    `;

    // 品目マスタ
    await testDb.prisma!.item.createMany({
      data: [
        {
          itemCode: 'PART-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '部品A',
          itemCategory: 'PART',
        },
        {
          itemCode: 'PART-002',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '部品B',
          itemCategory: 'PART',
        },
        {
          itemCode: 'MAT-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '材料A',
          itemCategory: 'MATERIAL',
        },
      ],
    });
  });

  describe('POST /purchase-orders', () => {
    it('発注を作成できる', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-001',
          発注日: '2025-01-15',
          取引先コード: 'SUP-001',
          ステータス: '作成中',
          発注担当者コード: 'USER-001',
          発注部門コード: 'DEPT-001',
          備考: 'テスト発注',
          明細: [
            {
              行番号: 1,
              品目コード: 'PART-001',
              受入予定日: '2025-01-20',
              発注単価: 1000,
              発注数量: 10,
              消費税金額: 1000,
            },
            {
              行番号: 2,
              品目コード: 'PART-002',
              受入予定日: '2025-01-25',
              発注単価: 2000,
              発注数量: 5,
              消費税金額: 1000,
            },
          ],
        },
      });

      if (response.statusCode !== 201) {
        console.error('Error response:', response.statusCode, response.body);
      }
      expect(response.statusCode).toBe(201);
      const body = JSON.parse(response.body);
      expect(body.発注番号).toBe('PO-001');
      expect(body.取引先コード).toBe('SUP-001');
      expect(body.ステータス).toBe('作成中');
      expect(body.明細).toHaveLength(2);
      expect(body.発注合計金額).toBe(20000); // getter
      expect(body.消費税合計額).toBe(2000); // getter
      expect(body.発注総額).toBe(22000); // getter
    });

    it('明細の未入荷数量が計算される', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-002',
          発注日: '2025-01-15',
          取引先コード: 'SUP-001',
          明細: [
            {
              行番号: 1,
              品目コード: 'PART-001',
              受入予定日: '2025-01-20',
              発注単価: 1000,
              発注数量: 100,
              入荷済数量: 30,
            },
          ],
        },
      });

      expect(response.statusCode).toBe(201);
      const body = JSON.parse(response.body);
      expect(body.明細[0].未入荷数量).toBe(70); // getter: 発注数量 - 入荷済数量
    });
  });

  describe('GET /purchase-orders/:orderNumber', () => {
    beforeEach(async () => {
      // テスト用発注を作成
      await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-100',
          発注日: '2025-01-15',
          取引先コード: 'SUP-001',
          明細: [
            {
              行番号: 1,
              品目コード: 'PART-001',
              受入予定日: '2025-01-20',
              発注単価: 1000,
              発注数量: 10,
            },
          ],
        },
      });
    });

    it('発注番号で発注を取得できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/purchase-orders/PO-100',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body.発注番号).toBe('PO-100');
      expect(body.取引先コード).toBe('SUP-001');
      expect(body.明細).toHaveLength(1);
    });

    it('存在しない発注の取得は404を返す', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/purchase-orders/PO-999',
      });

      expect(response.statusCode).toBe(404);
      const body = JSON.parse(response.body);
      expect(body.error).toBe('Not Found');
    });
  });

  describe('GET /purchase-orders/supplier/:supplierCode', () => {
    beforeEach(async () => {
      // 複数の発注を作成
      await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-SUP1-001',
          発注日: '2025-01-15',
          取引先コード: 'SUP-001',
          明細: [
            {
              行番号: 1,
              品目コード: 'PART-001',
              受入予定日: '2025-01-20',
              発注単価: 1000,
              発注数量: 10,
            },
          ],
        },
      });

      await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-SUP1-002',
          発注日: '2025-01-16',
          取引先コード: 'SUP-001',
          明細: [
            {
              行番号: 1,
              品目コード: 'PART-002',
              受入予定日: '2025-01-21',
              発注単価: 2000,
              発注数量: 5,
            },
          ],
        },
      });

      await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-SUP2-001',
          発注日: '2025-01-17',
          取引先コード: 'SUP-002',
          明細: [
            {
              行番号: 1,
              品目コード: 'MAT-001',
              受入予定日: '2025-01-22',
              発注単価: 500,
              発注数量: 100,
            },
          ],
        },
      });
    });

    it('取引先別の発注一覧を取得できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/purchase-orders/supplier/SUP-001',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body.every((po: any) => po.取引先コード === 'SUP-001')).toBe(true);
    });
  });

  describe('GET /purchase-orders/status/:status', () => {
    beforeEach(async () => {
      // 異なるステータスの発注を作成
      await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-DRAFT-001',
          発注日: '2025-01-15',
          取引先コード: 'SUP-001',
          ステータス: '作成中',
          明細: [
            {
              行番号: 1,
              品目コード: 'PART-001',
              受入予定日: '2025-01-20',
              発注単価: 1000,
              発注数量: 10,
            },
          ],
        },
      });

      await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-ORDERED-001',
          発注日: '2025-01-16',
          取引先コード: 'SUP-001',
          ステータス: '発注済',
          明細: [
            {
              行番号: 1,
              品目コード: 'PART-002',
              受入予定日: '2025-01-21',
              発注単価: 2000,
              発注数量: 5,
            },
          ],
        },
      });
    });

    it('ステータス別の発注一覧を取得できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/purchase-orders/status/作成中',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(1);
      expect(body[0].ステータス).toBe('作成中');
    });
  });

  describe('GET /purchase-orders', () => {
    beforeEach(async () => {
      // 複数の発注を作成
      await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-QUERY-001',
          発注日: '2025-01-15',
          取引先コード: 'SUP-001',
          ステータス: '作成中',
          明細: [
            {
              行番号: 1,
              品目コード: 'PART-001',
              受入予定日: '2025-01-20',
              発注単価: 1000,
              発注数量: 10,
            },
          ],
        },
      });

      await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-QUERY-002',
          発注日: '2025-01-16',
          取引先コード: 'SUP-001',
          ステータス: '発注済',
          明細: [
            {
              行番号: 1,
              品目コード: 'PART-002',
              受入予定日: '2025-01-21',
              発注単価: 2000,
              発注数量: 5,
            },
          ],
        },
      });

      await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-QUERY-003',
          発注日: '2025-01-17',
          取引先コード: 'SUP-002',
          ステータス: '作成中',
          明細: [
            {
              行番号: 1,
              品目コード: 'MAT-001',
              受入予定日: '2025-01-22',
              発注単価: 500,
              発注数量: 100,
            },
          ],
        },
      });
    });

    it('全発注を取得できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/purchase-orders',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(3);
    });

    it('取引先コードでフィルタリングできる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/purchase-orders?取引先コード=SUP-001',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body.every((po: any) => po.取引先コード === 'SUP-001')).toBe(true);
    });

    it('ステータスでフィルタリングできる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/purchase-orders?ステータス=作成中',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body.every((po: any) => po.ステータス === '作成中')).toBe(true);
    });

    it('取引先コードとステータスの両方でフィルタリングできる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/purchase-orders?取引先コード=SUP-001&ステータス=作成中',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(1);
      expect(body[0].発注番号).toBe('PO-QUERY-001');
      expect(body[0].取引先コード).toBe('SUP-001');
      expect(body[0].ステータス).toBe('作成中');
    });
  });

  describe('PATCH /purchase-orders/:orderNumber/status', () => {
    beforeEach(async () => {
      await app.inject({
        method: 'POST',
        url: '/purchase-orders',
        payload: {
          発注番号: 'PO-UPDATE-001',
          発注日: '2025-01-15',
          取引先コード: 'SUP-001',
          ステータス: '作成中',
          明細: [
            {
              行番号: 1,
              品目コード: 'PART-001',
              受入予定日: '2025-01-20',
              発注単価: 1000,
              発注数量: 10,
            },
          ],
        },
      });
    });

    it('発注ステータスを更新できる', async () => {
      const response = await app.inject({
        method: 'PATCH',
        url: '/purchase-orders/PO-UPDATE-001/status',
        payload: {
          ステータス: '発注済',
        },
      });

      expect(response.statusCode).toBe(204);

      // ステータスが更新されたことを確認
      const getResponse = await app.inject({
        method: 'GET',
        url: '/purchase-orders/PO-UPDATE-001',
      });

      const body = JSON.parse(getResponse.body);
      expect(body.ステータス).toBe('発注済');
    });
  });
});
