import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { FastifyInstance } from 'fastify';
import { TestDatabase } from '../../src/test-setup/database';
import { buildApp } from '../../src/infrastructure/web/application';

describe('Work Order API', () => {
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
    // 単位マスタ
    await testDb.prisma!.unit.createMany({
      data: [
        {
          unitCode: 'PC',
          unitSymbol: '個',
          unitName: '個数',
        },
      ],
    });

    // 品目マスタ
    await testDb.prisma!.item.createMany({
      data: [
        {
          itemCode: 'PRODUCT-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '製品A',
          itemCategory: 'PRODUCT',
          unitCode: 'PC',
        },
      ],
    });

    // 工程マスタ
    await testDb.prisma!.processMaster.createMany({
      data: [
        {
          processCode: 'PROC-001',
          processName: 'プレス加工',
        },
        {
          processCode: 'PROC-002',
          processName: 'メッキ加工',
        },
      ],
    });
  });

  describe('POST /work-orders', () => {
    it('作業指示を作成できる', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-001',
          製造オーダ番号: 'MO-001',
          作業指示日: '2025-01-20',
          品目コード: 'PRODUCT-001',
          指示数量: 100,
          ステータス: '未着手',
          開始予定日: '2025-01-21',
          完了予定日: '2025-01-25',
          備考: 'テスト作業指示',
          明細: [
            {
              工順: 1,
              工程コード: 'PROC-001',
              開始予定日時: '2025-01-21T09:00:00Z',
              完了予定日時: '2025-01-22T17:00:00Z',
            },
            {
              工順: 2,
              工程コード: 'PROC-002',
              開始予定日時: '2025-01-23T09:00:00Z',
              完了予定日時: '2025-01-25T17:00:00Z',
            },
          ],
        },
      });

      if (response.statusCode !== 201) {
        console.error('Error response:', response.statusCode, response.body);
      }
      expect(response.statusCode).toBe(201);
      const body = JSON.parse(response.body);
      expect(body.作業指示番号).toBe('WO-001');
      expect(body.製造オーダ番号).toBe('MO-001');
      expect(body.品目コード).toBe('PRODUCT-001');
      expect(body.指示数量).toBe(100);
      expect(body.完成数量).toBe(0);
      expect(body.ステータス).toBe('未着手');
      expect(body.明細).toHaveLength(2);
      expect(body.未完成数量).toBe(100); // getter
      expect(body.進捗率).toBe(0); // getter
      expect(body.完了済み).toBe(false); // getter
    });

    it('getter プロパティが計算される', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-002',
          製造オーダ番号: 'MO-001',
          作業指示日: '2025-01-20',
          品目コード: 'PRODUCT-001',
          指示数量: 100,
          完成数量: 30,
          ステータス: '作業中',
          明細: [
            {
              工順: 1,
              工程コード: 'PROC-001',
            },
          ],
        },
      });

      expect(response.statusCode).toBe(201);
      const body = JSON.parse(response.body);
      expect(body.未完成数量).toBe(70); // getter: 指示数量 - 完成数量
      expect(body.進捗率).toBe(30); // getter: 完成数量 / 指示数量 * 100
      expect(body.完了済み).toBe(false); // getter: ステータス !== '完了'
    });
  });

  describe('GET /work-orders/:workOrderNumber', () => {
    beforeEach(async () => {
      // テスト用作業指示を作成
      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-100',
          製造オーダ番号: 'MO-100',
          作業指示日: '2025-01-20',
          品目コード: 'PRODUCT-001',
          指示数量: 100,
          明細: [
            {
              工順: 1,
              工程コード: 'PROC-001',
            },
          ],
        },
      });
    });

    it('作業指示番号で作業指示を取得できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/work-orders/WO-100',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body.作業指示番号).toBe('WO-100');
      expect(body.製造オーダ番号).toBe('MO-100');
      expect(body.明細).toHaveLength(1);
    });

    it('存在しない作業指示の取得は404を返す', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/work-orders/WO-999',
      });

      expect(response.statusCode).toBe(404);
      const body = JSON.parse(response.body);
      expect(body.error).toBe('Not Found');
    });
  });

  describe('GET /work-orders/production-order/:productionOrderNumber', () => {
    beforeEach(async () => {
      // 同じ製造オーダに複数の作業指示を作成
      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-MO1-001',
          製造オーダ番号: 'MO-001',
          作業指示日: '2025-01-20',
          品目コード: 'PRODUCT-001',
          指示数量: 100,
          明細: [{ 工順: 1, 工程コード: 'PROC-001' }],
        },
      });

      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-MO1-002',
          製造オーダ番号: 'MO-001',
          作業指示日: '2025-01-21',
          品目コード: 'PRODUCT-001',
          指示数量: 50,
          明細: [{ 工順: 1, 工程コード: 'PROC-002' }],
        },
      });

      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-MO2-001',
          製造オーダ番号: 'MO-002',
          作業指示日: '2025-01-22',
          品目コード: 'PRODUCT-001',
          指示数量: 75,
          明細: [{ 工順: 1, 工程コード: 'PROC-001' }],
        },
      });
    });

    it('製造オーダ別の作業指示一覧を取得できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/work-orders/production-order/MO-001',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body.every((wo: any) => wo.製造オーダ番号 === 'MO-001')).toBe(true);
    });
  });

  describe('GET /work-orders/status/:status', () => {
    beforeEach(async () => {
      // 異なるステータスの作業指示を作成
      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-NS-001',
          製造オーダ番号: 'MO-001',
          作業指示日: '2025-01-20',
          品目コード: 'PRODUCT-001',
          指示数量: 100,
          ステータス: '未着手',
          明細: [{ 工順: 1, 工程コード: 'PROC-001' }],
        },
      });

      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-IP-001',
          製造オーダ番号: 'MO-002',
          作業指示日: '2025-01-21',
          品目コード: 'PRODUCT-001',
          指示数量: 50,
          ステータス: '作業中',
          明細: [{ 工順: 1, 工程コード: 'PROC-002' }],
        },
      });
    });

    it('ステータス別の作業指示一覧を取得できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/work-orders/status/未着手',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(1);
      expect(body[0].ステータス).toBe('未着手');
    });
  });

  describe('GET /work-orders', () => {
    beforeEach(async () => {
      // 複数の作業指示を作成
      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-Q-001',
          製造オーダ番号: 'MO-001',
          作業指示日: '2025-01-20',
          品目コード: 'PRODUCT-001',
          指示数量: 100,
          ステータス: '未着手',
          明細: [{ 工順: 1, 工程コード: 'PROC-001' }],
        },
      });

      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-Q-002',
          製造オーダ番号: 'MO-001',
          作業指示日: '2025-01-21',
          品目コード: 'PRODUCT-001',
          指示数量: 50,
          ステータス: '作業中',
          明細: [{ 工順: 1, 工程コード: 'PROC-002' }],
        },
      });

      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-Q-003',
          製造オーダ番号: 'MO-002',
          作業指示日: '2025-01-22',
          品目コード: 'PRODUCT-001',
          指示数量: 75,
          ステータス: '未着手',
          明細: [{ 工順: 1, 工程コード: 'PROC-001' }],
        },
      });
    });

    it('全作業指示を取得できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/work-orders',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(3);
    });

    it('製造オーダ番号でフィルタリングできる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/work-orders?製造オーダ番号=MO-001',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body.every((wo: any) => wo.製造オーダ番号 === 'MO-001')).toBe(true);
    });

    it('ステータスでフィルタリングできる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/work-orders?ステータス=未着手',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body.every((wo: any) => wo.ステータス === '未着手')).toBe(true);
    });

    it('製造オーダ番号とステータスの両方でフィルタリングできる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/work-orders?製造オーダ番号=MO-001&ステータス=未着手',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(1);
      expect(body[0].作業指示番号).toBe('WO-Q-001');
      expect(body[0].製造オーダ番号).toBe('MO-001');
      expect(body[0].ステータス).toBe('未着手');
    });
  });

  describe('PATCH /work-orders/:workOrderNumber/status', () => {
    beforeEach(async () => {
      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-UPDATE-001',
          製造オーダ番号: 'MO-001',
          作業指示日: '2025-01-20',
          品目コード: 'PRODUCT-001',
          指示数量: 100,
          ステータス: '未着手',
          明細: [{ 工順: 1, 工程コード: 'PROC-001' }],
        },
      });
    });

    it('作業指示ステータスを更新できる', async () => {
      const response = await app.inject({
        method: 'PATCH',
        url: '/work-orders/WO-UPDATE-001/status',
        payload: {
          ステータス: '作業中',
        },
      });

      expect(response.statusCode).toBe(204);

      // ステータスが更新されたことを確認
      const getResponse = await app.inject({
        method: 'GET',
        url: '/work-orders/WO-UPDATE-001',
      });

      const body = JSON.parse(getResponse.body);
      expect(body.ステータス).toBe('作業中');
    });
  });

  describe('PATCH /work-orders/:workOrderNumber/completed-quantity', () => {
    beforeEach(async () => {
      await app.inject({
        method: 'POST',
        url: '/work-orders',
        payload: {
          作業指示番号: 'WO-CQ-001',
          製造オーダ番号: 'MO-001',
          作業指示日: '2025-01-20',
          品目コード: 'PRODUCT-001',
          指示数量: 100,
          完成数量: 0,
          明細: [{ 工順: 1, 工程コード: 'PROC-001' }],
        },
      });
    });

    it('完成数量を更新できる', async () => {
      const response = await app.inject({
        method: 'PATCH',
        url: '/work-orders/WO-CQ-001/completed-quantity',
        payload: {
          完成数量: 50,
        },
      });

      expect(response.statusCode).toBe(204);

      // 完成数量が更新されたことを確認
      const getResponse = await app.inject({
        method: 'GET',
        url: '/work-orders/WO-CQ-001',
      });

      const body = JSON.parse(getResponse.body);
      expect(body.完成数量).toBe(50);
      expect(body.未完成数量).toBe(50); // getter
      expect(body.進捗率).toBe(50); // getter
    });
  });
});
