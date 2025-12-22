import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { FastifyInstance } from 'fastify';
import { TestDatabase } from '../../src/test-setup/database';
import { buildApp } from '../../src/infrastructure/web/application';

describe('Inventory API', () => {
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
    // 場所マスタ
    await testDb.prisma!.$executeRaw`
      INSERT INTO locations (location_code, location_name, location_type)
      VALUES
        ('WH-01', '第1倉庫', 'WAREHOUSE'::location_type),
        ('WH-02', '第2倉庫', 'WAREHOUSE'::location_type)
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

    // 在庫データ
    await testDb.prisma!.inventory.createMany({
      data: [
        {
          locationCode: 'WH-01',
          itemCode: 'PART-001',
          stockQuantity: 100,
          acceptedQuantity: 90,
          defectQuantity: 5,
          uninspectedQuantity: 5,
        },
        {
          locationCode: 'WH-01',
          itemCode: 'PART-002',
          stockQuantity: 50,
          acceptedQuantity: 45,
          defectQuantity: 3,
          uninspectedQuantity: 2,
        },
        {
          locationCode: 'WH-02',
          itemCode: 'PART-001',
          stockQuantity: 200,
          acceptedQuantity: 180,
          defectQuantity: 15,
          uninspectedQuantity: 5,
        },
        {
          locationCode: 'WH-02',
          itemCode: 'MAT-001',
          stockQuantity: 1000,
          acceptedQuantity: 950,
          defectQuantity: 30,
          uninspectedQuantity: 20,
        },
      ],
    });
  });

  describe('GET /inventory/:locationCode/:itemCode', () => {
    it('場所・品目別の在庫を照会できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/inventory/WH-01/PART-001',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body.場所コード).toBe('WH-01');
      expect(body.品目コード).toBe('PART-001');
      expect(body.在庫数量).toBe(100);
      expect(body.合格数).toBe(90);
      expect(body.不良数).toBe(5);
      expect(body.未検査数).toBe(5);
      expect(body.利用可能数量).toBe(90);
    });

    it('存在しない在庫の照会は404を返す', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/inventory/WH-99/PART-999',
      });

      expect(response.statusCode).toBe(404);
      const body = JSON.parse(response.body);
      expect(body.error).toBe('Not Found');
    });
  });

  describe('GET /inventory/location/:locationCode', () => {
    it('場所別の在庫一覧を照会できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/inventory/location/WH-01',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body[0].場所コード).toBe('WH-01');
      expect(body[0].品目コード).toBe('PART-001');
      expect(body[1].場所コード).toBe('WH-01');
      expect(body[1].品目コード).toBe('PART-002');
    });

    it('在庫がない場所は空配列を返す', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/inventory/location/WH-99',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(0);
    });
  });

  describe('GET /inventory/item/:itemCode', () => {
    it('品目別の在庫一覧を照会できる（全場所）', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/inventory/item/PART-001',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body[0].場所コード).toBe('WH-01');
      expect(body[0].品目コード).toBe('PART-001');
      expect(body[0].在庫数量).toBe(100);
      expect(body[1].場所コード).toBe('WH-02');
      expect(body[1].品目コード).toBe('PART-001');
      expect(body[1].在庫数量).toBe(200);
    });
  });

  describe('GET /inventory', () => {
    it('全在庫を照会できる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/inventory',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(4);
    });

    it('場所コードでフィルタリングできる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/inventory?場所コード=WH-02',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body.every((item: any) => item.場所コード === 'WH-02')).toBe(true);
    });

    it('品目コードでフィルタリングできる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/inventory?品目コード=PART-001',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body.every((item: any) => item.品目コード === 'PART-001')).toBe(true);
    });

    it('場所コードと品目コードの両方でフィルタリングできる', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/inventory?場所コード=WH-01&品目コード=PART-002',
      });

      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(1);
      expect(body[0].場所コード).toBe('WH-01');
      expect(body[0].品目コード).toBe('PART-002');
    });
  });
});
