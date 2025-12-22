import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { FastifyInstance } from 'fastify';
import { TestDatabase } from '../../src/test-setup/database';
import { buildApp } from '../../src/infrastructure/web/application';

describe('BOM API', () => {
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

    // テスト用品目を作成
    await testDb.prisma!.item.createMany({
      data: [
        {
          itemCode: 'PROD-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '製品A',
          itemCategory: 'PRODUCT',
        },
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

  describe('POST /bom', () => {
    it('BOMを登録できる', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/bom',
        payload: {
          親品目コード: 'PROD-001',
          子品目コード: 'PART-001',
          基準量: 1,
          必要量: 2,
        },
      });

      expect(response.statusCode).toBe(201);
      const body = JSON.parse(response.body);
      expect(body.親品目コード).toBe('PROD-001');
      expect(body.子品目コード).toBe('PART-001');
      expect(body.必要量).toBe(2);
    });

    it('循環参照のBOMは登録できない', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/bom',
        payload: {
          親品目コード: 'PROD-001',
          子品目コード: 'PROD-001',
          基準量: 1,
          必要量: 1,
        },
      });

      expect(response.statusCode).toBe(422);
    });
  });

  describe('GET /bom/:parentItemCode/children', () => {
    it('親品目の子品目一覧を取得できる', async () => {
      // Arrange: BOMデータを準備
      await testDb.prisma!.bom.createMany({
        data: [
          {
            parentItemCode: 'PROD-001',
            childItemCode: 'PART-001',
            effectiveFrom: new Date('2025-01-01'),
            baseQuantity: 1,
            requiredQuantity: 2,
          },
          {
            parentItemCode: 'PROD-001',
            childItemCode: 'PART-002',
            effectiveFrom: new Date('2025-01-01'),
            baseQuantity: 1,
            requiredQuantity: 3,
          },
        ],
      });

      // Act
      const response = await app.inject({
        method: 'GET',
        url: '/bom/PROD-001/children',
      });

      // Assert
      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body[0].子品目コード).toBe('PART-001');
      expect(body[1].子品目コード).toBe('PART-002');
    });
  });

  describe('GET /bom/:itemCode/explode', () => {
    it('BOMを展開できる', async () => {
      // Arrange: 2階層のBOM構造を準備
      // PROD-001 -> PART-001 (数量2) -> MAT-001 (数量4)
      await testDb.prisma!.bom.createMany({
        data: [
          {
            parentItemCode: 'PROD-001',
            childItemCode: 'PART-001',
            effectiveFrom: new Date('2025-01-01'),
            baseQuantity: 1,
            requiredQuantity: 2,
          },
          {
            parentItemCode: 'PART-001',
            childItemCode: 'MAT-001',
            effectiveFrom: new Date('2025-01-01'),
            baseQuantity: 1,
            requiredQuantity: 4,
          },
        ],
      });

      // Act
      const response = await app.inject({
        method: 'GET',
        url: '/bom/PROD-001/explode?quantity=1',
      });

      // Assert
      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);

      // レベル1: PART-001
      const level1 = body.find((item: any) => item.レベル === 1);
      expect(level1.子品目コード).toBe('PART-001');
      expect(level1.合計必要量).toBe(2);

      // レベル2: MAT-001
      const level2 = body.find((item: any) => item.レベル === 2);
      expect(level2.子品目コード).toBe('MAT-001');
      expect(level2.合計必要量).toBe(8); // 2 * 4 = 8
    });

    it('数量を指定してBOMを展開できる', async () => {
      // Arrange
      await testDb.prisma!.bom.create({
        data: {
          parentItemCode: 'PROD-001',
          childItemCode: 'PART-001',
          effectiveFrom: new Date('2025-01-01'),
          baseQuantity: 1,
          requiredQuantity: 2,
        },
      });

      // Act: 数量10で展開
      const response = await app.inject({
        method: 'GET',
        url: '/bom/PROD-001/explode?quantity=10',
      });

      // Assert
      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body[0].合計必要量).toBe(20); // 2 * 10 = 20
    });
  });

  describe('GET /bom/:childItemCode/where-used', () => {
    it('使用先を照会できる', async () => {
      // Arrange: 複数の親品目で使用されているBOM
      await testDb.prisma!.bom.createMany({
        data: [
          {
            parentItemCode: 'PROD-001',
            childItemCode: 'PART-001',
            effectiveFrom: new Date('2025-01-01'),
            baseQuantity: 1,
            requiredQuantity: 2,
          },
          {
            parentItemCode: 'PART-002',
            childItemCode: 'PART-001',
            effectiveFrom: new Date('2025-01-01'),
            baseQuantity: 1,
            requiredQuantity: 5,
          },
        ],
      });

      // Act
      const response = await app.inject({
        method: 'GET',
        url: '/bom/PART-001/where-used',
      });

      // Assert
      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(2);
      expect(body[0].親品目コード).toBe('PART-002');
      expect(body[1].親品目コード).toBe('PROD-001');
    });
  });

  describe('DELETE /bom/:parentItemCode/:childItemCode/:effectiveFrom', () => {
    it('BOMを削除できる', async () => {
      // Arrange
      const effectiveFrom = new Date('2025-01-01');
      await testDb.prisma!.bom.create({
        data: {
          parentItemCode: 'PROD-001',
          childItemCode: 'PART-001',
          effectiveFrom,
          baseQuantity: 1,
          requiredQuantity: 2,
        },
      });

      // Act
      const response = await app.inject({
        method: 'DELETE',
        url: `/bom/PROD-001/PART-001/${effectiveFrom.toISOString().split('T')[0]}`,
      });

      // Assert
      expect(response.statusCode).toBe(204);

      // データベースから削除されていることを確認
      const deleted = await testDb.prisma!.bom.findFirst({
        where: {
          parentItemCode: 'PROD-001',
          childItemCode: 'PART-001',
        },
      });
      expect(deleted).toBeNull();
    });
  });
});
