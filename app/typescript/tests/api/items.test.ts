import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { FastifyInstance } from 'fastify';
import { TestDatabase } from '../../src/test-setup/database';
import { buildApp } from '../../src/infrastructure/web/application';

describe('品目マスタ API', () => {
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
  });

  describe('GET /items', () => {
    it('品目一覧を取得できる', async () => {
      // Arrange: テストデータを準備
      await testDb.prisma!.item.create({
        data: {
          itemCode: 'PROD-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: 'テスト製品',
          itemCategory: 'PRODUCT',
        },
      });

      // Act
      const response = await app.inject({
        method: 'GET',
        url: '/items',
      });

      // Assert
      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(1);
      expect(body[0].品目コード).toBe('PROD-001');
    });

    it('品目区分でフィルタリングできる', async () => {
      // Arrange
      await testDb.prisma!.item.createMany({
        data: [
          { itemCode: 'PROD-001', effectiveFrom: new Date('2025-01-01'), itemName: '製品A', itemCategory: 'PRODUCT' },
          { itemCode: 'PART-001', effectiveFrom: new Date('2025-01-01'), itemName: '部品A', itemCategory: 'PART' },
        ],
      });

      // Act
      const response = await app.inject({
        method: 'GET',
        url: '/items?category=PRODUCT',
      });

      // Assert
      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body).toHaveLength(1);
      expect(body[0].品目区分).toBe('PRODUCT');
    });
  });

  describe('GET /items/:itemCode', () => {
    it('品目コードで品目を取得できる', async () => {
      // Arrange
      await testDb.prisma!.item.create({
        data: {
          itemCode: 'PROD-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: 'テスト製品',
          itemCategory: 'PRODUCT',
        },
      });

      // Act
      const response = await app.inject({
        method: 'GET',
        url: '/items/PROD-001',
      });

      // Assert
      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body.品目コード).toBe('PROD-001');
      expect(body.品名).toBe('テスト製品');
    });

    it('存在しない品目コードは404を返す', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/items/NOT-EXIST',
      });

      expect(response.statusCode).toBe(404);
    });
  });

  describe('POST /items', () => {
    it('品目を登録できる', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/items',
        payload: {
          品目コード: 'NEW-001',
          品名: '新規品目',
          品目区分: 'PRODUCT',
        },
      });

      expect(response.statusCode).toBe(201);
      const body = JSON.parse(response.body);
      expect(body.品目コード).toBe('NEW-001');

      // データベースに保存されていることを確認
      const saved = await testDb.prisma!.item.findFirst({
        where: { itemCode: 'NEW-001' },
      });
      expect(saved).not.toBeNull();
    });

    it('重複する品目コードは409エラーを返す', async () => {
      // Arrange
      await testDb.prisma!.item.create({
        data: {
          itemCode: 'EXIST-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '既存品目',
          itemCategory: 'PRODUCT',
        },
      });

      // Act
      const response = await app.inject({
        method: 'POST',
        url: '/items',
        payload: {
          品目コード: 'EXIST-001',
          品名: '新規品目',
          品目区分: 'PRODUCT',
        },
      });

      // Assert
      expect(response.statusCode).toBe(409);
    });
  });

  describe('DELETE /items/:itemCode', () => {
    it('品目を削除できる', async () => {
      // Arrange
      await testDb.prisma!.item.create({
        data: {
          itemCode: 'DELETE-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '削除対象',
          itemCategory: 'PRODUCT',
        },
      });

      // Act
      const response = await app.inject({
        method: 'DELETE',
        url: '/items/DELETE-001',
      });

      // Assert
      expect(response.statusCode).toBe(204);

      const deleted = await testDb.prisma!.item.findFirst({
        where: { itemCode: 'DELETE-001' },
      });
      expect(deleted).toBeNull();
    });
  });
});
