import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { FastifyInstance } from 'fastify';
import { TestDatabase } from '../../src/test-setup/database';
import { buildApp } from '../../src/infrastructure/web/application';

describe('MRP API', () => {
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

    // 品目マスタ（製品と部品）
    await testDb.prisma!.item.createMany({
      data: [
        {
          itemCode: 'PRODUCT-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '製品A',
          itemCategory: 'PRODUCT',
          unitCode: 'PC',
        },
        {
          itemCode: 'PART-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '部品A',
          itemCategory: 'PART',
          unitCode: 'PC',
        },
        {
          itemCode: 'PART-002',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '部品B',
          itemCategory: 'PART',
          unitCode: 'PC',
        },
      ],
    });

    // BOM（部品構成表）
    await testDb.prisma!.bom.createMany({
      data: [
        {
          parentItemCode: 'PRODUCT-001',
          childItemCode: 'PART-001',
          effectiveFrom: new Date('2025-01-01'),
          baseQuantity: 1,
          requiredQuantity: 2, // 製品1個につき部品A 2個必要
        },
        {
          parentItemCode: 'PRODUCT-001',
          childItemCode: 'PART-002',
          effectiveFrom: new Date('2025-01-01'),
          baseQuantity: 1,
          requiredQuantity: 3, // 製品1個につき部品B 3個必要
        },
      ],
    });
  });

  describe('POST /mrp/execute', () => {
    it('MRPを実行して製造オーダ、購買オーダ、所要情報を生成できる', async () => {
      // Arrange: MPS（基準生産計画）を作成
      await testDb.prisma!.$executeRaw`
        INSERT INTO master_production_schedules (
          mps_number, plan_date, item_code, planned_quantity, due_date, status, location_code
        )
        VALUES (
          'MPS-2025-001', '2025-01-01', 'PRODUCT-001', 100, '2025-01-31', 'CONFIRMED', 'WH-001'
        )
      `;

      // Act: MRP実行
      const response = await app.inject({
        method: 'POST',
        url: '/mrp/execute',
        payload: {
          mpsNumber: 'MPS-2025-001',
        },
      });

      // Assert
      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body.mpsNumber).toBe('MPS-2025-001');
      expect(body.generatedManufacturingOrders).toBe(1);
      expect(body.generatedPurchaseOrders).toBe(2); // 部品A, 部品B
      expect(body.generatedRequirements).toBe(2); // 部品A, 部品B
      expect(body.executedAt).toBeDefined();

      // 製造オーダが生成されたことを確認
      const manufacturingOrders = await testDb.prisma!.$queryRaw<any[]>`
        SELECT * FROM orders WHERE order_type = 'MANUFACTURING'
      `;
      expect(manufacturingOrders).toHaveLength(1);
      expect(manufacturingOrders[0].order_number).toBe('MO-MPS-2025-001');
      expect(Number(manufacturingOrders[0].planned_quantity)).toBe(100);

      // 購買オーダが生成されたことを確認
      const purchaseOrders = await testDb.prisma!.$queryRaw<any[]>`
        SELECT * FROM orders WHERE order_type = 'PURCHASE' ORDER BY order_number
      `;
      expect(purchaseOrders).toHaveLength(2);
      expect(purchaseOrders[0].order_number).toBe('PO-MPS-2025-001-001'); // 部品A
      expect(Number(purchaseOrders[0].planned_quantity)).toBe(200); // 100 * 2
      expect(purchaseOrders[1].order_number).toBe('PO-MPS-2025-001-002'); // 部品B
      expect(Number(purchaseOrders[1].planned_quantity)).toBe(300); // 100 * 3

      // 所要情報が生成されたことを確認
      const requirements = await testDb.prisma!.$queryRaw<any[]>`
        SELECT * FROM requirements ORDER BY requirement_number
      `;
      expect(requirements).toHaveLength(2);
      expect(requirements[0].requirement_number).toBe('REQ-MPS-2025-001-001');
      expect(Number(requirements[0].required_quantity)).toBe(200); // 部品A
      expect(requirements[1].requirement_number).toBe('REQ-MPS-2025-001-002');
      expect(Number(requirements[1].required_quantity)).toBe(300); // 部品B

      // MPSステータスがEXPANDEDに更新されたことを確認
      const mps = await testDb.prisma!.$queryRaw<any[]>`
        SELECT * FROM master_production_schedules WHERE mps_number = 'MPS-2025-001'
      `;
      expect(mps[0].status).toBe('EXPANDED');
    });

    it('存在しないMPSの場合は404エラーを返す', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/mrp/execute',
        payload: {
          mpsNumber: 'MPS-INVALID',
        },
      });

      expect(response.statusCode).toBe(404);
      const body = JSON.parse(response.body);
      expect(body.error).toBe('Not Found');
      expect(body.message).toContain('MPS not found');
    });

    it('ステータスがCONFIRMED以外の場合は400エラーを返す', async () => {
      // Arrange: DRAFTステータスのMPSを作成
      await testDb.prisma!.$executeRaw`
        INSERT INTO master_production_schedules (
          mps_number, plan_date, item_code, planned_quantity, due_date, status, location_code
        )
        VALUES (
          'MPS-2025-002', '2025-01-01', 'PRODUCT-001', 100, '2025-01-31', 'DRAFT', 'WH-001'
        )
      `;

      // Act
      const response = await app.inject({
        method: 'POST',
        url: '/mrp/execute',
        payload: {
          mpsNumber: 'MPS-2025-002',
        },
      });

      // Assert
      expect(response.statusCode).toBe(400);
      const body = JSON.parse(response.body);
      expect(body.error).toBe('Bad Request');
      expect(body.message).toContain('status must be CONFIRMED');
    });

    it('BOMが存在しない製品でもMRPを実行できる（部品なし製品）', async () => {
      // Arrange: 部品なし製品のMPSを作成
      await testDb.prisma!.item.create({
        data: {
          itemCode: 'SIMPLE-001',
          effectiveFrom: new Date('2025-01-01'),
          itemName: '単純製品',
          itemCategory: 'PRODUCT',
          unitCode: 'PC',
        },
      });

      await testDb.prisma!.$executeRaw`
        INSERT INTO master_production_schedules (
          mps_number, plan_date, item_code, planned_quantity, due_date, status, location_code
        )
        VALUES (
          'MPS-2025-003', '2025-01-01', 'SIMPLE-001', 50, '2025-01-31', 'CONFIRMED', 'WH-001'
        )
      `;

      // Act
      const response = await app.inject({
        method: 'POST',
        url: '/mrp/execute',
        payload: {
          mpsNumber: 'MPS-2025-003',
        },
      });

      // Assert
      expect(response.statusCode).toBe(200);
      const body = JSON.parse(response.body);
      expect(body.generatedManufacturingOrders).toBe(1);
      expect(body.generatedPurchaseOrders).toBe(0); // 部品なし
      expect(body.generatedRequirements).toBe(0); // 部品なし
    });
  });
});
