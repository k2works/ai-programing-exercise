import Fastify, { FastifyInstance } from 'fastify';
import cors from '@fastify/cors';
import swagger from '@fastify/swagger';
import swaggerUi from '@fastify/swagger-ui';
import { PrismaClient } from '@prisma/client';
import { errorHandler } from './error-handler';
import { ItemController } from './controller/ItemController';
import { ItemService } from '../../application/service/ItemService';
import { PrismaItemRepository } from '../persistence/repository/PrismaItemRepository';
import { BomController } from './controller/BomController';
import { BomService } from '../../application/service/BomService';
import { PrismaBomRepository } from '../persistence/repository/PrismaBomRepository';
import { InventoryController } from './controller/InventoryController';
import { InventoryService } from '../../application/service/InventoryService';
import { PrismaInventoryRepository } from '../persistence/repository/PrismaInventoryRepository';
import { PurchaseOrderController } from './controller/PurchaseOrderController';
import { PurchaseOrderService } from '../../application/service/PurchaseOrderService';
import { PrismaPurchaseOrderRepository } from '../persistence/repository/PrismaPurchaseOrderRepository';
import { WorkOrderController } from './controller/WorkOrderController';
import { WorkOrderService } from '../../application/service/WorkOrderService';
import { PrismaWorkOrderRepository } from '../persistence/repository/PrismaWorkOrderRepository';
import { MRPController } from './controller/MRPController';
import { MRPService } from '../../application/service/MRPService';
import { MPSRepository } from '../../domain/production-plan/mps.repository';
import { OrderRepository } from '../../domain/production-plan/order.repository';
import { RequirementRepository } from '../../domain/production-plan/requirement.repository';
import { prisma } from '../../lib/prisma';

/**
 * Fastify アプリケーションを構築
 * @param customPrisma オプション: テスト用のPrismaインスタンス
 */
export async function buildApp(customPrisma?: PrismaClient): Promise<FastifyInstance> {
  const app = Fastify({
    logger: true,
  });

  // エラーハンドラーを登録
  app.setErrorHandler(errorHandler);

  // CORS の設定
  await app.register(cors, {
    origin: true, // 開発環境用、本番環境では適切に制限する
  });

  // Swagger の設定
  await app.register(swagger, {
    openapi: {
      info: {
        title: '生産管理システム API',
        description: 'TDD で育てる生産管理システムの API ドキュメント',
        version: '1.0.0',
      },
      servers: [
        {
          url: 'http://localhost:3000',
          description: '開発サーバー',
        },
      ],
      tags: [
        { name: 'items', description: '品目マスタ API' },
        { name: 'bom', description: 'BOM（部品構成表）API' },
        { name: 'suppliers', description: '取引先マスタ API' },
        { name: 'orders', description: 'オーダ API' },
        { name: 'inventory', description: '在庫 API' },
        { name: 'work-orders', description: '作業指示 API' },
        { name: 'purchase-orders', description: '発注 API' },
        { name: 'mrp', description: 'MRP（資材所要量計画）API' },
      ],
    },
  });

  // Swagger UI の設定
  await app.register(swaggerUi, {
    routePrefix: '/docs',
    uiConfig: {
      docExpansion: 'list',
      deepLinking: true,
    },
    staticCSP: true,
  });

  /**
   * ルートエンドポイント
   */
  app.get('/', async () => {
    return {
      message: '生産管理システム API',
      version: '1.0.0',
      endpoints: [
        '/items',
        '/bom',
        '/suppliers',
        '/orders',
        '/inventory',
        '/work-orders',
        '/purchase-orders',
      ],
      docs: '/docs',
    };
  });

  /**
   * ヘルスチェックエンドポイント
   */
  app.get('/health', async () => {
    return { status: 'ok', timestamp: new Date().toISOString() };
  });

  // 品目マスタ API の登録
  const dbClient = customPrisma || prisma;
  const itemRepository = new PrismaItemRepository(dbClient);
  const itemService = new ItemService(itemRepository);
  const itemController = new ItemController(itemService);
  itemController.registerRoutes(app);

  // BOM API の登録
  const bomRepository = new PrismaBomRepository(dbClient);
  const bomService = new BomService(bomRepository);
  const bomController = new BomController(bomService);
  bomController.registerRoutes(app);

  // 在庫 API の登録
  const inventoryRepository = new PrismaInventoryRepository(dbClient);
  const inventoryService = new InventoryService(inventoryRepository);
  const inventoryController = new InventoryController(inventoryService);
  inventoryController.registerRoutes(app);

  // 発注 API の登録
  const purchaseOrderRepository = new PrismaPurchaseOrderRepository(dbClient);
  const purchaseOrderService = new PurchaseOrderService(purchaseOrderRepository);
  const purchaseOrderController = new PurchaseOrderController(purchaseOrderService);
  purchaseOrderController.registerRoutes(app);

  // 作業指示 API の登録
  const workOrderRepository = new PrismaWorkOrderRepository(dbClient);
  const workOrderService = new WorkOrderService(workOrderRepository);
  const workOrderController = new WorkOrderController(workOrderService);
  workOrderController.registerRoutes(app);

  // MRP API の登録
  const mpsRepository = new MPSRepository(dbClient);
  const orderRepository = new OrderRepository(dbClient);
  const requirementRepository = new RequirementRepository(dbClient);
  const mrpService = new MRPService(
    mpsRepository,
    orderRepository,
    requirementRepository,
    bomRepository
  );
  const mrpController = new MRPController(mrpService);
  mrpController.registerRoutes(app);

  return app;
}

/**
 * サーバーを起動
 */
export async function startServer(port = 3000): Promise<FastifyInstance> {
  const app = await buildApp();

  try {
    await app.listen({ port, host: '0.0.0.0' });
    console.log(`サーバーが http://localhost:${port} で起動しました`);
    console.log(`API 仕様は http://localhost:${port}/docs で確認できます`);
  } catch (err) {
    app.log.error(err);
    process.exit(1);
  }

  return app;
}
