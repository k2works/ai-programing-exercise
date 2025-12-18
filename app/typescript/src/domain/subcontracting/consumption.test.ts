import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { ConsumptionRepository } from './consumption.repository'
import { SupplyRepository, SupplyCategory } from './supply.repository'
import { ReceivingRepository } from '../receiving/receiving.repository'
import { PurchaseOrderRepository } from '../purchase/purchase-order.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'
import { SupplierRepository } from '../supplier/supplier.repository'

describe('消費業務', () => {
  let testDb: TestDatabase
  let consumptionRepository: ConsumptionRepository
  let supplyRepository: SupplyRepository
  let receivingRepository: ReceivingRepository
  let purchaseOrderRepository: PurchaseOrderRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository
  let supplierRepository: SupplierRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    consumptionRepository = new ConsumptionRepository(testDb.prisma!)
    supplyRepository = new SupplyRepository(testDb.prisma!)
    receivingRepository = new ReceivingRepository(testDb.prisma!)
    purchaseOrderRepository = new PurchaseOrderRepository(testDb.prisma!)
    itemRepository = new ItemRepository(testDb.prisma!)
    unitRepository = new UnitRepository(testDb.prisma!)
    supplierRepository = new SupplierRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('消費データを登録できる', async () => {
    // Arrange: テストデータを準備
    await setupTestData()

    // Act: 消費データを登録
    const consumption = await consumptionRepository.create({
      consumptionNumber: 'CON-2025-001',
      receivingNumber: 'RCV-2025-001',
      consumptionDate: new Date('2025-01-26'),
      supplierCode: 'SUP-001',
      details: [
        {
          itemCode: 'PRESS-001',
          consumptionQuantity: 95, // 100支給して95消費（歩留り95%）
        },
      ],
      createdBy: 'user01',
    })

    // Assert
    expect(consumption.id).toBeDefined()
    expect(consumption.consumptionNumber).toBe('CON-2025-001')
    expect(consumption.receivingNumber).toBe('RCV-2025-001')
    expect(consumption.supplierCode).toBe('SUP-001')
    expect(consumption.details).toHaveLength(1)
    expect(consumption.details[0].itemCode).toBe('PRESS-001')
    expect(Number(consumption.details[0].consumptionQuantity)).toBe(95)
  })

  test('消費番号で消費データを検索できる', async () => {
    // Arrange
    await setupTestData()

    await consumptionRepository.create({
      consumptionNumber: 'CON-2025-001',
      receivingNumber: 'RCV-2025-001',
      consumptionDate: new Date('2025-01-26'),
      supplierCode: 'SUP-001',
      details: [
        {
          itemCode: 'PRESS-001',
          consumptionQuantity: 95,
        },
      ],
    })

    // Act
    const consumption = await consumptionRepository.findByNumber('CON-2025-001')

    // Assert
    expect(consumption).toBeDefined()
    expect(consumption!.consumptionNumber).toBe('CON-2025-001')
    expect(consumption!.details).toHaveLength(1)
    expect(Number(consumption!.details[0].consumptionQuantity)).toBe(95)
  })

  test('入荷番号で消費データを検索できる', async () => {
    // Arrange
    await setupTestData()

    await consumptionRepository.create({
      consumptionNumber: 'CON-2025-001',
      receivingNumber: 'RCV-2025-001',
      consumptionDate: new Date('2025-01-26'),
      supplierCode: 'SUP-001',
      details: [
        {
          itemCode: 'PRESS-001',
          consumptionQuantity: 95,
        },
      ],
    })

    // Act
    const consumptions = await consumptionRepository.findByReceiving('RCV-2025-001')

    // Assert
    expect(consumptions).toHaveLength(1)
    expect(consumptions[0].consumptionNumber).toBe('CON-2025-001')
  })

  test('消費率を計算できる', async () => {
    // Arrange
    await setupTestData()

    await consumptionRepository.create({
      consumptionNumber: 'CON-2025-001',
      receivingNumber: 'RCV-2025-001',
      consumptionDate: new Date('2025-01-26'),
      supplierCode: 'SUP-001',
      details: [
        {
          itemCode: 'PRESS-001',
          consumptionQuantity: 95, // 100支給して95消費
        },
      ],
    })

    // Act
    const consumptionRate = await consumptionRepository.calculateConsumptionRate('SUP-2025-001', 'PRESS-001')

    // Assert
    expect(consumptionRate).toBeCloseTo(0.95, 2) // 95 / 100 = 0.95 (95%)
  })

  test('複数回消費した場合の消費率を計算できる', async () => {
    // Arrange
    await setupTestData()

    // 1回目の消費
    await consumptionRepository.create({
      consumptionNumber: 'CON-2025-001',
      receivingNumber: 'RCV-2025-001',
      consumptionDate: new Date('2025-01-26'),
      supplierCode: 'SUP-001',
      details: [
        {
          itemCode: 'PRESS-001',
          consumptionQuantity: 50,
        },
      ],
    })

    // 2回目の消費
    await consumptionRepository.create({
      consumptionNumber: 'CON-2025-002',
      receivingNumber: 'RCV-2025-001',
      consumptionDate: new Date('2025-01-27'),
      supplierCode: 'SUP-001',
      details: [
        {
          itemCode: 'PRESS-001',
          consumptionQuantity: 45, // 合計95
        },
      ],
    })

    // Act
    const consumptionRate = await consumptionRepository.calculateConsumptionRate('SUP-2025-001', 'PRESS-001')

    // Assert
    expect(consumptionRate).toBeCloseTo(0.95, 2) // (50 + 45) / 100 = 0.95
  })

  // テストデータ準備用ヘルパー関数
  async function setupTestData() {
    // 取引先
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '外注先A',
      supplierType: 'VENDOR',
    })

    // 単位
    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    // 品目（プレス部品：支給品）
    await itemRepository.create({
      itemCode: 'PRESS-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: 'プレス部品',
      itemCategory: 'PART',
      unitCode: 'PC',
    })

    // 品目（メッキ加工品：発注品）
    await itemRepository.create({
      itemCode: 'PLATED-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: 'メッキ加工品',
      itemCategory: 'SEMI_PRODUCT',
      unitCode: 'PC',
    })

    // 発注データ
    await purchaseOrderRepository.create({
      orderNumber: 'PO-2025-001',
      orderDate: new Date('2025-01-10'),
      supplierCode: 'SUP-001',
    })

    await purchaseOrderRepository.createDetail({
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      itemCode: 'PLATED-001',
      scheduledReceiptDate: new Date('2025-01-25'),
      orderUnitPrice: 500,
      orderQuantity: 100,
    })

    // 支給データ
    await supplyRepository.create({
      supplyNumber: 'SUP-2025-001',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      supplierCode: 'SUP-001',
      supplyDate: new Date('2025-01-16'),
      supplyCategory: SupplyCategory.FREE,
      details: [
        {
          itemCode: 'PRESS-001',
          supplyQuantity: 100,
          supplyUnitPrice: 200,
          supplyAmount: 20000,
        },
      ],
    })

    // 入荷データ
    await receivingRepository.create({
      receivingNumber: 'RCV-2025-001',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      receivingDate: new Date('2025-01-25'),
      itemCode: 'PLATED-001',
      receivedQuantity: 100,
    })
  }
})
