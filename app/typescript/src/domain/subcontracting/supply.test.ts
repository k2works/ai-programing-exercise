import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { SupplyRepository, SupplyCategory } from './supply.repository'
import { PurchaseOrderRepository } from '../purchase/purchase-order.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'
import { SupplierRepository } from '../supplier/supplier.repository'

describe('支給業務', () => {
  let testDb: TestDatabase
  let supplyRepository: SupplyRepository
  let purchaseOrderRepository: PurchaseOrderRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository
  let supplierRepository: SupplierRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    supplyRepository = new SupplyRepository(testDb.prisma!)
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

  test('支給データを登録できる', async () => {
    // Arrange: 取引先、単位、品目、発注データを作成
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '外注先A',
      supplierType: 'VENDOR',
    })

    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PRESS-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: 'プレス部品',
      itemCategory: 'PART',
      unitCode: 'PC',
    })

    await itemRepository.create({
      itemCode: 'PLATED-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: 'メッキ加工品',
      itemCategory: 'SEMI_PRODUCT',
      unitCode: 'PC',
    })

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

    // Act: 支給データを登録
    const supply = await supplyRepository.create({
      supplyNumber: 'SUP-2025-001',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      supplierCode: 'SUP-001',
      supplyDate: new Date('2025-01-16'),
      supplierPersonCode: 'EMP-001',
      supplyCategory: SupplyCategory.FREE,
      details: [
        {
          itemCode: 'PRESS-001',
          supplyQuantity: 100,
          supplyUnitPrice: 200,
          supplyAmount: 20000,
        },
      ],
      createdBy: 'user01',
    })

    // Assert
    expect(supply.id).toBeDefined()
    expect(supply.supplyNumber).toBe('SUP-2025-001')
    expect(supply.orderNumber).toBe('PO-2025-001')
    expect(supply.lineNumber).toBe(1)
    expect(supply.supplierCode).toBe('SUP-001')
    expect(supply.supplyCategory).toBe(SupplyCategory.FREE)
    expect(supply.details).toHaveLength(1)
    expect(supply.details[0].itemCode).toBe('PRESS-001')
    expect(Number(supply.details[0].supplyQuantity)).toBe(100)
    expect(Number(supply.details[0].supplyUnitPrice)).toBe(200)
    expect(Number(supply.details[0].supplyAmount)).toBe(20000)
  })

  test('有償支給データを登録できる', async () => {
    // Arrange
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '外注先A',
      supplierType: 'VENDOR',
    })

    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PRESS-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: 'プレス部品',
      itemCategory: 'PART',
      unitCode: 'PC',
    })

    await itemRepository.create({
      itemCode: 'PLATED-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: 'メッキ加工品',
      itemCategory: 'SEMI_PRODUCT',
      unitCode: 'PC',
    })

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

    // Act: 有償支給データを登録
    const supply = await supplyRepository.create({
      supplyNumber: 'SUP-2025-001',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      supplierCode: 'SUP-001',
      supplyDate: new Date('2025-01-16'),
      supplyCategory: SupplyCategory.PAID, // 有償支給
      details: [
        {
          itemCode: 'PRESS-001',
          supplyQuantity: 100,
          supplyUnitPrice: 200,
          supplyAmount: 20000,
        },
      ],
    })

    // Assert
    expect(supply.supplyCategory).toBe(SupplyCategory.PAID)
  })

  test('支給番号で支給データを検索できる', async () => {
    // Arrange
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '外注先A',
      supplierType: 'VENDOR',
    })

    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PRESS-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: 'プレス部品',
      itemCategory: 'PART',
      unitCode: 'PC',
    })

    await itemRepository.create({
      itemCode: 'PLATED-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: 'メッキ加工品',
      itemCategory: 'SEMI_PRODUCT',
      unitCode: 'PC',
    })

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

    await supplyRepository.create({
      supplyNumber: 'SUP-2025-001',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      supplierCode: 'SUP-001',
      supplyDate: new Date('2025-01-16'),
      details: [
        {
          itemCode: 'PRESS-001',
          supplyQuantity: 100,
          supplyUnitPrice: 200,
          supplyAmount: 20000,
        },
      ],
    })

    // Act
    const supply = await supplyRepository.findByNumber('SUP-2025-001')

    // Assert
    expect(supply).toBeDefined()
    expect(supply!.supplyNumber).toBe('SUP-2025-001')
    expect(supply!.details).toHaveLength(1)
    expect(Number(supply!.details[0].supplyQuantity)).toBe(100)
  })

  test('発注明細で支給データを検索できる', async () => {
    // Arrange
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '外注先A',
      supplierType: 'VENDOR',
    })

    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PRESS-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: 'プレス部品',
      itemCategory: 'PART',
      unitCode: 'PC',
    })

    await itemRepository.create({
      itemCode: 'PLATED-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: 'メッキ加工品',
      itemCategory: 'SEMI_PRODUCT',
      unitCode: 'PC',
    })

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

    // 2回に分けて支給
    await supplyRepository.create({
      supplyNumber: 'SUP-2025-001',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      supplierCode: 'SUP-001',
      supplyDate: new Date('2025-01-16'),
      details: [
        {
          itemCode: 'PRESS-001',
          supplyQuantity: 60,
          supplyUnitPrice: 200,
          supplyAmount: 12000,
        },
      ],
    })

    await supplyRepository.create({
      supplyNumber: 'SUP-2025-002',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      supplierCode: 'SUP-001',
      supplyDate: new Date('2025-01-17'),
      details: [
        {
          itemCode: 'PRESS-001',
          supplyQuantity: 40,
          supplyUnitPrice: 200,
          supplyAmount: 8000,
        },
      ],
    })

    // Act
    const supplies = await supplyRepository.findByOrderDetail('PO-2025-001', 1)

    // Assert
    expect(supplies).toHaveLength(2)
    expect(supplies[0].supplyNumber).toBe('SUP-2025-001')
    expect(supplies[1].supplyNumber).toBe('SUP-2025-002')
  })
})
