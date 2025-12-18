import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { ReceivingRepository, ReceivingCategory } from './receiving.repository'
import { PurchaseOrderRepository } from '../purchase/purchase-order.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'
import { SupplierRepository } from '../supplier/supplier.repository'

describe('入荷受入業務', () => {
  let testDb: TestDatabase
  let receivingRepository: ReceivingRepository
  let purchaseOrderRepository: PurchaseOrderRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository
  let supplierRepository: SupplierRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
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

  test('入荷受入データを登録できる', async () => {
    // Arrange: 発注明細を作成
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '取引先A',
      supplierType: 'VENDOR',
    })

    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PART-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '部品A',
      itemCategory: 'PART',
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
      itemCode: 'PART-001',
      scheduledReceiptDate: new Date('2025-01-20'),
      orderUnitPrice: 1000,
      orderQuantity: 100,
    })

    // Act: 入荷受入データを登録
    const receiving = await receivingRepository.create({
      receivingNumber: 'RCV-2025-001',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      receivingDate: new Date('2025-01-20'),
      receiverCode: 'EMP-001',
      receivingCategory: ReceivingCategory.NORMAL,
      itemCode: 'PART-001',
      receivedQuantity: 100,
      note: '全量入荷',
      createdBy: 'user01',
    })

    // Assert
    expect(receiving.id).toBeDefined()
    expect(receiving.receivingNumber).toBe('RCV-2025-001')
    expect(receiving.orderNumber).toBe('PO-2025-001')
    expect(receiving.lineNumber).toBe(1)
    expect(Number(receiving.receivedQuantity)).toBe(100)
    expect(receiving.receivingCategory).toBe(ReceivingCategory.NORMAL)
  })

  test('分割入荷を登録できる', async () => {
    // Arrange
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '取引先A',
      supplierType: 'VENDOR',
    })

    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PART-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '部品A',
      itemCategory: 'PART',
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
      itemCode: 'PART-001',
      scheduledReceiptDate: new Date('2025-01-20'),
      orderUnitPrice: 1000,
      orderQuantity: 100,
    })

    // Act: 分割入荷を2回登録
    await receivingRepository.create({
      receivingNumber: 'RCV-2025-001',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      receivingDate: new Date('2025-01-20'),
      receivingCategory: ReceivingCategory.SPLIT,
      itemCode: 'PART-001',
      receivedQuantity: 60,
      note: '1回目分割入荷',
    })

    await receivingRepository.create({
      receivingNumber: 'RCV-2025-002',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      receivingDate: new Date('2025-01-22'),
      receivingCategory: ReceivingCategory.SPLIT,
      itemCode: 'PART-001',
      receivedQuantity: 40,
      note: '2回目分割入荷',
    })

    // 発注明細の入荷履歴を取得
    const receivings = await receivingRepository.findByOrderDetail('PO-2025-001', 1)

    // Assert
    expect(receivings).toHaveLength(2)
    expect(receivings.map((r) => Number(r.receivedQuantity))).toEqual([60, 40])
    expect(receivings.every((r) => r.receivingCategory === ReceivingCategory.SPLIT)).toBe(true)
  })

  test('入荷番号で入荷受入データを検索できる', async () => {
    // Arrange
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '取引先A',
      supplierType: 'VENDOR',
    })

    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PART-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '部品A',
      itemCategory: 'PART',
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
      itemCode: 'PART-001',
      scheduledReceiptDate: new Date('2025-01-20'),
      orderUnitPrice: 1000,
      orderQuantity: 100,
    })

    await receivingRepository.create({
      receivingNumber: 'RCV-2025-001',
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      receivingDate: new Date('2025-01-20'),
      itemCode: 'PART-001',
      receivedQuantity: 100,
    })

    // Act
    const receiving = await receivingRepository.findByNumber('RCV-2025-001')

    // Assert
    expect(receiving).toBeDefined()
    expect(receiving!.receivingNumber).toBe('RCV-2025-001')
    expect(Number(receiving!.receivedQuantity)).toBe(100)
  })
})
