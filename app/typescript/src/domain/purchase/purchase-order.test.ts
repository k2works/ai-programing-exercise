import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import {
  PurchaseOrderRepository,
  PurchaseOrderStatus,
} from './purchase-order.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'
import { SupplierRepository } from '../supplier/supplier.repository'

describe('発注業務', () => {
  let testDb: TestDatabase
  let purchaseOrderRepository: PurchaseOrderRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository
  let supplierRepository: SupplierRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
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

  test('発注データを登録できる', async () => {
    // Arrange: 取引先を作成
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '取引先A',
      supplierType: 'VENDOR',
    })

    // Act: 発注データを登録
    const purchaseOrder = await purchaseOrderRepository.create({
      orderNumber: 'PO-2025-001',
      orderDate: new Date('2025-01-10'),
      supplierCode: 'SUP-001',
      purchaserCode: 'EMP-001',
      departmentCode: 'DEPT-001',
      status: PurchaseOrderStatus.DRAFT,
      note: '1月度部品発注',
      createdBy: 'user01',
    })

    // Assert
    expect(purchaseOrder.id).toBeDefined()
    expect(purchaseOrder.orderNumber).toBe('PO-2025-001')
    expect(purchaseOrder.supplierCode).toBe('SUP-001')
    expect(purchaseOrder.status).toBe(PurchaseOrderStatus.DRAFT)
    expect(purchaseOrder.note).toBe('1月度部品発注')
  })

  test('発注番号で発注データを検索できる', async () => {
    // Arrange
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '取引先A',
      supplierType: 'VENDOR',
    })

    await purchaseOrderRepository.create({
      orderNumber: 'PO-2025-001',
      orderDate: new Date('2025-01-10'),
      supplierCode: 'SUP-001',
    })

    // Act
    const purchaseOrder = await purchaseOrderRepository.findByNumber('PO-2025-001')

    // Assert
    expect(purchaseOrder).toBeDefined()
    expect(purchaseOrder!.orderNumber).toBe('PO-2025-001')
  })

  test('発注ステータスを更新できる', async () => {
    // Arrange
    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '取引先A',
      supplierType: 'VENDOR',
    })

    const purchaseOrder = await purchaseOrderRepository.create({
      orderNumber: 'PO-2025-001',
      orderDate: new Date('2025-01-10'),
      supplierCode: 'SUP-001',
      status: PurchaseOrderStatus.DRAFT,
    })

    // Act: 草案から発注済に更新
    const updated = await purchaseOrderRepository.updateStatus(
      purchaseOrder.id,
      PurchaseOrderStatus.ORDERED,
      'user01'
    )

    // Assert
    expect(updated.status).toBe(PurchaseOrderStatus.ORDERED)
    expect(updated.updatedBy).toBe('user01')
  })

  test('発注明細を登録できる', async () => {
    // Arrange: 取引先、単位、品目、発注データを作成
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

    // Act: 発注明細を登録
    const detail = await purchaseOrderRepository.createDetail({
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      itemCode: 'PART-001',
      scheduledReceiptDate: new Date('2025-01-20'),
      orderUnitPrice: 1000,
      orderQuantity: 100,
      createdBy: 'user01',
    })

    // Assert
    expect(detail.id).toBeDefined()
    expect(detail.orderNumber).toBe('PO-2025-001')
    expect(detail.lineNumber).toBe(1)
    expect(detail.itemCode).toBe('PART-001')
    expect(Number(detail.orderUnitPrice)).toBe(1000)
    expect(Number(detail.orderQuantity)).toBe(100)
    expect(Number(detail.orderAmount)).toBe(100000) // 1000 × 100
  })

  test('発注番号で発注明細一覧を取得できる', async () => {
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

    await itemRepository.create({
      itemCode: 'PART-002',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '部品B',
      itemCategory: 'PART',
      unitCode: 'PC',
    })

    await purchaseOrderRepository.create({
      orderNumber: 'PO-2025-001',
      orderDate: new Date('2025-01-10'),
      supplierCode: 'SUP-001',
    })

    // 複数の明細を登録
    await purchaseOrderRepository.createDetail({
      orderNumber: 'PO-2025-001',
      lineNumber: 1,
      itemCode: 'PART-001',
      scheduledReceiptDate: new Date('2025-01-20'),
      orderUnitPrice: 1000,
      orderQuantity: 100,
    })

    await purchaseOrderRepository.createDetail({
      orderNumber: 'PO-2025-001',
      lineNumber: 2,
      itemCode: 'PART-002',
      scheduledReceiptDate: new Date('2025-01-20'),
      orderUnitPrice: 500,
      orderQuantity: 200,
    })

    // Act
    const details = await purchaseOrderRepository.findDetailsByOrderNumber('PO-2025-001')

    // Assert
    expect(details).toHaveLength(2)
    expect(details.map((d) => d.lineNumber).sort()).toEqual([1, 2])
    expect(details.map((d) => d.itemCode).sort()).toEqual(['PART-001', 'PART-002'])
  })
})
