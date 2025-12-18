import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { OrderRepository, OrderType, PlanStatus } from './order.repository'
import { MPSRepository } from './mps.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'

describe('オーダ情報', () => {
  let testDb: TestDatabase
  let orderRepository: OrderRepository
  let mpsRepository: MPSRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    orderRepository = new OrderRepository(testDb.prisma!)
    mpsRepository = new MPSRepository(testDb.prisma!)
    itemRepository = new ItemRepository(testDb.prisma!)
    unitRepository = new UnitRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('製造オーダを登録できる', async () => {
    // Arrange: 単位と品目を作成
    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PROD-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '製品A',
      itemCategory: 'PRODUCT',
      unitCode: 'PC',
    })

    // Act: 製造オーダを登録
    const order = await orderRepository.create({
      orderNumber: 'MO-2025-001',
      orderType: OrderType.MANUFACTURING,
      itemCode: 'PROD-001',
      startDate: new Date('2025-01-15'),
      dueDate: new Date('2025-01-20'),
      plannedQuantity: 100,
      locationCode: 'WH-001',
      status: PlanStatus.CONFIRMED,
      createdBy: 'user01',
    })

    // Assert
    expect(order.id).toBeDefined()
    expect(order.orderNumber).toBe('MO-2025-001')
    expect(order.orderType).toBe(OrderType.MANUFACTURING)
    expect(order.itemCode).toBe('PROD-001')
    expect(Number(order.plannedQuantity)).toBe(100)
    expect(order.status).toBe(PlanStatus.CONFIRMED)
  })

  test('購買オーダを登録できる', async () => {
    // Arrange
    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PART-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '部品B',
      itemCategory: 'PART',
      unitCode: 'PC',
    })

    // Act: 購買オーダを登録
    const order = await orderRepository.create({
      orderNumber: 'PO-2025-001',
      orderType: OrderType.PURCHASE,
      itemCode: 'PART-001',
      startDate: new Date('2025-01-10'),
      dueDate: new Date('2025-01-15'),
      plannedQuantity: 200,
      locationCode: 'WH-001',
    })

    // Assert
    expect(order.orderNumber).toBe('PO-2025-001')
    expect(order.orderType).toBe(OrderType.PURCHASE)
    expect(Number(order.plannedQuantity)).toBe(200)
  })

  test('MPSに紐づくオーダを登録できる', async () => {
    // Arrange: 品目とMPSを作成
    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PROD-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '製品A',
      itemCategory: 'PRODUCT',
      unitCode: 'PC',
    })

    const mps = await mpsRepository.create({
      mpsNumber: 'MPS-2025-001',
      planDate: new Date('2025-01-10'),
      itemCode: 'PROD-001',
      plannedQuantity: 100,
      dueDate: new Date('2025-01-20'),
    })

    // Act: MPSに紐づくオーダを登録
    const order = await orderRepository.create({
      orderNumber: 'MO-2025-001',
      orderType: OrderType.MANUFACTURING,
      itemCode: 'PROD-001',
      startDate: new Date('2025-01-15'),
      dueDate: new Date('2025-01-20'),
      plannedQuantity: 100,
      locationCode: 'WH-001',
      mpsId: mps.id,
    })

    // Assert
    expect(order.mpsId).toBe(mps.id)
  })

  test('親子オーダの階層を登録できる', async () => {
    // Arrange
    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    await itemRepository.create({
      itemCode: 'PROD-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '製品A',
      itemCategory: 'PRODUCT',
      unitCode: 'PC',
    })

    await itemRepository.create({
      itemCode: 'PART-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '部品B',
      itemCategory: 'PART',
      unitCode: 'PC',
    })

    // 親オーダ（製品）
    const parentOrder = await orderRepository.create({
      orderNumber: 'MO-2025-001',
      orderType: OrderType.MANUFACTURING,
      itemCode: 'PROD-001',
      startDate: new Date('2025-01-15'),
      dueDate: new Date('2025-01-20'),
      plannedQuantity: 100,
      locationCode: 'WH-001',
    })

    // Act: 子オーダ（部品）を登録
    const childOrder = await orderRepository.create({
      orderNumber: 'PO-2025-001',
      orderType: OrderType.PURCHASE,
      itemCode: 'PART-001',
      startDate: new Date('2025-01-10'),
      dueDate: new Date('2025-01-15'),
      plannedQuantity: 200,
      locationCode: 'WH-001',
      parentOrderId: parentOrder.id,
    })

    // Assert
    expect(childOrder.parentOrderId).toBe(parentOrder.id)

    // 子オーダを検索
    const childOrders = await orderRepository.findChildOrders(parentOrder.id)
    expect(childOrders).toHaveLength(1)
    expect(childOrders[0].orderNumber).toBe('PO-2025-001')
  })
})
