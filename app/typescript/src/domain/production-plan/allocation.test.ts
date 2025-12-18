import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { AllocationRepository, AllocationCategory } from './allocation.repository'
import { RequirementRepository } from './requirement.repository'
import { OrderRepository, OrderType } from './order.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'

describe('引当情報', () => {
  let testDb: TestDatabase
  let allocationRepository: AllocationRepository
  let requirementRepository: RequirementRepository
  let orderRepository: OrderRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    allocationRepository = new AllocationRepository(testDb.prisma!)
    requirementRepository = new RequirementRepository(testDb.prisma!)
    orderRepository = new OrderRepository(testDb.prisma!)
    itemRepository = new ItemRepository(testDb.prisma!)
    unitRepository = new UnitRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('在庫引当を登録できる', async () => {
    // Arrange: 単位、品目、オーダ、所要を作成
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

    const order = await orderRepository.create({
      orderNumber: 'MO-2025-001',
      orderType: OrderType.MANUFACTURING,
      itemCode: 'PART-001',
      startDate: new Date('2025-01-15'),
      dueDate: new Date('2025-01-20'),
      plannedQuantity: 100,
      locationCode: 'WH-001',
    })

    const requirement = await requirementRepository.create({
      requirementNumber: 'REQ-2025-001',
      orderId: order.id,
      itemCode: 'PART-001',
      dueDate: new Date('2025-01-15'),
      requiredQuantity: 200,
      locationCode: 'WH-001',
    })

    // Act: 在庫から引当
    const allocation = await allocationRepository.create({
      requirementId: requirement.id,
      allocationCategory: AllocationCategory.INVENTORY,
      allocationDate: new Date('2025-01-10'),
      allocatedQuantity: 50,
      locationCode: 'WH-001',
    })

    // Assert
    expect(allocation.id).toBeDefined()
    expect(allocation.requirementId).toBe(requirement.id)
    expect(allocation.allocationCategory).toBe(AllocationCategory.INVENTORY)
    expect(Number(allocation.allocatedQuantity)).toBe(50)
    expect(allocation.orderId).toBeNull()
  })

  test('発注残から引当を登録できる', async () => {
    // Arrange
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

    // 購買オーダ（引当元）
    const purchaseOrder = await orderRepository.create({
      orderNumber: 'PO-2025-001',
      orderType: OrderType.PURCHASE,
      itemCode: 'PART-001',
      startDate: new Date('2025-01-10'),
      dueDate: new Date('2025-01-15'),
      plannedQuantity: 300,
      locationCode: 'WH-001',
    })

    // 製造オーダ
    const manufacturingOrder = await orderRepository.create({
      orderNumber: 'MO-2025-001',
      orderType: OrderType.MANUFACTURING,
      itemCode: 'PART-001',
      startDate: new Date('2025-01-15'),
      dueDate: new Date('2025-01-20'),
      plannedQuantity: 100,
      locationCode: 'WH-001',
    })

    const requirement = await requirementRepository.create({
      requirementNumber: 'REQ-2025-001',
      orderId: manufacturingOrder.id,
      itemCode: 'PART-001',
      dueDate: new Date('2025-01-15'),
      requiredQuantity: 200,
      locationCode: 'WH-001',
    })

    // Act: 発注残から引当
    const allocation = await allocationRepository.create({
      requirementId: requirement.id,
      allocationCategory: AllocationCategory.PURCHASE_REMAIN,
      orderId: purchaseOrder.id,
      allocationDate: new Date('2025-01-10'),
      allocatedQuantity: 200,
      locationCode: 'WH-001',
    })

    // Assert
    expect(allocation.allocationCategory).toBe(AllocationCategory.PURCHASE_REMAIN)
    expect(allocation.orderId).toBe(purchaseOrder.id)
    expect(Number(allocation.allocatedQuantity)).toBe(200)
  })

  test('所要IDで引当情報を検索できる', async () => {
    // Arrange
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

    const order = await orderRepository.create({
      orderNumber: 'MO-2025-001',
      orderType: OrderType.MANUFACTURING,
      itemCode: 'PART-001',
      startDate: new Date('2025-01-15'),
      dueDate: new Date('2025-01-20'),
      plannedQuantity: 100,
      locationCode: 'WH-001',
    })

    const requirement = await requirementRepository.create({
      requirementNumber: 'REQ-2025-001',
      orderId: order.id,
      itemCode: 'PART-001',
      dueDate: new Date('2025-01-15'),
      requiredQuantity: 200,
      locationCode: 'WH-001',
    })

    // 複数の引当を作成
    await allocationRepository.create({
      requirementId: requirement.id,
      allocationCategory: AllocationCategory.INVENTORY,
      allocationDate: new Date('2025-01-10'),
      allocatedQuantity: 50,
      locationCode: 'WH-001',
    })

    await allocationRepository.create({
      requirementId: requirement.id,
      allocationCategory: AllocationCategory.MANUFACTURING_REMAIN,
      allocationDate: new Date('2025-01-11'),
      allocatedQuantity: 100,
      locationCode: 'WH-001',
    })

    // Act
    const allocations = await allocationRepository.findByRequirementId(requirement.id)

    // Assert
    expect(allocations).toHaveLength(2)
    const categories = allocations.map((a) => a.allocationCategory)
    expect(categories).toContain(AllocationCategory.INVENTORY)
    expect(categories).toContain(AllocationCategory.MANUFACTURING_REMAIN)
  })
})
