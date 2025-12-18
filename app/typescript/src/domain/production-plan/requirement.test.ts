import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { RequirementRepository } from './requirement.repository'
import { OrderRepository, OrderType } from './order.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'

describe('所要情報', () => {
  let testDb: TestDatabase
  let requirementRepository: RequirementRepository
  let orderRepository: OrderRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
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

  test('所要情報を登録できる', async () => {
    // Arrange: 単位、品目、オーダを作成
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

    // Act: 所要情報を登録
    const requirement = await requirementRepository.create({
      requirementNumber: 'REQ-2025-001',
      orderId: order.id,
      itemCode: 'PART-001',
      dueDate: new Date('2025-01-15'),
      requiredQuantity: 200,
      locationCode: 'WH-001',
    })

    // Assert
    expect(requirement.id).toBeDefined()
    expect(requirement.requirementNumber).toBe('REQ-2025-001')
    expect(requirement.orderId).toBe(order.id)
    expect(Number(requirement.requiredQuantity)).toBe(200)
    expect(Number(requirement.allocatedQuantity)).toBe(0)
    expect(Number(requirement.shortageQuantity)).toBe(0)
  })

  test('オーダIDで所要情報を検索できる', async () => {
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

    await itemRepository.create({
      itemCode: 'PART-002',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '部品B',
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

    // 同じオーダに対する複数の所要を作成
    await requirementRepository.create({
      requirementNumber: 'REQ-2025-001',
      orderId: order.id,
      itemCode: 'PART-001',
      dueDate: new Date('2025-01-15'),
      requiredQuantity: 200,
      locationCode: 'WH-001',
    })

    await requirementRepository.create({
      requirementNumber: 'REQ-2025-002',
      orderId: order.id,
      itemCode: 'PART-002',
      dueDate: new Date('2025-01-15'),
      requiredQuantity: 300,
      locationCode: 'WH-001',
    })

    // Act
    const requirements = await requirementRepository.findByOrderId(order.id)

    // Assert
    expect(requirements).toHaveLength(2)
    expect(requirements.map((r) => r.requirementNumber).sort()).toEqual([
      'REQ-2025-001',
      'REQ-2025-002',
    ])
  })

  test('引当数量と不足数量を更新できる', async () => {
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

    // Act: 引当数量100、不足数量100に更新
    const updated = await requirementRepository.updateAllocated(requirement.id, 100, 100)

    // Assert
    expect(Number(updated.allocatedQuantity)).toBe(100)
    expect(Number(updated.shortageQuantity)).toBe(100)
  })
})
