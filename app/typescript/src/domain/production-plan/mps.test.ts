import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { MPSRepository, PlanStatus } from './mps.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'

describe('基準生産計画（MPS）', () => {
  let testDb: TestDatabase
  let mpsRepository: MPSRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
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

  test('基準生産計画を登録できる', async () => {
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

    // Act: MPSを登録
    const mps = await mpsRepository.create({
      mpsNumber: 'MPS-2025-001',
      planDate: new Date('2025-01-10'),
      itemCode: 'PROD-001',
      plannedQuantity: 100,
      dueDate: new Date('2025-01-20'),
      status: PlanStatus.DRAFT,
      locationCode: 'WH-001',
      note: '1月度生産計画',
      createdBy: 'user01',
    })

    // Assert
    expect(mps.id).toBeDefined()
    expect(mps.mpsNumber).toBe('MPS-2025-001')
    expect(mps.itemCode).toBe('PROD-001')
    expect(Number(mps.plannedQuantity)).toBe(100)
    expect(mps.status).toBe(PlanStatus.DRAFT)
    expect(mps.locationCode).toBe('WH-001')
    expect(mps.createdBy).toBe('user01')
  })

  test('MPS番号で検索できる', async () => {
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

    await mpsRepository.create({
      mpsNumber: 'MPS-2025-001',
      planDate: new Date('2025-01-10'),
      itemCode: 'PROD-001',
      plannedQuantity: 100,
      dueDate: new Date('2025-01-20'),
    })

    // Act
    const mps = await mpsRepository.findByNumber('MPS-2025-001')

    // Assert
    expect(mps).toBeDefined()
    expect(mps!.mpsNumber).toBe('MPS-2025-001')
    expect(Number(mps!.plannedQuantity)).toBe(100)
  })

  test('ステータスで検索できる', async () => {
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
      itemCode: 'PROD-002',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '製品B',
      itemCategory: 'PRODUCT',
      unitCode: 'PC',
    })

    // 草案のMPSを2つ作成
    await mpsRepository.create({
      mpsNumber: 'MPS-2025-001',
      planDate: new Date('2025-01-10'),
      itemCode: 'PROD-001',
      plannedQuantity: 100,
      dueDate: new Date('2025-01-20'),
      status: PlanStatus.DRAFT,
    })

    await mpsRepository.create({
      mpsNumber: 'MPS-2025-002',
      planDate: new Date('2025-01-10'),
      itemCode: 'PROD-002',
      plannedQuantity: 200,
      dueDate: new Date('2025-01-25'),
      status: PlanStatus.DRAFT,
    })

    // 確定のMPSを1つ作成
    await mpsRepository.create({
      mpsNumber: 'MPS-2025-003',
      planDate: new Date('2025-01-10'),
      itemCode: 'PROD-001',
      plannedQuantity: 150,
      dueDate: new Date('2025-01-30'),
      status: PlanStatus.CONFIRMED,
    })

    // Act: 草案のMPSを検索
    const draftMPSs = await mpsRepository.findByStatus(PlanStatus.DRAFT)

    // Assert
    expect(draftMPSs).toHaveLength(2)
    expect(draftMPSs.map((m) => m.mpsNumber).sort()).toEqual(['MPS-2025-001', 'MPS-2025-002'])
  })

  test('MPSのステータスを更新できる', async () => {
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

    const mps = await mpsRepository.create({
      mpsNumber: 'MPS-2025-001',
      planDate: new Date('2025-01-10'),
      itemCode: 'PROD-001',
      plannedQuantity: 100,
      dueDate: new Date('2025-01-20'),
      status: PlanStatus.DRAFT,
    })

    // Act: 草案から確定に更新
    const updated = await mpsRepository.updateStatus(mps.id, PlanStatus.CONFIRMED, 'user01')

    // Assert
    expect(updated.status).toBe(PlanStatus.CONFIRMED)
    expect(updated.updatedBy).toBe('user01')
  })
})
