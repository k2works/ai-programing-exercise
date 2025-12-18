import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { WorkOrderRepository, WorkOrderStatus } from './work-order.repository'
import { ProcessMasterRepository } from './process.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'

describe('作業指示業務', () => {
  let testDb: TestDatabase
  let workOrderRepository: WorkOrderRepository
  let processMasterRepository: ProcessMasterRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    workOrderRepository = new WorkOrderRepository(testDb.prisma!)
    processMasterRepository = new ProcessMasterRepository(testDb.prisma!)
    itemRepository = new ItemRepository(testDb.prisma!)
    unitRepository = new UnitRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  async function setupTestData() {
    // 単位
    await unitRepository.create({
      unitCode: 'PC',
      unitSymbol: '個',
      unitName: '個数',
    })

    // 品目
    await itemRepository.create({
      itemCode: 'PRODUCT-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '製品A',
      itemCategory: 'PRODUCT',
      unitCode: 'PC',
    })

    // 工程マスタ
    await processMasterRepository.create({
      processCode: 'PROC-001',
      processName: 'プレス加工',
      standardCycleTime: 5.5,
      setupTime: 30.0,
    })

    await processMasterRepository.create({
      processCode: 'PROC-002',
      processName: 'メッキ加工',
      standardCycleTime: 10.0,
      setupTime: 60.0,
    })
  }

  test('作業指示データを登録できる', async () => {
    // Arrange
    await setupTestData()

    // Act
    const workOrder = await workOrderRepository.create({
      workOrderNumber: 'WO-2025-001',
      productionOrderNumber: 'PO-2025-001',
      workOrderDate: new Date('2025-01-20'),
      itemCode: 'PRODUCT-001',
      orderQuantity: 100,
      scheduledStartDate: new Date('2025-01-21'),
      scheduledEndDate: new Date('2025-01-25'),
      details: [
        {
          sequence: 1,
          processCode: 'PROC-001',
          scheduledStartTime: new Date('2025-01-21T09:00:00'),
          scheduledEndTime: new Date('2025-01-22T17:00:00'),
        },
        {
          sequence: 2,
          processCode: 'PROC-002',
          scheduledStartTime: new Date('2025-01-23T09:00:00'),
          scheduledEndTime: new Date('2025-01-25T17:00:00'),
        },
      ],
      createdBy: 'user01',
    })

    // Assert
    expect(workOrder.id).toBeDefined()
    expect(workOrder.workOrderNumber).toBe('WO-2025-001')
    expect(workOrder.productionOrderNumber).toBe('PO-2025-001')
    expect(workOrder.itemCode).toBe('PRODUCT-001')
    expect(Number(workOrder.orderQuantity)).toBe(100)
    expect(Number(workOrder.completedQuantity)).toBe(0)
    expect(workOrder.status).toBe(WorkOrderStatus.NOT_STARTED)
    expect(workOrder.details).toHaveLength(2)
    expect(workOrder.details[0].sequence).toBe(1)
    expect(workOrder.details[0].processCode).toBe('PROC-001')
    expect(workOrder.details[1].sequence).toBe(2)
    expect(workOrder.details[1].processCode).toBe('PROC-002')
  })

  test('作業指示番号で作業指示データを検索できる', async () => {
    // Arrange
    await setupTestData()

    await workOrderRepository.create({
      workOrderNumber: 'WO-2025-001',
      productionOrderNumber: 'PO-2025-001',
      workOrderDate: new Date('2025-01-20'),
      itemCode: 'PRODUCT-001',
      orderQuantity: 100,
      scheduledStartDate: new Date('2025-01-21'),
      scheduledEndDate: new Date('2025-01-25'),
      details: [
        {
          sequence: 1,
          processCode: 'PROC-001',
        },
      ],
    })

    // Act
    const workOrder = await workOrderRepository.findByNumber('WO-2025-001')

    // Assert
    expect(workOrder).toBeDefined()
    expect(workOrder!.workOrderNumber).toBe('WO-2025-001')
    expect(workOrder!.details).toHaveLength(1)
  })

  test('製造オーダ番号で作業指示データを検索できる', async () => {
    // Arrange
    await setupTestData()

    await workOrderRepository.create({
      workOrderNumber: 'WO-2025-001',
      productionOrderNumber: 'PO-2025-001',
      workOrderDate: new Date('2025-01-20'),
      itemCode: 'PRODUCT-001',
      orderQuantity: 100,
      scheduledStartDate: new Date('2025-01-21'),
      scheduledEndDate: new Date('2025-01-25'),
      details: [
        {
          sequence: 1,
          processCode: 'PROC-001',
        },
      ],
    })

    await workOrderRepository.create({
      workOrderNumber: 'WO-2025-002',
      productionOrderNumber: 'PO-2025-001',
      workOrderDate: new Date('2025-01-21'),
      itemCode: 'PRODUCT-001',
      orderQuantity: 50,
      scheduledStartDate: new Date('2025-01-26'),
      scheduledEndDate: new Date('2025-01-30'),
      details: [
        {
          sequence: 1,
          processCode: 'PROC-001',
        },
      ],
    })

    // Act
    const workOrders = await workOrderRepository.findByProductionOrder('PO-2025-001')

    // Assert
    expect(workOrders).toHaveLength(2)
    expect(workOrders[0].workOrderNumber).toBe('WO-2025-001')
    expect(workOrders[1].workOrderNumber).toBe('WO-2025-002')
  })

  test('作業指示のステータスを更新できる', async () => {
    // Arrange
    await setupTestData()

    await workOrderRepository.create({
      workOrderNumber: 'WO-2025-001',
      productionOrderNumber: 'PO-2025-001',
      workOrderDate: new Date('2025-01-20'),
      itemCode: 'PRODUCT-001',
      orderQuantity: 100,
      scheduledStartDate: new Date('2025-01-21'),
      scheduledEndDate: new Date('2025-01-25'),
      details: [
        {
          sequence: 1,
          processCode: 'PROC-001',
        },
      ],
    })

    // Act
    await workOrderRepository.updateStatus('WO-2025-001', WorkOrderStatus.IN_PROGRESS)

    // Assert
    const workOrder = await workOrderRepository.findByNumber('WO-2025-001')
    expect(workOrder!.status).toBe(WorkOrderStatus.IN_PROGRESS)
  })

  test('完成数量を更新できる', async () => {
    // Arrange
    await setupTestData()

    await workOrderRepository.create({
      workOrderNumber: 'WO-2025-001',
      productionOrderNumber: 'PO-2025-001',
      workOrderDate: new Date('2025-01-20'),
      itemCode: 'PRODUCT-001',
      orderQuantity: 100,
      scheduledStartDate: new Date('2025-01-21'),
      scheduledEndDate: new Date('2025-01-25'),
      details: [
        {
          sequence: 1,
          processCode: 'PROC-001',
        },
      ],
    })

    // Act
    await workOrderRepository.updateCompletedQuantity('WO-2025-001', 50)

    // Assert
    const workOrder = await workOrderRepository.findByNumber('WO-2025-001')
    expect(Number(workOrder!.completedQuantity)).toBe(50)
  })

  test('作業中ステータスで作業指示を登録できる', async () => {
    // Arrange
    await setupTestData()

    // Act
    const workOrder = await workOrderRepository.create({
      workOrderNumber: 'WO-2025-001',
      productionOrderNumber: 'PO-2025-001',
      workOrderDate: new Date('2025-01-20'),
      itemCode: 'PRODUCT-001',
      orderQuantity: 100,
      scheduledStartDate: new Date('2025-01-21'),
      scheduledEndDate: new Date('2025-01-25'),
      status: WorkOrderStatus.IN_PROGRESS,
      details: [
        {
          sequence: 1,
          processCode: 'PROC-001',
        },
      ],
    })

    // Assert
    expect(workOrder.status).toBe(WorkOrderStatus.IN_PROGRESS)
  })
})
