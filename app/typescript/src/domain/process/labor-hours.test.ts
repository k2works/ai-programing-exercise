import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { LaborHoursRepository } from './labor-hours.repository'
import { WorkOrderRepository } from './work-order.repository'
import { ProcessMasterRepository } from './process.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'

describe('工数実績業務', () => {
  let testDb: TestDatabase
  let laborHoursRepository: LaborHoursRepository
  let workOrderRepository: WorkOrderRepository
  let processMasterRepository: ProcessMasterRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    laborHoursRepository = new LaborHoursRepository(testDb.prisma!)
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
    })

    await processMasterRepository.create({
      processCode: 'PROC-002',
      processName: 'メッキ加工',
    })

    // 作業指示
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
        {
          sequence: 2,
          processCode: 'PROC-002',
        },
      ],
    })
  }

  test('工数実績データを登録できる', async () => {
    // Arrange
    await setupTestData()

    // Act
    const laborHours = await laborHoursRepository.create({
      laborHoursNumber: 'LH-2025-001',
      workOrderNumber: 'WO-2025-001',
      sequence: 1,
      processCode: 'PROC-001',
      workDate: new Date('2025-01-21'),
      employeeCode: 'EMP-001',
      startTime: new Date('2025-01-21T09:00:00'),
      endTime: new Date('2025-01-21T17:00:00'),
      workMinutes: 480, // 8時間（休憩1時間を除く）
      createdBy: 'user01',
    })

    // Assert
    expect(laborHours.id).toBeDefined()
    expect(laborHours.laborHoursNumber).toBe('LH-2025-001')
    expect(laborHours.workOrderNumber).toBe('WO-2025-001')
    expect(laborHours.sequence).toBe(1)
    expect(laborHours.processCode).toBe('PROC-001')
    expect(laborHours.employeeCode).toBe('EMP-001')
    expect(Number(laborHours.workMinutes)).toBe(480)
  })

  test('工数実績番号で工数実績データを検索できる', async () => {
    // Arrange
    await setupTestData()

    await laborHoursRepository.create({
      laborHoursNumber: 'LH-2025-001',
      workOrderNumber: 'WO-2025-001',
      sequence: 1,
      processCode: 'PROC-001',
      workDate: new Date('2025-01-21'),
      employeeCode: 'EMP-001',
      startTime: new Date('2025-01-21T09:00:00'),
      endTime: new Date('2025-01-21T17:00:00'),
      workMinutes: 480,
    })

    // Act
    const laborHours = await laborHoursRepository.findByNumber('LH-2025-001')

    // Assert
    expect(laborHours).toBeDefined()
    expect(laborHours!.laborHoursNumber).toBe('LH-2025-001')
    expect(Number(laborHours!.workMinutes)).toBe(480)
  })

  test('作業指示番号で工数実績データを検索できる', async () => {
    // Arrange
    await setupTestData()

    // 工順1の工数実績
    await laborHoursRepository.create({
      laborHoursNumber: 'LH-2025-001',
      workOrderNumber: 'WO-2025-001',
      sequence: 1,
      processCode: 'PROC-001',
      workDate: new Date('2025-01-21'),
      employeeCode: 'EMP-001',
      startTime: new Date('2025-01-21T09:00:00'),
      endTime: new Date('2025-01-21T17:00:00'),
      workMinutes: 480,
    })

    // 工順2の工数実績
    await laborHoursRepository.create({
      laborHoursNumber: 'LH-2025-002',
      workOrderNumber: 'WO-2025-001',
      sequence: 2,
      processCode: 'PROC-002',
      workDate: new Date('2025-01-23'),
      employeeCode: 'EMP-002',
      startTime: new Date('2025-01-23T09:00:00'),
      endTime: new Date('2025-01-23T17:00:00'),
      workMinutes: 480,
    })

    // Act
    const laborHoursList = await laborHoursRepository.findByWorkOrder('WO-2025-001')

    // Assert
    expect(laborHoursList).toHaveLength(2)
    expect(laborHoursList[0].sequence).toBe(1)
    expect(laborHoursList[1].sequence).toBe(2)
  })

  test('担当者コードと期間で工数実績データを検索できる', async () => {
    // Arrange
    await setupTestData()

    await laborHoursRepository.create({
      laborHoursNumber: 'LH-2025-001',
      workOrderNumber: 'WO-2025-001',
      sequence: 1,
      processCode: 'PROC-001',
      workDate: new Date('2025-01-21'),
      employeeCode: 'EMP-001',
      startTime: new Date('2025-01-21T09:00:00'),
      endTime: new Date('2025-01-21T17:00:00'),
      workMinutes: 480,
    })

    await laborHoursRepository.create({
      laborHoursNumber: 'LH-2025-002',
      workOrderNumber: 'WO-2025-001',
      sequence: 1,
      processCode: 'PROC-001',
      workDate: new Date('2025-01-22'),
      employeeCode: 'EMP-001',
      startTime: new Date('2025-01-22T09:00:00'),
      endTime: new Date('2025-01-22T17:00:00'),
      workMinutes: 480,
    })

    // Act
    const laborHoursList = await laborHoursRepository.findByEmployee(
      'EMP-001',
      new Date('2025-01-20'),
      new Date('2025-01-31')
    )

    // Assert
    expect(laborHoursList).toHaveLength(2)
    expect(laborHoursList[0].employeeCode).toBe('EMP-001')
    expect(laborHoursList[1].employeeCode).toBe('EMP-001')
  })

  test('工順ごとの合計工数を計算できる', async () => {
    // Arrange
    await setupTestData()

    // 同じ工順で複数日作業
    await laborHoursRepository.create({
      laborHoursNumber: 'LH-2025-001',
      workOrderNumber: 'WO-2025-001',
      sequence: 1,
      processCode: 'PROC-001',
      workDate: new Date('2025-01-21'),
      employeeCode: 'EMP-001',
      startTime: new Date('2025-01-21T09:00:00'),
      endTime: new Date('2025-01-21T17:00:00'),
      workMinutes: 480, // 8時間
    })

    await laborHoursRepository.create({
      laborHoursNumber: 'LH-2025-002',
      workOrderNumber: 'WO-2025-001',
      sequence: 1,
      processCode: 'PROC-001',
      workDate: new Date('2025-01-22'),
      employeeCode: 'EMP-001',
      startTime: new Date('2025-01-22T09:00:00'),
      endTime: new Date('2025-01-22T12:00:00'),
      workMinutes: 180, // 3時間
    })

    // Act
    const totalMinutes = await laborHoursRepository.calculateTotalHours('WO-2025-001', 1)

    // Assert
    expect(totalMinutes).toBe(660) // 480 + 180 = 660分（11時間）
  })
})
