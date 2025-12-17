import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { EmployeeRepository } from './employee.repository'
import { DepartmentRepository } from '../department/department.repository'

describe('担当者マスタ', () => {
  let testDb: TestDatabase
  let employeeRepository: EmployeeRepository
  let departmentRepository: DepartmentRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    employeeRepository = new EmployeeRepository(testDb.prisma!)
    departmentRepository = new DepartmentRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('担当者を登録できる', async () => {
    // Arrange: 部門を作成
    await departmentRepository.create({
      departmentCode: 'DEPT001',
      effectiveFrom: new Date('2025-01-01'),
      level: 1,
      departmentName: '製造部',
    })

    // Act
    const employee = await employeeRepository.create({
      employeeCode: 'EMP001',
      employeeName: '山田太郎',
      employeeNameKana: 'ヤマダタロウ',
      effectiveFrom: new Date('2025-01-01'),
      departmentCode: 'DEPT001',
    })

    // Assert
    expect(employee.employeeCode).toBe('EMP001')
    expect(employee.employeeName).toBe('山田太郎')
    expect(employee.departmentCode).toBe('DEPT001')
  })

  test('適用期間で有効な担当者を取得できる', async () => {
    // Arrange: 部門を作成
    await departmentRepository.create({
      departmentCode: 'DEPT001',
      effectiveFrom: new Date('2024-01-01'),
      level: 1,
      departmentName: '製造部',
    })

    await departmentRepository.create({
      departmentCode: 'DEPT002',
      effectiveFrom: new Date('2025-01-01'),
      level: 1,
      departmentName: '品質管理部',
    })

    // 異動履歴を持つ担当者
    await employeeRepository.create({
      employeeCode: 'EMP001',
      employeeName: '山田太郎',
      effectiveFrom: new Date('2024-01-01'),
      effectiveTo: new Date('2024-12-31'),
      departmentCode: 'DEPT001', // 製造部
    })

    await employeeRepository.create({
      employeeCode: 'EMP001',
      employeeName: '山田太郎',
      effectiveFrom: new Date('2025-01-01'),
      departmentCode: 'DEPT002', // 品質管理部に異動
    })

    // Act: 2025年時点の情報を取得
    const employee = await employeeRepository.findByCodeAndDate(
      'EMP001',
      new Date('2025-06-01')
    )

    // Assert
    expect(employee?.departmentCode).toBe('DEPT002')
  })
})
