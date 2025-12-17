import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { DepartmentRepository } from './department.repository'

describe('部門マスタ', () => {
  let testDb: TestDatabase
  let departmentRepository: DepartmentRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    departmentRepository = new DepartmentRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('部門を登録できる', async () => {
    // Arrange & Act
    const department = await departmentRepository.create({
      departmentCode: 'DEPT001',
      effectiveFrom: new Date('2025-01-01'),
      level: 1,
      departmentName: '製造部',
      shortName: '製造',
    })

    // Assert
    expect(department.departmentCode).toBe('DEPT001')
    expect(department.departmentName).toBe('製造部')
    expect(department.level).toBe(1)
  })
})
