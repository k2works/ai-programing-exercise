import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { LocationRepository } from './location.repository'
import { DepartmentRepository } from '../department/department.repository'
import { SupplierRepository } from '../supplier/supplier.repository'

describe('場所マスタ', () => {
  let testDb: TestDatabase
  let locationRepository: LocationRepository
  let departmentRepository: DepartmentRepository
  let supplierRepository: SupplierRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    locationRepository = new LocationRepository(testDb.prisma!)
    departmentRepository = new DepartmentRepository(testDb.prisma!)
    supplierRepository = new SupplierRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('場所を登録できる', async () => {
    // Arrange: 部門を作成
    await departmentRepository.create({
      departmentCode: 'DEPT001',
      effectiveFrom: new Date('2025-01-01'),
      level: 1,
      departmentName: '製造部',
    })

    // Act
    const location = await locationRepository.create({
      locationCode: 'LOC001',
      locationName: '第一倉庫',
      locationType: 'WAREHOUSE',
      departmentCode: 'DEPT001',
      postalCode: '100-0001',
      address: '東京都千代田区千代田1-1',
      phone: '03-1234-5678',
    })

    // Assert
    expect(location.locationCode).toBe('LOC001')
    expect(location.locationName).toBe('第一倉庫')
    expect(location.locationType).toBe('WAREHOUSE')
    expect(location.departmentCode).toBe('DEPT001')
  })

  test('外部倉庫を登録できる', async () => {
    // Arrange: 取引先を作成
    await supplierRepository.create({
      supplierCode: 'SUP001',
      supplierName: '倉庫会社A',
      supplierType: 'VENDOR',
    })

    // Act
    const location = await locationRepository.create({
      locationCode: 'LOC002',
      locationName: '外部倉庫A',
      locationType: 'EXTERNAL',
      supplierCode: 'SUP001',
    })

    // Assert
    expect(location.locationCode).toBe('LOC002')
    expect(location.locationType).toBe('EXTERNAL')
    expect(location.supplierCode).toBe('SUP001')
  })

  test('部門コードで場所を検索できる', async () => {
    // Arrange: 部門と複数の場所を作成
    await departmentRepository.create({
      departmentCode: 'DEPT001',
      effectiveFrom: new Date('2025-01-01'),
      level: 1,
      departmentName: '製造部',
    })

    await locationRepository.create({
      locationCode: 'LOC001',
      locationName: '第一倉庫',
      locationType: 'WAREHOUSE',
      departmentCode: 'DEPT001',
    })

    await locationRepository.create({
      locationCode: 'LOC002',
      locationName: '第一工場',
      locationType: 'FACTORY',
      departmentCode: 'DEPT001',
    })

    await locationRepository.create({
      locationCode: 'LOC003',
      locationName: '外部倉庫',
      locationType: 'EXTERNAL',
      supplierCode: 'SUP001',
    })

    // Act
    const locations = await locationRepository.findByDepartment('DEPT001')

    // Assert
    expect(locations).toHaveLength(2)
    expect(locations.map((l) => l.locationCode).sort()).toEqual(['LOC001', 'LOC002'])
  })
})
