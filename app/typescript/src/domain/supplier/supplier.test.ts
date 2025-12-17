import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { SupplierRepository } from './supplier.repository'

describe('取引先マスタ', () => {
  let testDb: TestDatabase
  let supplierRepository: SupplierRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    supplierRepository = new SupplierRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('取引先を登録できる', async () => {
    // Arrange & Act
    const supplier = await supplierRepository.create({
      supplierCode: 'SUP001',
      supplierName: '株式会社サプライヤー',
      supplierNameKana: 'カブシキガイシャサプライヤー',
      supplierType: 'VENDOR',
      postalCode: '100-0001',
      address: '東京都千代田区千代田1-1',
      phone: '03-1234-5678',
      email: 'info@supplier.example.com',
    })

    // Assert
    expect(supplier.supplierCode).toBe('SUP001')
    expect(supplier.supplierName).toBe('株式会社サプライヤー')
    expect(supplier.supplierType).toBe('VENDOR')
  })

  test('取引先区分で検索できる', async () => {
    // Arrange: 複数の取引先を登録
    await supplierRepository.create({
      supplierCode: 'SUP001',
      supplierName: '仕入先A',
      supplierType: 'VENDOR',
    })

    await supplierRepository.create({
      supplierCode: 'SUP002',
      supplierName: '得意先B',
      supplierType: 'CUSTOMER',
    })

    await supplierRepository.create({
      supplierCode: 'SUP003',
      supplierName: '得意先兼仕入先C',
      supplierType: 'BOTH',
    })

    // Act: 仕入先を検索
    const vendors = await supplierRepository.findByType('VENDOR')

    // Assert: VENDORとBOTHが取得される
    expect(vendors).toHaveLength(2)
    expect(vendors.map((v) => v.supplierCode).sort()).toEqual(['SUP001', 'SUP003'])
  })
})
