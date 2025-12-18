import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { UnitPriceRepository } from './unit-price.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'
import { SupplierRepository } from '../supplier/supplier.repository'

describe('単価マスタ', () => {
  let testDb: TestDatabase
  let unitPriceRepository: UnitPriceRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository
  let supplierRepository: SupplierRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    unitPriceRepository = new UnitPriceRepository(testDb.prisma!)
    itemRepository = new ItemRepository(testDb.prisma!)
    unitRepository = new UnitRepository(testDb.prisma!)
    supplierRepository = new SupplierRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('単価マスタを登録できる', async () => {
    // Arrange: 単位、品目、取引先を作成
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

    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '取引先A',
      supplierType: 'VENDOR',
    })

    // Act: 単価マスタを登録
    const unitPrice = await unitPriceRepository.create({
      itemCode: 'PART-001',
      supplierCode: 'SUP-001',
      lotUnitQuantity: 100,
      effectiveFrom: new Date('2025-01-01'),
      unitPrice: 1000,
      createdBy: 'user01',
    })

    // Assert
    expect(unitPrice.id).toBeDefined()
    expect(unitPrice.itemCode).toBe('PART-001')
    expect(unitPrice.supplierCode).toBe('SUP-001')
    expect(Number(unitPrice.lotUnitQuantity)).toBe(100)
    expect(Number(unitPrice.unitPrice)).toBe(1000)
    expect(unitPrice.createdBy).toBe('user01')
  })

  test('有効期間内の単価を取得できる', async () => {
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

    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '取引先A',
      supplierType: 'VENDOR',
    })

    // 2つの期間の単価を登録
    await unitPriceRepository.create({
      itemCode: 'PART-001',
      supplierCode: 'SUP-001',
      lotUnitQuantity: 100,
      effectiveFrom: new Date('2025-01-01'),
      effectiveTo: new Date('2025-06-30'),
      unitPrice: 1000,
    })

    await unitPriceRepository.create({
      itemCode: 'PART-001',
      supplierCode: 'SUP-001',
      lotUnitQuantity: 100,
      effectiveFrom: new Date('2025-07-01'),
      unitPrice: 950, // 値下げ
    })

    // Act & Assert: 1月時点の単価
    const priceInJan = await unitPriceRepository.findCurrentPrice(
      'PART-001',
      'SUP-001',
      100,
      new Date('2025-01-15')
    )
    expect(priceInJan).toBeDefined()
    expect(Number(priceInJan!.unitPrice)).toBe(1000)

    // Act & Assert: 7月時点の単価
    const priceInJuly = await unitPriceRepository.findCurrentPrice(
      'PART-001',
      'SUP-001',
      100,
      new Date('2025-07-15')
    )
    expect(priceInJuly).toBeDefined()
    expect(Number(priceInJuly!.unitPrice)).toBe(950)
  })

  test('ロット単位数ごとに異なる単価を登録できる', async () => {
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

    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '取引先A',
      supplierType: 'VENDOR',
    })

    // 異なるロット数で異なる単価を登録（ボリュームディスカウント）
    await unitPriceRepository.create({
      itemCode: 'PART-001',
      supplierCode: 'SUP-001',
      lotUnitQuantity: 100,
      effectiveFrom: new Date('2025-01-01'),
      unitPrice: 1000, // 100個単位
    })

    await unitPriceRepository.create({
      itemCode: 'PART-001',
      supplierCode: 'SUP-001',
      lotUnitQuantity: 500,
      effectiveFrom: new Date('2025-01-01'),
      unitPrice: 950, // 500個単位（割引）
    })

    // Act: ロット単位数100の単価を取得
    const price100 = await unitPriceRepository.findCurrentPrice(
      'PART-001',
      'SUP-001',
      100,
      new Date('2025-01-15')
    )

    // Act: ロット単位数500の単価を取得
    const price500 = await unitPriceRepository.findCurrentPrice(
      'PART-001',
      'SUP-001',
      500,
      new Date('2025-01-15')
    )

    // Assert
    expect(Number(price100!.unitPrice)).toBe(1000)
    expect(Number(price500!.unitPrice)).toBe(950)
  })

  test('品目と取引先で単価一覧を取得できる', async () => {
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

    await supplierRepository.create({
      supplierCode: 'SUP-001',
      supplierName: '取引先A',
      supplierType: 'VENDOR',
    })

    // 複数のロット単位数と期間の単価を登録
    await unitPriceRepository.create({
      itemCode: 'PART-001',
      supplierCode: 'SUP-001',
      lotUnitQuantity: 100,
      effectiveFrom: new Date('2025-01-01'),
      unitPrice: 1000,
    })

    await unitPriceRepository.create({
      itemCode: 'PART-001',
      supplierCode: 'SUP-001',
      lotUnitQuantity: 500,
      effectiveFrom: new Date('2025-01-01'),
      unitPrice: 950,
    })

    // Act
    const prices = await unitPriceRepository.findByItemAndSupplier('PART-001', 'SUP-001')

    // Assert
    expect(prices).toHaveLength(2)
    expect(prices.map((p) => Number(p.lotUnitQuantity)).sort()).toEqual([100, 500])
  })
})
