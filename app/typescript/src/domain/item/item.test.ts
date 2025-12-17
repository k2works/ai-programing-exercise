import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { ItemRepository } from './item.repository'
import { UnitRepository } from '../unit/unit.repository'

describe('品目マスタ', () => {
  let testDb: TestDatabase
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    itemRepository = new ItemRepository(testDb.prisma!)
    unitRepository = new UnitRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  describe('登録', () => {
    test('品目を登録できる', async () => {
      // Arrange
      const input = {
        itemCode: 'ITEM001',
        itemName: '製品A',
        itemCategory: 'PRODUCT' as const,
        effectiveFrom: new Date('2025-01-01'),
      }

      // Act
      const result = await itemRepository.create(input)

      // Assert
      expect(result.id).toBeDefined()
      expect(result.itemCode).toBe('ITEM001')
      expect(result.itemName).toBe('製品A')
      expect(result.itemCategory).toBe('PRODUCT')
    })
  })

  describe('制約', () => {
    test('品目コードは一意である', async () => {
      // Arrange
      const input = {
        itemCode: 'ITEM001',
        itemName: '製品A',
        itemCategory: 'PRODUCT' as const,
        effectiveFrom: new Date('2025-01-01'),
      }

      // 1件目を登録
      await itemRepository.create(input)

      // Act & Assert: 同じ品目コードで2件目を登録しようとするとエラー
      await expect(
        itemRepository.create({ ...input, itemName: '製品B' })
      ).rejects.toThrow()
    })
  })

  describe('単位マスタ', () => {
    test('品目に単位を設定できる', async () => {
      // Arrange: 単位を作成
      const unit = await unitRepository.create({
        unitCode: 'PCS',
        unitSymbol: '個',
        unitName: '個数',
      })

      // Act: 単位を持つ品目を作成
      const item = await itemRepository.create({
        itemCode: 'ITEM001',
        itemName: '製品A',
        itemCategory: 'PRODUCT',
        effectiveFrom: new Date('2025-01-01'),
        unitCode: unit.unitCode,
      })

      // Assert
      expect(item.unitCode).toBe('PCS')
    })
  })

  describe('生産管理属性', () => {
    test('リードタイムと安全在庫を設定できる', async () => {
      // Arrange & Act
      const item = await itemRepository.create({
        itemCode: 'ITEM001',
        itemName: '製品A',
        itemCategory: 'PRODUCT',
        effectiveFrom: new Date('2025-01-01'),
        leadTime: 5, // リードタイム: 5日
        safetyLeadTime: 2, // 安全リードタイム: 2日
        safetyStock: 100, // 安全在庫: 100個
      })

      // Assert
      expect(item.leadTime).toBe(5)
      expect(item.safetyLeadTime).toBe(2)
      expect(Number(item.safetyStock)).toBe(100)
    })

    test('ロットサイズを設定できる', async () => {
      // Arrange & Act
      const item = await itemRepository.create({
        itemCode: 'ITEM001',
        itemName: '製品A',
        itemCategory: 'PRODUCT',
        effectiveFrom: new Date('2025-01-01'),
        minLotSize: 10, // 最小ロット: 10個
        lotIncrement: 5, // 刻みロット: 5個単位
        maxLotSize: 1000, // 最大ロット: 1000個
      })

      // Assert
      expect(Number(item.minLotSize)).toBe(10)
      expect(Number(item.lotIncrement)).toBe(5)
      expect(Number(item.maxLotSize)).toBe(1000)
    })
  })
})
