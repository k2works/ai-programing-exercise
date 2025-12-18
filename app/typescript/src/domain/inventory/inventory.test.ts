import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { InventoryRepository } from './inventory.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'
import { LocationRepository } from '../location/location.repository'

describe('在庫管理業務', () => {
  let testDb: TestDatabase
  let inventoryRepository: InventoryRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository
  let locationRepository: LocationRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    inventoryRepository = new InventoryRepository(testDb.prisma!)
    itemRepository = new ItemRepository(testDb.prisma!)
    unitRepository = new UnitRepository(testDb.prisma!)
    locationRepository = new LocationRepository(testDb.prisma!)
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
      itemCode: 'ITEM-001',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '製品A',
      itemCategory: 'PRODUCT',
      unitCode: 'PC',
    })

    await itemRepository.create({
      itemCode: 'ITEM-002',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '材料B',
      itemCategory: 'MATERIAL',
      unitCode: 'PC',
    })

    // 場所マスタ
    await locationRepository.create({
      locationCode: 'LOC-001',
      locationName: '倉庫A',
      locationType: 'WAREHOUSE',
    })

    await locationRepository.create({
      locationCode: 'LOC-002',
      locationName: '工場B',
      locationType: 'FACTORY',
    })
  }

  describe('在庫の参照', () => {
    test('在庫情報を取得できる', async () => {
      // Arrange
      await setupTestData()
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 100,
        status: 'accepted',
      })

      // Act
      const inventory = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')

      // Assert
      expect(inventory).toBeDefined()
      expect(inventory!.locationCode).toBe('LOC-001')
      expect(inventory!.itemCode).toBe('ITEM-001')
      expect(Number(inventory!.stockQuantity)).toBe(100)
      expect(Number(inventory!.acceptedQuantity)).toBe(100)
      expect(Number(inventory!.defectQuantity)).toBe(0)
      expect(Number(inventory!.uninspectedQuantity)).toBe(0)
    })

    test('在庫が存在しない場合は0を返す', async () => {
      // Arrange
      await setupTestData()

      // Act
      const inventory = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')

      // Assert
      expect(inventory).toBeDefined()
      expect(inventory!.id).toBe(0)
      expect(inventory!.locationCode).toBe('LOC-001')
      expect(inventory!.itemCode).toBe('ITEM-001')
      expect(Number(inventory!.stockQuantity)).toBe(0)
      expect(Number(inventory!.acceptedQuantity)).toBe(0)
      expect(Number(inventory!.defectQuantity)).toBe(0)
      expect(Number(inventory!.uninspectedQuantity)).toBe(0)
    })
  })

  describe('在庫の増加', () => {
    test('合格品在庫を増やすことができる', async () => {
      // Arrange
      await setupTestData()

      // Act
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 100,
        status: 'accepted',
      })

      // Assert
      const inventory = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')
      expect(Number(inventory!.stockQuantity)).toBe(100)
      expect(Number(inventory!.acceptedQuantity)).toBe(100)
      expect(Number(inventory!.defectQuantity)).toBe(0)
      expect(Number(inventory!.uninspectedQuantity)).toBe(0)
    })

    test('不良品在庫を増やすことができる', async () => {
      // Arrange
      await setupTestData()

      // Act
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 10,
        status: 'defect',
      })

      // Assert
      const inventory = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')
      expect(Number(inventory!.stockQuantity)).toBe(10)
      expect(Number(inventory!.acceptedQuantity)).toBe(0)
      expect(Number(inventory!.defectQuantity)).toBe(10)
      expect(Number(inventory!.uninspectedQuantity)).toBe(0)
    })

    test('未検査在庫を増やすことができる', async () => {
      // Arrange
      await setupTestData()

      // Act
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 50,
        status: 'uninspected',
      })

      // Assert
      const inventory = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')
      expect(Number(inventory!.stockQuantity)).toBe(50)
      expect(Number(inventory!.acceptedQuantity)).toBe(0)
      expect(Number(inventory!.defectQuantity)).toBe(0)
      expect(Number(inventory!.uninspectedQuantity)).toBe(50)
    })

    test('既存在庫に追加で在庫を増やすことができる', async () => {
      // Arrange
      await setupTestData()

      // 初回の在庫増加
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 100,
        status: 'accepted',
      })

      // Act: 2回目の在庫増加
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 50,
        status: 'accepted',
      })

      // Assert
      const inventory = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')
      expect(Number(inventory!.stockQuantity)).toBe(150)
      expect(Number(inventory!.acceptedQuantity)).toBe(150)
    })

    test('異なるステータスの在庫を増やすことができる', async () => {
      // Arrange
      await setupTestData()

      // Act
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 100,
        status: 'accepted',
      })

      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 10,
        status: 'defect',
      })

      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 20,
        status: 'uninspected',
      })

      // Assert
      const inventory = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')
      expect(Number(inventory!.stockQuantity)).toBe(130)
      expect(Number(inventory!.acceptedQuantity)).toBe(100)
      expect(Number(inventory!.defectQuantity)).toBe(10)
      expect(Number(inventory!.uninspectedQuantity)).toBe(20)
    })
  })

  describe('在庫の減少', () => {
    test('在庫を減らすことができる', async () => {
      // Arrange
      await setupTestData()
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 100,
        status: 'accepted',
      })

      // Act
      await inventoryRepository.decrease({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 30,
        status: 'accepted',
      })

      // Assert
      const inventory = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')
      expect(Number(inventory!.stockQuantity)).toBe(70)
      expect(Number(inventory!.acceptedQuantity)).toBe(70)
    })

    test('在庫が不足している場合はエラーになる', async () => {
      // Arrange
      await setupTestData()
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 50,
        status: 'accepted',
      })

      // Act & Assert
      await expect(
        inventoryRepository.decrease({
          locationCode: 'LOC-001',
          itemCode: 'ITEM-001',
          quantity: 100,
          status: 'accepted',
        })
      ).rejects.toThrow('在庫が不足しています')
    })

    test('在庫が存在しない場合はエラーになる', async () => {
      // Arrange
      await setupTestData()

      // Act & Assert
      await expect(
        inventoryRepository.decrease({
          locationCode: 'LOC-001',
          itemCode: 'ITEM-001',
          quantity: 10,
          status: 'accepted',
        })
      ).rejects.toThrow('在庫が不足しています')
    })

    test('指定したステータスの在庫が不足している場合はエラーになる', async () => {
      // Arrange
      await setupTestData()
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 100,
        status: 'accepted',
      })

      // 合格品は100個あるが、不良品は0個
      // Act & Assert
      await expect(
        inventoryRepository.decrease({
          locationCode: 'LOC-001',
          itemCode: 'ITEM-001',
          quantity: 10,
          status: 'defect',
        })
      ).rejects.toThrow('在庫が不足しています')
    })
  })

  describe('在庫状態の変更', () => {
    test('未検査から合格品に状態を変更できる', async () => {
      // Arrange
      await setupTestData()
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 100,
        status: 'uninspected',
      })

      // Act
      await inventoryRepository.changeStatus({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 80,
        fromStatus: 'uninspected',
        toStatus: 'accepted',
      })

      // Assert
      const inventory = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')
      expect(Number(inventory!.stockQuantity)).toBe(100) // 総在庫数は変わらない
      expect(Number(inventory!.acceptedQuantity)).toBe(80)
      expect(Number(inventory!.defectQuantity)).toBe(0)
      expect(Number(inventory!.uninspectedQuantity)).toBe(20)
    })

    test('未検査から不良品に状態を変更できる', async () => {
      // Arrange
      await setupTestData()
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 100,
        status: 'uninspected',
      })

      // Act
      await inventoryRepository.changeStatus({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 20,
        fromStatus: 'uninspected',
        toStatus: 'defect',
      })

      // Assert
      const inventory = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')
      expect(Number(inventory!.stockQuantity)).toBe(100)
      expect(Number(inventory!.acceptedQuantity)).toBe(0)
      expect(Number(inventory!.defectQuantity)).toBe(20)
      expect(Number(inventory!.uninspectedQuantity)).toBe(80)
    })

    test('元のステータスの在庫が不足している場合はエラーになる', async () => {
      // Arrange
      await setupTestData()
      await inventoryRepository.increase({
        locationCode: 'LOC-001',
        itemCode: 'ITEM-001',
        quantity: 50,
        status: 'uninspected',
      })

      // Act & Assert
      await expect(
        inventoryRepository.changeStatus({
          locationCode: 'LOC-001',
          itemCode: 'ITEM-001',
          quantity: 100,
          fromStatus: 'uninspected',
          toStatus: 'accepted',
        })
      ).rejects.toThrow('uninspectedの在庫が不足しています')
    })

    test('在庫が存在しない場合はエラーになる', async () => {
      // Arrange
      await setupTestData()

      // Act & Assert
      await expect(
        inventoryRepository.changeStatus({
          locationCode: 'LOC-001',
          itemCode: 'ITEM-001',
          quantity: 10,
          fromStatus: 'uninspected',
          toStatus: 'accepted',
        })
      ).rejects.toThrow('uninspectedの在庫が不足しています')
    })
  })
})
