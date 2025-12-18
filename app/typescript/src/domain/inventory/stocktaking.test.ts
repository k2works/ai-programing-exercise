import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { StocktakingRepository } from './stocktaking.repository'
import { InventoryRepository } from './inventory.repository'
import { ItemRepository } from '../item/item.repository'
import { UnitRepository } from '../unit/unit.repository'
import { LocationRepository } from '../location/location.repository'

describe('棚卸業務', () => {
  let testDb: TestDatabase
  let stocktakingRepository: StocktakingRepository
  let inventoryRepository: InventoryRepository
  let itemRepository: ItemRepository
  let unitRepository: UnitRepository
  let locationRepository: LocationRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    stocktakingRepository = new StocktakingRepository(testDb.prisma!)
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
      itemName: '製品B',
      itemCategory: 'PRODUCT',
      unitCode: 'PC',
    })

    await itemRepository.create({
      itemCode: 'ITEM-003',
      effectiveFrom: new Date('2025-01-01'),
      itemName: '製品C',
      itemCategory: 'PRODUCT',
      unitCode: 'PC',
    })

    // 場所マスタ
    await locationRepository.create({
      locationCode: 'LOC-001',
      locationName: '倉庫A',
      locationType: 'WAREHOUSE',
    })

    // 初期在庫を設定
    await inventoryRepository.increase({
      locationCode: 'LOC-001',
      itemCode: 'ITEM-001',
      quantity: 100,
      status: 'accepted',
    })

    await inventoryRepository.increase({
      locationCode: 'LOC-001',
      itemCode: 'ITEM-002',
      quantity: 200,
      status: 'accepted',
    })

    await inventoryRepository.increase({
      locationCode: 'LOC-001',
      itemCode: 'ITEM-003',
      quantity: 150,
      status: 'accepted',
    })
  }

  describe('棚卸表発行', () => {
    test('棚卸データを発行できる', async () => {
      // Arrange
      await setupTestData()

      // Act
      const result = await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
        createdBy: 'user01',
      })

      // Assert
      expect(result.id).toBeDefined()
      expect(result.stocktakingNumber).toBe('ST-2025-001')
      expect(result.locationCode).toBe('LOC-001')
      expect(result.status).toBe('ISSUED')
      expect(result.details).toHaveLength(3) // 3品目の在庫がある
    })

    test('在庫データから棚卸明細を作成できる', async () => {
      // Arrange
      await setupTestData()

      // Act
      const result = await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
      })

      // Assert
      expect(result.details[0].itemCode).toBe('ITEM-001')
      expect(Number(result.details[0].bookQuantity)).toBe(100)
      expect(result.details[0].actualQuantity).toBeNull()
      expect(Number(result.details[0].differenceQuantity)).toBe(0)

      expect(result.details[1].itemCode).toBe('ITEM-002')
      expect(Number(result.details[1].bookQuantity)).toBe(200)

      expect(result.details[2].itemCode).toBe('ITEM-003')
      expect(Number(result.details[2].bookQuantity)).toBe(150)
    })
  })

  describe('実棚入力', () => {
    test('実棚数を入力できる', async () => {
      // Arrange
      await setupTestData()

      const stocktaking = await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
      })

      // Act
      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[0].id,
        actualQuantity: 95, // 帳簿100、実棚95 → 差異-5
      })

      // Assert
      const result = await stocktakingRepository.findByNumber('ST-2025-001')
      expect(result).toBeDefined()
      expect(Number(result!.details[0].actualQuantity)).toBe(95)
      expect(Number(result!.details[0].differenceQuantity)).toBe(-5)
    })

    test('すべての明細に実棚数が入力されたらステータスが入力済みになる', async () => {
      // Arrange
      await setupTestData()

      const stocktaking = await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
      })

      // 最初のステータスは発行済み
      const beforeStatus = await stocktakingRepository.findByNumber('ST-2025-001')
      expect(beforeStatus!.status).toBe('ISSUED')

      // Act: すべての明細に実棚数を入力
      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[0].id,
        actualQuantity: 95,
      })

      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[1].id,
        actualQuantity: 205,
      })

      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[2].id,
        actualQuantity: 150,
      })

      // Assert
      const result = await stocktakingRepository.findByNumber('ST-2025-001')
      expect(result!.status).toBe('INPUTTED')
    })

    test('一部の明細のみ入力された場合はステータスが発行済みのまま', async () => {
      // Arrange
      await setupTestData()

      const stocktaking = await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
      })

      // Act: 1つの明細のみ実棚数を入力
      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[0].id,
        actualQuantity: 95,
      })

      // Assert
      const result = await stocktakingRepository.findByNumber('ST-2025-001')
      expect(result!.status).toBe('ISSUED')
    })
  })

  describe('棚卸確定', () => {
    test('棚卸を確定できる', async () => {
      // Arrange
      await setupTestData()

      const stocktaking = await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
      })

      // すべての明細に実棚数を入力
      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[0].id,
        actualQuantity: 95,
      })

      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[1].id,
        actualQuantity: 205,
      })

      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[2].id,
        actualQuantity: 150,
      })

      // Act
      const adjustments = await stocktakingRepository.confirmStocktaking('ST-2025-001', 'user01')

      // Assert
      const result = await stocktakingRepository.findByNumber('ST-2025-001')
      expect(result!.status).toBe('CONFIRMED')
      expect(adjustments).toHaveLength(2) // 差異がある明細は2つ
    })

    test('差異がある場合は在庫調整データが作成される', async () => {
      // Arrange
      await setupTestData()

      const stocktaking = await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
      })

      // すべての明細に実棚数を入力
      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[0].id,
        actualQuantity: 95, // 帳簿100 → 不足5
      })

      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[1].id,
        actualQuantity: 205, // 帳簿200 → 余剰5
      })

      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[2].id,
        actualQuantity: 150, // 差異なし
      })

      // Act
      const adjustments = await stocktakingRepository.confirmStocktaking('ST-2025-001')

      // Assert
      expect(adjustments).toHaveLength(2)

      // 不足の調整
      const shortage = adjustments.find((adj) => adj.itemCode === 'ITEM-001')
      expect(shortage).toBeDefined()
      expect(Number(shortage!.adjustmentQuantity)).toBe(-5)
      expect(shortage!.reasonCode).toBe('SHORTAGE')

      // 余剰の調整
      const surplus = adjustments.find((adj) => adj.itemCode === 'ITEM-002')
      expect(surplus).toBeDefined()
      expect(Number(surplus!.adjustmentQuantity)).toBe(5)
      expect(surplus!.reasonCode).toBe('SURPLUS')
    })

    test('棚卸確定時に在庫が調整される', async () => {
      // Arrange
      await setupTestData()

      const stocktaking = await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
      })

      // 確定前の在庫確認
      const beforeInventory1 = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')
      const beforeInventory2 = await inventoryRepository.findByLocation('LOC-001', 'ITEM-002')
      expect(Number(beforeInventory1!.stockQuantity)).toBe(100)
      expect(Number(beforeInventory2!.stockQuantity)).toBe(200)

      // すべての明細に実棚数を入力
      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[0].id,
        actualQuantity: 95,
      })

      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[1].id,
        actualQuantity: 205,
      })

      await stocktakingRepository.inputActualQuantity({
        stocktakingNumber: 'ST-2025-001',
        detailId: stocktaking.details[2].id,
        actualQuantity: 150,
      })

      // Act
      await stocktakingRepository.confirmStocktaking('ST-2025-001')

      // Assert
      const afterInventory1 = await inventoryRepository.findByLocation('LOC-001', 'ITEM-001')
      const afterInventory2 = await inventoryRepository.findByLocation('LOC-001', 'ITEM-002')
      const afterInventory3 = await inventoryRepository.findByLocation('LOC-001', 'ITEM-003')

      expect(Number(afterInventory1!.stockQuantity)).toBe(95) // 100 - 5
      expect(Number(afterInventory2!.stockQuantity)).toBe(205) // 200 + 5
      expect(Number(afterInventory3!.stockQuantity)).toBe(150) // 変更なし
    })

    test('ステータスが入力済みでない場合はエラーになる', async () => {
      // Arrange
      await setupTestData()

      await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
      })

      // 実棚数を入力せずに確定しようとする

      // Act & Assert
      await expect(stocktakingRepository.confirmStocktaking('ST-2025-001')).rejects.toThrow(
        '棚卸データのステータスが入力済みではありません'
      )
    })
  })

  describe('棚卸データ検索', () => {
    test('棚卸番号で棚卸データを検索できる', async () => {
      // Arrange
      await setupTestData()

      await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
      })

      // Act
      const result = await stocktakingRepository.findByNumber('ST-2025-001')

      // Assert
      expect(result).toBeDefined()
      expect(result!.stocktakingNumber).toBe('ST-2025-001')
      expect(result!.details).toHaveLength(3)
    })

    test('場所コードで棚卸データを検索できる', async () => {
      // Arrange
      await setupTestData()

      await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-001',
        stocktakingDate: new Date('2025-01-31'),
        locationCode: 'LOC-001',
      })

      await stocktakingRepository.issueStocktaking({
        stocktakingNumber: 'ST-2025-002',
        stocktakingDate: new Date('2025-02-28'),
        locationCode: 'LOC-001',
      })

      // Act
      const results = await stocktakingRepository.findByLocation('LOC-001')

      // Assert
      expect(results).toHaveLength(2)
      expect(results[0].stocktakingNumber).toBe('ST-2025-002') // 日付降順
      expect(results[1].stocktakingNumber).toBe('ST-2025-001')
    })
  })
})
