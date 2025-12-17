import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { BomRepository } from './bom.repository'
import { ItemRepository } from '../item/item.repository'

describe('BOM（部品構成表）', () => {
  let testDb: TestDatabase
  let bomRepository: BomRepository
  let itemRepository: ItemRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    bomRepository = new BomRepository(testDb.prisma!)
    itemRepository = new ItemRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  describe('登録', () => {
    test('親品目と子品目の関係を登録できる', async () => {
      // Arrange: 親品目と子品目を作成
      const parent = await itemRepository.create({
        itemCode: 'PRODUCT-X',
        itemName: '製品X',
        itemCategory: 'PRODUCT',
        effectiveFrom: new Date('2025-01-01'),
      })

      const child = await itemRepository.create({
        itemCode: 'PART-A',
        itemName: '部品A',
        itemCategory: 'PART',
        effectiveFrom: new Date('2025-01-01'),
      })

      // Act: BOMを登録
      const bom = await bomRepository.create({
        parentItemCode: parent.itemCode,
        childItemCode: child.itemCode,
        effectiveFrom: new Date('2025-01-01'),
        baseQuantity: 1,
        requiredQuantity: 2, // 製品X 1個に対して部品A 2個必要
      })

      // Assert
      expect(bom.parentItemCode).toBe('PRODUCT-X')
      expect(bom.childItemCode).toBe('PART-A')
      expect(Number(bom.requiredQuantity)).toBe(2)
    })
  })

  describe('展開', () => {
    test('1階層の部品展開ができる', async () => {
      // Arrange: 製品と部品を作成
      const product = await itemRepository.create({
        itemCode: 'PRODUCT-X',
        itemName: '製品X',
        itemCategory: 'PRODUCT',
        effectiveFrom: new Date('2025-01-01'),
      })

      const partA = await itemRepository.create({
        itemCode: 'PART-A',
        itemName: '部品A',
        itemCategory: 'PART',
        effectiveFrom: new Date('2025-01-01'),
      })

      const partB = await itemRepository.create({
        itemCode: 'PART-B',
        itemName: '部品B',
        itemCategory: 'PART',
        effectiveFrom: new Date('2025-01-01'),
      })

      // BOMを登録
      await bomRepository.create({
        parentItemCode: product.itemCode,
        childItemCode: partA.itemCode,
        effectiveFrom: new Date('2025-01-01'),
        baseQuantity: 1,
        requiredQuantity: 2,
      })

      await bomRepository.create({
        parentItemCode: product.itemCode,
        childItemCode: partB.itemCode,
        effectiveFrom: new Date('2025-01-01'),
        baseQuantity: 1,
        requiredQuantity: 3,
      })

      // Act: 製品Xの部品を展開
      const children = await bomRepository.getChildren(product.itemCode)

      // Assert
      expect(children).toHaveLength(2)
      expect(children.map((c) => c.childItemCode)).toContain('PART-A')
      expect(children.map((c) => c.childItemCode)).toContain('PART-B')
    })
  })

  describe('多階層展開', () => {
    test('再帰的に全階層を展開できる', async () => {
      // Arrange: 3階層の構造を作成
      // 製品X -> 中間品N -> 部品A
      const productX = await itemRepository.create({
        itemCode: 'PRODUCT-X',
        itemName: '製品X',
        itemCategory: 'PRODUCT',
        effectiveFrom: new Date('2025-01-01'),
      })

      const intermediateN = await itemRepository.create({
        itemCode: 'INTERMEDIATE-N',
        itemName: '中間品N',
        itemCategory: 'INTERMEDIATE',
        effectiveFrom: new Date('2025-01-01'),
      })

      const partA = await itemRepository.create({
        itemCode: 'PART-A',
        itemName: '部品A',
        itemCategory: 'PART',
        effectiveFrom: new Date('2025-01-01'),
      })

      // BOMを登録
      await bomRepository.create({
        parentItemCode: productX.itemCode,
        childItemCode: intermediateN.itemCode,
        effectiveFrom: new Date('2025-01-01'),
        baseQuantity: 1,
        requiredQuantity: 1,
      })

      await bomRepository.create({
        parentItemCode: intermediateN.itemCode,
        childItemCode: partA.itemCode,
        effectiveFrom: new Date('2025-01-01'),
        baseQuantity: 1,
        requiredQuantity: 2,
      })

      // Act: 全階層を展開
      const explosion = await bomRepository.explode(productX.itemCode)

      // Assert
      expect(explosion).toHaveLength(2)
      expect(explosion[0].childItemCode).toBe('INTERMEDIATE-N')
      expect(explosion[0].level).toBe(1)
      expect(explosion[1].childItemCode).toBe('PART-A')
      expect(explosion[1].level).toBe(2)
    })
  })
})
