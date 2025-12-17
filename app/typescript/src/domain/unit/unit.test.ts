import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { UnitRepository } from './unit.repository'

describe('単位マスタ', () => {
  let testDb: TestDatabase
  let unitRepository: UnitRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    unitRepository = new UnitRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('単位を登録できる', async () => {
    // Arrange
    const input = {
      unitCode: 'PCS',
      unitSymbol: '個',
      unitName: '個数',
    }

    // Act
    const result = await unitRepository.create(input)

    // Assert
    expect(result.unitCode).toBe('PCS')
    expect(result.unitSymbol).toBe('個')
    expect(result.unitName).toBe('個数')
  })
})
