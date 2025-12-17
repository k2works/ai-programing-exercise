import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from '../../test-setup/database'
import { WorkdayRepository } from './workday.repository'

describe('就業日マスタ', () => {
  let testDb: TestDatabase
  let workdayRepository: WorkdayRepository

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
    workdayRepository = new WorkdayRepository(testDb.prisma!)
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('就業日を登録できる', async () => {
    // Arrange & Act
    const workday = await workdayRepository.create({
      date: new Date('2025-01-06'), // 月曜日
      workdayType: 'WORKING',
    })

    // Assert
    expect(workday.workdayType).toBe('WORKING')
    expect(workday.date).toEqual(new Date('2025-01-06'))
  })

  test('指定期間の稼働日数を取得できる', async () => {
    // Arrange: 1週間分の就業日を登録
    await workdayRepository.bulkCreate([
      { date: new Date('2025-01-06'), workdayType: 'WORKING' },
      { date: new Date('2025-01-07'), workdayType: 'WORKING' },
      { date: new Date('2025-01-08'), workdayType: 'WORKING' },
      { date: new Date('2025-01-09'), workdayType: 'WORKING' },
      { date: new Date('2025-01-10'), workdayType: 'WORKING' },
      { date: new Date('2025-01-11'), workdayType: 'HOLIDAY' }, // 土曜
      { date: new Date('2025-01-12'), workdayType: 'HOLIDAY' }, // 日曜
    ])

    // Act
    const count = await workdayRepository.countWorkingDays(
      new Date('2025-01-06'),
      new Date('2025-01-12')
    )

    // Assert
    expect(count).toBe(5)
  })
})
