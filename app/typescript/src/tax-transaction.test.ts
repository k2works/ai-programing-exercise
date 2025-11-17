// src/tax-transaction.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

describe('課税取引マスタ', () => {
  let testDb: TestDatabase

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()
  })

  afterAll(async () => {
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  test('課税取引を登録できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    const taxTransaction = await testDb.prisma.taxTransaction.create({
      data: {
        taxCode: '01',
        taxName: '課税売上10%',
        taxRate: 10.0,
        taxType: '課税'
      }
    })

    expect(taxTransaction.taxCode).toBe('01')
    expect(taxTransaction.taxName).toBe('課税売上10%')
    expect(taxTransaction.taxRate).toBe(10.0)
    expect(taxTransaction.taxType).toBe('課税')
  })

  test('複数の課税取引を登録できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    await testDb.prisma.taxTransaction.createMany({
      data: [
        { taxCode: '01', taxName: '課税売上10%', taxRate: 10.0, taxType: '課税' },
        {
          taxCode: '02',
          taxName: '課税売上8%（軽減税率）',
          taxRate: 8.0,
          taxType: '課税'
        },
        { taxCode: '03', taxName: '非課税売上', taxRate: 0.0, taxType: '非課税' },
        { taxCode: '04', taxName: '免税売上', taxRate: 0.0, taxType: '免税' },
        { taxCode: '05', taxName: '不課税取引', taxRate: 0.0, taxType: '不課税' }
      ]
    })

    const transactions = await testDb.prisma.taxTransaction.findMany()
    expect(transactions).toHaveLength(5)
  })

  test('課税取引コードで検索できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    await testDb.prisma.taxTransaction.create({
      data: {
        taxCode: '01',
        taxName: '課税売上10%',
        taxRate: 10.0,
        taxType: '課税'
      }
    })

    const found = await testDb.prisma.taxTransaction.findUnique({
      where: { taxCode: '01' }
    })

    expect(found).not.toBeNull()
    expect(found?.taxName).toBe('課税売上10%')
  })

  test('課税区分で検索できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    await testDb.prisma.taxTransaction.createMany({
      data: [
        { taxCode: '01', taxName: '課税売上10%', taxRate: 10.0, taxType: '課税' },
        { taxCode: '02', taxName: '課税売上8%', taxRate: 8.0, taxType: '課税' },
        { taxCode: '03', taxName: '非課税売上', taxRate: 0.0, taxType: '非課税' }
      ]
    })

    const taxable = await testDb.prisma.taxTransaction.findMany({
      where: { taxType: '課税' }
    })

    expect(taxable).toHaveLength(2)
  })
})
