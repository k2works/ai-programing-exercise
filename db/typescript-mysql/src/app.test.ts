/* eslint-disable no-undef */
import { describe, test, expect, beforeAll, afterAll } from 'vitest'
import { PrismaClient, Department, Employee } from '@prisma/client'

/**
 * テスト実行時は TEST_DATABASE_URL を使用する
 */
const isTest = process.env.VITEST === 'true' || process.env.NODE_ENV === 'test'
const databaseUrl = isTest ? process.env.TEST_DATABASE_URL : process.env.DATABASE_URL

const prisma = new PrismaClient({
  datasources: {
    db: {
      url: databaseUrl
    }
  }
})

describe('Prisma接続テスト', () => {
  beforeAll(async () => {
    // データベース接続の確認
  })

  afterAll(async () => {
    await prisma.$disconnect()
  })

  test('データベースに接続できる', async () => {
    // 接続確認用の簡単なクエリ
    const result = await prisma.$queryRaw`SELECT 1 as result`
    expect(result).toBeDefined()
  })
})

// テスト用の部門データ
const departments: Department[] = [
  {
    deptCode: '11101',
    startDate: new Date('2021-01-01'),
    endDate: new Date('2100-12-31'),
    name: '新規部署',
    layer: 1,
    psth: 'D0001',
    lowestType: 1,
    slitYn: 0,
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  }
]

describe('部門マスタ', () => {
  // 各テストの前に、テーブルをクリーンな状態にする
  beforeAll(async () => {
    await prisma.department.deleteMany()
  })

  afterAll(async () => {
    await prisma.$disconnect()
  })

  test('部門を登録できる', async () => {
    // 1. テストデータを作成
    await prisma.department.create({ data: departments[0] })
    // 2. 取得したデータが期待通りか検証
    const result = await prisma.department.findMany()
    expect(result).toEqual(departments)
  })

  test('部門を更新できる', async () => {
    // 1. データを「更新部署」という名前に更新
    const expected = { ...departments[0], name: '更新部署' }
    await prisma.department.update({
      where: {
        deptCode_startDate: {
          deptCode: departments[0].deptCode,
          startDate: departments[0].startDate
        }
      },
      data: { name: '更新部署' }
    })
    // 2. 更新されたか検証
    const result = await prisma.department.findUnique({
      where: {
        deptCode_startDate: {
          deptCode: departments[0].deptCode,
          startDate: departments[0].startDate
        }
      }
    })
    expect(result).toEqual(expected)
  })

  test('部門を削除できる', async () => {
    // 1. データを削除
    await prisma.department.delete({
      where: {
        deptCode_startDate: {
          deptCode: departments[0].deptCode,
          startDate: departments[0].startDate
        }
      }
    })
    // 2. テーブルが空になったか検証
    const result = await prisma.department.findMany()
    expect(result).toEqual([])
  })
})

// テスト用の社員データ
const employees: Employee[] = [
  {
    empCode: 'EMP999',
    name: '伊藤 裕子',
    kana: 'イトウ ユウコ',
    loginPassword: 'password',
    tel: '090-1234-5678',
    fax: '03-1234-5678',
    deptCode: '11101',
    startDate: new Date('2021-01-01'),
    occuCode: '',
    approvalCode: '',
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  }
]

describe('社員マスタ', () => {
  // 各テストの前に、テーブルをクリーンな状態にする
  beforeAll(async () => {
    await prisma.employee.deleteMany()
    await prisma.department.deleteMany()
    // 社員が参照する部門を先に登録
    await prisma.department.create({ data: departments[0] })
  })

  afterAll(async () => {
    await prisma.employee.deleteMany()
    await prisma.department.deleteMany()
    await prisma.$disconnect()
  })

  test('社員を登録できる', async () => {
    // 1. テストデータを作成
    await prisma.employee.create({ data: employees[0] })
    // 2. 取得したデータが期待通りか検証
    const result = await prisma.employee.findMany()
    expect(result).toEqual(employees)
  })

  test('社員を更新できる', async () => {
    // 1. データを「佐藤 裕子」という名前に更新
    const expected = { ...employees[0], name: '佐藤 裕子' }
    await prisma.employee.update({
      where: { empCode: employees[0].empCode },
      data: { name: '佐藤 裕子' }
    })
    // 2. 更新されたか検証
    const result = await prisma.employee.findUnique({
      where: { empCode: employees[0].empCode }
    })
    expect(result).toEqual(expected)
  })

  test('社員を削除できる', async () => {
    // 1. データを削除
    await prisma.employee.delete({ where: { empCode: employees[0].empCode } })
    // 2. テーブルが空になったか検証
    const result = await prisma.employee.findMany()
    expect(result).toEqual([])
  })
})
