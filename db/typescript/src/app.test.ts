import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { PrismaClient } from '@prisma/client'

const prisma = new PrismaClient()

beforeAll(async () => {
  // テストデータベースへの接続確認
  await prisma.$connect()
})

afterAll(async () => {
  await prisma.$disconnect()
})

// テスト用の部門データ
const departments = [
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
    updater: 'admin',
  },
]

// テスト用の社員データ
const employees = [
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
    updater: 'admin',
  },
]

describe('部門マスタ', () => {
  beforeEach(async () => {
    // 各テストの前にテーブルをクリーンな状態にする
    await prisma.employee.deleteMany()
    await prisma.department.deleteMany()
  })

  test('部門を登録できる', async () => {
    // 1. テストデータを作成
    await prisma.department.create({ data: departments[0] })
    // 2. 取得したデータが期待通りか検証
    const result = await prisma.department.findMany()
    expect(result).toEqual(departments)
  })

  test('部門を更新できる', async () => {
    // 前提: 部門データを登録
    await prisma.department.create({ data: departments[0] })

    // 1. データを「更新部署」という名前に更新
    const expected = { ...departments[0], name: '更新部署' }
    await prisma.department.update({
      where: {
        deptCode_startDate: {
          deptCode: departments[0].deptCode,
          startDate: departments[0].startDate,
        },
      },
      data: { name: '更新部署' },
    })

    // 2. 更新されたか検証
    const result = await prisma.department.findUnique({
      where: {
        deptCode_startDate: {
          deptCode: departments[0].deptCode,
          startDate: departments[0].startDate,
        },
      },
    })
    expect(result).toEqual(expected)
  })

  test('部門を削除できる', async () => {
    // 前提: 部門データを登録
    await prisma.department.create({ data: departments[0] })

    // 1. データを削除
    await prisma.department.delete({
      where: {
        deptCode_startDate: {
          deptCode: departments[0].deptCode,
          startDate: departments[0].startDate,
        },
      },
    })

    // 2. テーブルが空になったか検証
    const result = await prisma.department.findMany()
    expect(result).toEqual([])
  })
})

describe('社員マスタ', () => {
  beforeEach(async () => {
    // 各テストの前にテーブルをクリーンな状態にする
    await prisma.employee.deleteMany()
    await prisma.department.deleteMany()

    // 社員テストのため、部門データを事前に登録
    await prisma.department.create({ data: departments[0] })
  })

  test('社員を登録できる', async () => {
    // 1. テストデータを作成
    await prisma.employee.create({ data: employees[0] })

    // 2. 取得したデータが期待通りか検証
    const result = await prisma.employee.findMany()
    expect(result).toEqual(employees)
  })

  test('社員を更新できる', async () => {
    // 前提: 社員データを登録
    await prisma.employee.create({ data: employees[0] })

    // 1. データを更新
    const expected = { ...employees[0], name: '佐藤 太郎' }
    await prisma.employee.update({
      where: { empCode: employees[0].empCode },
      data: { name: '佐藤 太郎' },
    })

    // 2. 更新されたか検証
    const result = await prisma.employee.findUnique({
      where: { empCode: employees[0].empCode },
    })
    expect(result).toEqual(expected)
  })

  test('社員を削除できる', async () => {
    // 前提: 社員データを登録
    await prisma.employee.create({ data: employees[0] })

    // 1. データを削除
    await prisma.employee.delete({
      where: { empCode: employees[0].empCode },
    })

    // 2. テーブルが空になったか検証
    const result = await prisma.employee.findMany()
    expect(result).toEqual([])
  })

  test('部門に所属する社員を取得できる', async () => {
    // 前提: 社員データを登録
    await prisma.employee.create({ data: employees[0] })

    // 1. 部門とその所属社員を取得
    const result = await prisma.department.findUnique({
      where: {
        deptCode_startDate: {
          deptCode: departments[0].deptCode,
          startDate: departments[0].startDate,
        },
      },
      include: {
        employees: true,
      },
    })

    // 2. 社員が含まれているか検証
    expect(result?.employees).toEqual(employees)
  })
})
