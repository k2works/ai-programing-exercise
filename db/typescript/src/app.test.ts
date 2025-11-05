import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { PrismaClient } from '@prisma/client'
import {
  DepartmentOptionalDefaultsSchema,
  EmployeeOptionalDefaultsSchema,
  ProductCategoryOptionalDefaultsSchema,
  ProductOptionalDefaultsSchema,
  PriceByCustomerOptionalDefaultsSchema,
  AlternateProductOptionalDefaultsSchema
} from './generated/zod'

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
  DepartmentOptionalDefaultsSchema.parse({
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
  })
]

// テスト用の社員データ
const employees = [
  EmployeeOptionalDefaultsSchema.parse({
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
  })
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
    // 前提: 部門データを登録
    await prisma.department.create({ data: departments[0] })

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
      data: { name: '佐藤 太郎' }
    })

    // 2. 更新されたか検証
    const result = await prisma.employee.findUnique({
      where: { empCode: employees[0].empCode }
    })
    expect(result).toEqual(expected)
  })

  test('社員を削除できる', async () => {
    // 前提: 社員データを登録
    await prisma.employee.create({ data: employees[0] })

    // 1. データを削除
    await prisma.employee.delete({
      where: { empCode: employees[0].empCode }
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
          startDate: departments[0].startDate
        }
      },
      include: {
        employees: true
      }
    })

    // 2. 社員が含まれているか検証
    expect(result?.employees).toEqual(employees)
  })
})

// テスト用の商品分類データ
const productCategories = [
  ProductCategoryOptionalDefaultsSchema.parse({
    categoryCode: 'CAT001',
    name: '電化製品',
    layer: 1,
    path: 'CAT001',
    lowestType: 1,
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

// テスト用の商品データ
const products = [
  ProductOptionalDefaultsSchema.parse({
    prodCode: 'PROD001',
    fullname: 'ノートパソコン ProBook 450',
    name: 'ProBook',
    kana: 'プロブック',
    prodType: '1',
    serialNo: 'HP-PB450-2023',
    unitprice: 150000,
    poPrice: 120000,
    primeCost: 110000,
    taxType: 1,
    categoryCode: 'CAT001',
    wideUseType: 0,
    stockManageType: 1,
    stockReserveType: 1,
    supCode: 'SUP001',
    supSubNo: 1,
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  }),
  ProductOptionalDefaultsSchema.parse({
    prodCode: 'PROD002',
    fullname: 'ノートパソコン EliteBook 840',
    name: 'EliteBook',
    kana: 'エリートブック',
    prodType: '1',
    serialNo: 'HP-EB840-2023',
    unitprice: 200000,
    poPrice: 160000,
    primeCost: 150000,
    taxType: 1,
    categoryCode: 'CAT001',
    wideUseType: 0,
    stockManageType: 1,
    stockReserveType: 1,
    supCode: 'SUP001',
    supSubNo: 1,
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

// テスト用の顧客別販売単価データ
const priceByCustomers = [
  PriceByCustomerOptionalDefaultsSchema.parse({
    prodCode: 'PROD001',
    compCode: 'COMP001',
    unitprice: 145000,
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

// テスト用の代替商品データ
const alternateProducts = [
  AlternateProductOptionalDefaultsSchema.parse({
    prodCode: 'PROD001',
    altProdCode: 'PROD002',
    priority: 1,
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

describe('商品マスタ', () => {
  beforeEach(async () => {
    // 各テストの前にテーブルをクリーンな状態にする
    await prisma.alternateProduct.deleteMany()
    await prisma.priceByCustomer.deleteMany()
    await prisma.product.deleteMany()
    await prisma.productCategory.deleteMany()
  })

  test('商品分類を登録できる', async () => {
    // 1. テストデータを作成
    await prisma.productCategory.create({ data: productCategories[0] })

    // 2. 取得したデータが期待通りか検証
    const result = await prisma.productCategory.findMany()
    expect(result).toEqual(productCategories)
  })

  test('商品を登録できる', async () => {
    // 前提: 商品分類データを登録
    await prisma.productCategory.create({ data: productCategories[0] })

    // 1. 商品データを作成
    await prisma.product.create({ data: products[0] })

    // 2. 取得したデータが期待通りか検証
    const result = await prisma.product.findMany()
    expect(result).toEqual([products[0]])
  })

  test('商品と関連データを一括登録できる', async () => {
    // トランザクションを使って関連データを一括登録
    await prisma.$transaction(async (prisma) => {
      await prisma.productCategory.createMany({ data: productCategories })
      await prisma.product.createMany({ data: products })
      await prisma.alternateProduct.createMany({ data: alternateProducts })
      await prisma.priceByCustomer.createMany({ data: priceByCustomers })
    })

    // 商品が登録されていることを検証
    const productResult = await prisma.product.findMany()
    expect(productResult.length).toBe(2)

    // 代替商品が登録されていることを検証
    const alternateResult = await prisma.alternateProduct.findMany()
    expect(alternateResult).toEqual(alternateProducts)

    // 顧客別販売単価が登録されていることを検証
    const priceResult = await prisma.priceByCustomer.findMany()
    expect(priceResult).toEqual(priceByCustomers)
  })

  test('商品に関連データを含めて取得できる', async () => {
    // 前提: 関連データを登録
    await prisma.$transaction(async (prisma) => {
      await prisma.productCategory.createMany({ data: productCategories })
      await prisma.product.createMany({ data: products })
      await prisma.alternateProduct.createMany({ data: alternateProducts })
      await prisma.priceByCustomer.createMany({ data: priceByCustomers })
    })

    const expected = {
      ...products[0],
      alternateProducts: alternateProducts,
      pricebycustomers: priceByCustomers
    }

    // 関連データを含めて取得
    const result = await prisma.product.findUnique({
      where: { prodCode: products[0].prodCode },
      include: {
        alternateProducts: true,
        pricebycustomers: true
      }
    })

    expect(result).toEqual(expected)
  })

  test('商品を更新できる', async () => {
    // 前提: 商品分類と商品データを登録
    await prisma.productCategory.create({ data: productCategories[0] })
    await prisma.product.create({ data: products[0] })

    // 1. 商品データを更新
    const expected = { ...products[0], name: '更新商品' }
    await prisma.product.update({
      where: { prodCode: products[0].prodCode },
      data: { name: '更新商品' }
    })

    // 2. 更新されたか検証
    const result = await prisma.product.findUnique({
      where: { prodCode: products[0].prodCode }
    })
    expect(result).toEqual(expected)
  })

  test('商品を削除できる', async () => {
    // 前提: 商品分類と商品データを登録
    await prisma.productCategory.create({ data: productCategories[0] })
    await prisma.product.create({ data: products[0] })

    // 1. 商品データを削除
    await prisma.product.delete({
      where: { prodCode: products[0].prodCode }
    })

    // 2. テーブルが空になったか検証
    const result = await prisma.product.findMany()
    expect(result).toEqual([])
  })
})
