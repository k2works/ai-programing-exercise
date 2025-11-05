import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { PrismaClient } from '@prisma/client'
import {
  DepartmentOptionalDefaultsSchema,
  EmployeeOptionalDefaultsSchema,
  ProductCategoryOptionalDefaultsSchema,
  ProductOptionalDefaultsSchema,
  PriceByCustomerOptionalDefaultsSchema,
  AlternateProductOptionalDefaultsSchema,
  CompanyGroupOptionalDefaultsSchema,
  CompanyOptionalDefaultsSchema,
  CustomerOptionalDefaultsSchema,
  SupplierOptionalDefaultsSchema,
  CategoryTypeOptionalDefaultsSchema,
  CompanyCategoryOptionalDefaultsSchema,
  CompanyCategoryGroupOptionalDefaultsSchema,
  OrderOptionalDefaultsSchema,
  OrderDetailOptionalDefaultsSchema,
  SalesOptionalDefaultsSchema,
  SalesDetailOptionalDefaultsSchema,
  WarehouseOptionalDefaultsSchema,
  PurchaseOrderOptionalDefaultsSchema,
  PurchaseOrderDetailOptionalDefaultsSchema,
  PurchaseOptionalDefaultsSchema,
  PurchaseDetailOptionalDefaultsSchema,
  StockOptionalDefaultsSchema,
  BankAccountOptionalDefaultsSchema,
  InvoiceOptionalDefaultsSchema,
  InvoiceDetailOptionalDefaultsSchema,
  CreditOptionalDefaultsSchema,
  PaymentOptionalDefaultsSchema
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

// テスト用の取引先グループデータ
const companyGroups = [
  CompanyGroupOptionalDefaultsSchema.parse({
    compGroupCode: 'GRP1',
    groupName: '主要取引先',
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

// テスト用の取引先データ
const companies = [
  CompanyOptionalDefaultsSchema.parse({
    compCode: 'COMP001',
    name: '株式会社サンプル商事',
    kana: 'カブシキガイシャサンプルショウジ',
    supType: 1,
    zipCode: '100-0001',
    state: '東京都',
    address1: '千代田区千代田1-1-1',
    address2: 'サンプルビル10F',
    noSalesFlg: 0,
    wideUseType: 0,
    compGroupCode: 'GRP1',
    maxCredit: 10000000,
    tempCreditUp: 2000000,
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

// テスト用の顧客データ
const customers = [
  CustomerOptionalDefaultsSchema.parse({
    custCode: 'COMP001',
    custSubNo: 1,
    custType: 1,
    arCode: 'COMP001',
    arSubNo: 1,
    payerCode: 'COMP001',
    payerSubNo: 1,
    name: '株式会社サンプル商事 本社',
    kana: 'カブシキガイシャサンプルショウジ ホンシャ',
    empCode: 'EMP999',
    custUserName: '山田太郎',
    custUserDepName: '営業部',
    custZipCode: '100-0001',
    custState: '東京都',
    custAddress1: '千代田区千代田1-1-1',
    custAddress2: 'サンプルビル10F',
    custTel: '03-1234-5678',
    custFax: '03-1234-5679',
    custEmail: 'yamada@sample.co.jp',
    custArType: 1,
    custCloseDate1: 31,
    custPayMonths1: 1,
    custPayDates1: 25,
    custPayMethod1: 1,
    custCloseDate2: 15,
    custPayMonths2: 1,
    custPayDates2: 10,
    custPayMethod2: 2,
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

// テスト用の仕入先データ
const suppliers = [
  SupplierOptionalDefaultsSchema.parse({
    supCode: 'COMP001',
    supSubNo: 1,
    name: '株式会社サンプル商事 仕入部',
    kana: 'カブシキガイシャサンプルショウジ シイレブ',
    supEmpName: '鈴木一郎',
    supDepName: '営業部',
    supZipCode: '100-0001',
    supState: '東京都',
    supAddress1: '千代田区千代田1-1-1',
    supAddress2: 'サンプルビル10F',
    supTel: '03-1234-5678',
    supFax: '03-1234-5679',
    supEmail: 'suzuki@sample.co.jp',
    supCloseDate: 31,
    supPayMonths: 2,
    supPayDates: 25,
    payMethodType: 1,
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

// テスト用の取引先分類種別データ
const categoryTypes = [
  CategoryTypeOptionalDefaultsSchema.parse({
    categoryTypeCode: '01',
    categoryTypeName: '業種',
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

// テスト用の取引先分類データ
const companyCategories = [
  CompanyCategoryOptionalDefaultsSchema.parse({
    categoryTypeCode: '01',
    compCateCode: 'IT001',
    compCateName: 'IT業',
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

// テスト用の取引先分類所属データ
const companyCategoryGroups = [
  CompanyCategoryGroupOptionalDefaultsSchema.parse({
    categoryTypeCode: '01',
    compCateCode: 'IT001',
    compCode: 'COMP001',
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  })
]

describe('取引先マスタ', () => {
  beforeEach(async () => {
    // 各テストの前にテーブルをクリーンな状態にする
    await prisma.companyCategoryGroup.deleteMany()
    await prisma.companyCategory.deleteMany()
    await prisma.categoryType.deleteMany()
    await prisma.customer.deleteMany()
    await prisma.supplier.deleteMany()
    await prisma.company.deleteMany()
    await prisma.companyGroup.deleteMany()
  })

  test('取引先を登録できる', async () => {
    // Company, Customer, Supplierをまとめて登録
    await prisma.$transaction(async (prisma) => {
      await prisma.companyGroup.createMany({ data: companyGroups })
      await prisma.company.createMany({ data: companies })
      await prisma.customer.createMany({ data: customers })
      await prisma.supplier.createMany({ data: suppliers })
    })

    // 関連モデルを含めて取得し、検証
    const result = await prisma.company.findMany({
      include: {
        customers: true,
        suppliers: true
      }
    })

    const expected = companies.map((c) => ({
      ...c,
      customers: customers.filter((cust) => cust.custCode === c.compCode),
      suppliers: suppliers.filter((sup) => sup.supCode === c.compCode)
    }))

    expect(result).toEqual(expected)
  })

  test('取引先をグループ化できる', async () => {
    // 前提: 取引先グループと取引先を登録
    await prisma.$transaction(async (prisma) => {
      await prisma.companyGroup.createMany({ data: companyGroups })
      await prisma.company.createMany({ data: companies })
    })

    const expected = {
      ...companyGroups[0],
      companies: companies.filter((c) => c.compGroupCode === companyGroups[0].compGroupCode)
    }

    // 取引先グループを取得
    const companyGroup = await prisma.companyGroup.findUnique({
      where: { compGroupCode: companyGroups[0].compGroupCode }
    })

    // 所属する取引先を取得
    const companiesInGroup = await prisma.company.findMany({
      where: { compGroupCode: companyGroups[0].compGroupCode }
    })

    const result = {
      ...companyGroup,
      companies: companiesInGroup
    }

    expect(result).toEqual(expected)
  })

  test('取引先を分類で整理できる', async () => {
    // 前提: 全データを登録
    await prisma.$transaction(async (prisma) => {
      await prisma.companyGroup.createMany({ data: companyGroups })
      await prisma.company.createMany({ data: companies })
      await prisma.categoryType.createMany({ data: categoryTypes })
      await prisma.companyCategory.createMany({ data: companyCategories })
      await prisma.companyCategoryGroup.createMany({ data: companyCategoryGroups })
    })

    // 取引先分類を含めて取得
    const result = await prisma.company.findUnique({
      where: { compCode: companies[0].compCode },
      include: {
        companyCategoryGroups: {
          include: {
            companyCategory: {
              include: {
                categoryType: true
              }
            }
          }
        }
      }
    })

    expect(result).toBeTruthy()
    expect(result?.companyCategoryGroups).toHaveLength(1)
    expect(result?.companyCategoryGroups[0].companyCategory.compCateName).toBe('IT業')
    expect(result?.companyCategoryGroups[0].companyCategory.categoryType.categoryTypeName).toBe(
      '業種'
    )
  })
})

// テスト用の受注データ
const orders = [
  OrderOptionalDefaultsSchema.parse({
    orderNo: 'ORD0000001',
    orderDate: new Date('2023-05-15'),
    deptCode: '11101',
    startDate: new Date('2021-01-01'),
    custCode: 'COMP001',
    custSubNo: 1,
    empCode: 'EMP001',
    requiredDate: new Date('2023-05-20'),
    custorderNo: 'CUST-ORD-001',
    whCode: 'WH1',
    orderAmnt: 100000,
    cmpTax: 10000,
    slipComment: '通常受注',
    createDate: new Date('2023-05-15'),
    creator: 'admin',
    updateDate: new Date('2023-05-15'),
    updater: 'admin'
  })
]

// テスト用の受注明細データ
const orderDetails = [
  OrderDetailOptionalDefaultsSchema.parse({
    orderNo: 'ORD0000001',
    soRowNo: 1,
    prodCode: 'PROD001',
    prodName: 'テスト商品A',
    unitprice: 50000,
    quantity: 2,
    cmpTaxRate: 10,
    reserveQty: 2,
    deliveryOrderQty: 0,
    deliveredQty: 0,
    completeFlg: 0,
    discount: 0,
    deliveryDate: new Date('2023-05-20'),
    createDate: new Date('2023-05-15'),
    creator: 'admin',
    updateDate: new Date('2023-05-15'),
    updater: 'admin'
  })
]

// テスト用の売上データ
const sales = [
  SalesOptionalDefaultsSchema.parse({
    salesNo: 'SAL0000001',
    orderNo: 'ORD0000001',
    salesDate: new Date('2023-05-16'),
    salesType: 1,
    deptCode: '11101',
    startDate: new Date('2021-01-01'),
    compCode: 'COMP001',
    empCode: 'EMP001',
    salesAmnt: 100000,
    cmpTax: 10000,
    slipComment: '通常売上',
    updatedNo: null,
    orgnlNo: null,
    createDate: new Date('2023-05-16'),
    creator: 'admin',
    updateDate: new Date('2023-05-16'),
    updater: 'admin'
  })
]

// テスト用の売上明細データ
const salesDetails = [
  SalesDetailOptionalDefaultsSchema.parse({
    salesNo: 'SAL0000001',
    rowNo: 1,
    prodCode: 'PROD001',
    prodName: 'テスト商品A',
    unitprice: 50000,
    deliveredQty: 2,
    quantity: 2,
    discount: 0,
    invoicedDate: null,
    invoiceNo: null,
    invoiceDelayType: null,
    autoJournalDate: null,
    createDate: new Date('2023-05-16'),
    creator: 'admin',
    updateDate: new Date('2023-05-16'),
    updater: 'admin'
  })
]

describe('受注と売上', () => {
  beforeEach(async () => {
    // 各テストの前にテーブルをクリーンな状態にする
    await prisma.invoiceDetail.deleteMany()
    await prisma.invoice.deleteMany()
    await prisma.salesDetail.deleteMany()
    await prisma.sales.deleteMany()
    await prisma.orderDetail.deleteMany()
    await prisma.order.deleteMany()
    await prisma.customer.deleteMany()
    await prisma.companyCategoryGroup.deleteMany()
    await prisma.companyCategory.deleteMany()
    await prisma.categoryType.deleteMany()
    await prisma.company.deleteMany()
    await prisma.companyGroup.deleteMany()
    await prisma.employee.deleteMany()
    await prisma.department.deleteMany()

    // 前提データの登録
    await prisma.$transaction(async (prisma) => {
      await prisma.department.createMany({ data: departments })
      await prisma.employee.createMany({ data: employees })
      await prisma.companyGroup.createMany({ data: companyGroups })
      await prisma.company.createMany({ data: companies })
      await prisma.customer.createMany({ data: customers })
    })
  })

  test('受注を登録できる', async () => {
    const expected = orders.map((o) => {
      return {
        ...o,
        orderDetails: orderDetails.filter((od) => od.orderNo === o.orderNo)
      }
    })

    await prisma.$transaction(async (prisma) => {
      await prisma.order.createMany({ data: orders })
      await prisma.orderDetail.createMany({ data: orderDetails })
    })

    const result = await prisma.order.findMany({
      include: {
        orderDetails: true
      }
    })

    expect(result).toEqual(expected)
  })

  test('売上を登録できる', async () => {
    // 前提: 受注データを登録
    await prisma.$transaction(async (prisma) => {
      await prisma.order.createMany({ data: orders })
      await prisma.orderDetail.createMany({ data: orderDetails })
    })

    const expected = sales.map((s) => {
      return {
        ...s,
        salesDetails: salesDetails.filter((sd) => sd.salesNo === s.salesNo)
      }
    })

    await prisma.$transaction(async (prisma) => {
      await prisma.sales.createMany({ data: sales })
      await prisma.salesDetail.createMany({ data: salesDetails })
    })

    const result = await prisma.sales.findMany({
      include: {
        salesDetails: true
      }
    })

    expect(result).toEqual(expected)
  })

  test('受注から売上への流れを追跡できる', async () => {
    // 受注→売上の一連の流れを登録
    await prisma.$transaction(async (prisma) => {
      await prisma.order.createMany({ data: orders })
      await prisma.orderDetail.createMany({ data: orderDetails })
      await prisma.sales.createMany({ data: sales })
      await prisma.salesDetail.createMany({ data: salesDetails })
    })

    // 受注データを売上データと一緒に取得
    const result = await prisma.order.findUnique({
      where: { orderNo: orders[0].orderNo },
      include: {
        orderDetails: true,
        sales: {
          include: {
            salesDetails: true
          }
        }
      }
    })

    expect(result).toBeTruthy()
    expect(result?.orderNo).toBe('ORD0000001')
    expect(result?.orderDetails).toHaveLength(1)
    expect(result?.sales).toHaveLength(1)
    expect(result?.sales[0].salesNo).toBe('SAL0000001')
    expect(result?.sales[0].salesDetails).toHaveLength(1)
  })
})

// テスト用の倉庫データ
const warehouses = [
  WarehouseOptionalDefaultsSchema.parse({
    whCode: 'WH1',
    name: '東京倉庫',
    createDate: new Date('2023-05-15'),
    creator: 'admin',
    updateDate: new Date('2023-05-15'),
    updater: 'admin'
  })
]

// テスト用の発注データ
const purchaseOrders = [
  PurchaseOrderOptionalDefaultsSchema.parse({
    poNo: 'PO0000001',
    poDate: new Date('2023-05-15'),
    orderNo: 'ORD0000001',
    supCode: 'COMP001',
    supSubNo: 1,
    empCode: 'EMP001',
    dueDate: new Date('2023-05-25'),
    whCode: 'WH1',
    poAmnt: 80000,
    cmpTax: 8000,
    slipComment: '通常発注',
    createDate: new Date('2023-05-15'),
    creator: 'admin',
    updateDate: new Date('2023-05-15'),
    updater: 'admin'
  })
]

// テスト用の発注明細データ
const purchaseOrderDetails = [
  PurchaseOrderDetailOptionalDefaultsSchema.parse({
    poNo: 'PO0000001',
    poRowNo: 1,
    prodCode: 'PROD001',
    prodName: 'テスト商品A',
    unitprice: 40000,
    quantity: 2,
    arrivedQty: 0,
    completeFlg: 0,
    dueDate: new Date('2023-05-25'),
    createDate: new Date('2023-05-15'),
    creator: 'admin',
    updateDate: new Date('2023-05-15'),
    updater: 'admin'
  })
]

// テスト用の仕入データ
const purchases = [
  PurchaseOptionalDefaultsSchema.parse({
    puNo: 'PU0000001',
    puDate: new Date('2023-05-20'),
    supCode: 'COMP001',
    supSubNo: 1,
    empCode: 'EMP001',
    startDate: new Date('2021-01-01'),
    poNo: 'PO0000001',
    deptCode: '11101',
    puAmmount: 80000,
    cmpTax: 8000,
    slipComment: '通常仕入',
    createDate: new Date('2023-05-20'),
    creator: 'admin',
    updateDate: new Date('2023-05-20'),
    updater: 'admin'
  })
]

// テスト用の仕入明細データ
const purchaseDetails = [
  PurchaseDetailOptionalDefaultsSchema.parse({
    puNo: 'PU0000001',
    puRowNo: 1,
    prodCode: 'PROD001',
    prodName: 'テスト商品A',
    unitprice: 40000,
    quantity: 2,
    createDate: new Date('2023-05-20'),
    creator: 'admin',
    updateDate: new Date('2023-05-20'),
    updater: 'admin'
  })
]

// テスト用の在庫データ
const stocks = [
  StockOptionalDefaultsSchema.parse({
    whCode: 'WH1',
    prodCode: 'PROD001',
    rotNo: 'LOT001',
    stockType: '1',
    qualityType: 'G',
    actual: 10,
    valid: 10,
    lastDeliveryDate: null,
    createDate: new Date('2023-05-20'),
    creator: 'admin',
    updateDate: new Date('2023-05-20'),
    updater: 'admin'
  })
]

describe('発注・仕入・在庫管理', () => {
  beforeEach(async () => {
    // 各テストの前にテーブルをクリーンな状態にする
    await prisma.stock.deleteMany()
    await prisma.purchaseDetail.deleteMany()
    await prisma.purchase.deleteMany()
    await prisma.purchaseOrderDetail.deleteMany()
    await prisma.purchaseOrder.deleteMany()
    await prisma.warehouse.deleteMany()
    await prisma.customer.deleteMany()
    await prisma.supplier.deleteMany()
    await prisma.companyCategoryGroup.deleteMany()
    await prisma.companyCategory.deleteMany()
    await prisma.categoryType.deleteMany()
    await prisma.company.deleteMany()
    await prisma.companyGroup.deleteMany()
    await prisma.employee.deleteMany()
    await prisma.department.deleteMany()

    // 前提データの登録
    await prisma.$transaction(async (prisma) => {
      await prisma.department.createMany({ data: departments })
      await prisma.employee.createMany({ data: employees })
      await prisma.companyGroup.createMany({ data: companyGroups })
      await prisma.company.createMany({ data: companies })
      await prisma.supplier.createMany({ data: suppliers })
      await prisma.warehouse.createMany({ data: warehouses })
    })
  })

  test('発注を登録できる', async () => {
    const expected = purchaseOrders.map((po) => {
      return {
        ...po,
        purchaseOrderDetails: purchaseOrderDetails.filter((pod) => pod.poNo === po.poNo)
      }
    })

    await prisma.$transaction(async (prisma) => {
      await prisma.purchaseOrder.createMany({ data: purchaseOrders })
      await prisma.purchaseOrderDetail.createMany({ data: purchaseOrderDetails })
    })

    const result = await prisma.purchaseOrder.findMany({
      include: {
        purchaseOrderDetails: true
      }
    })

    expect(result).toEqual(expected)
  })

  test('仕入を登録できる', async () => {
    // 前提: 発注データを登録
    await prisma.$transaction(async (prisma) => {
      await prisma.purchaseOrder.createMany({ data: purchaseOrders })
      await prisma.purchaseOrderDetail.createMany({ data: purchaseOrderDetails })
    })

    const expected = purchases.map((p) => {
      return {
        ...p,
        purchaseDetails: purchaseDetails.filter((pd) => pd.puNo === p.puNo)
      }
    })

    await prisma.$transaction(async (prisma) => {
      await prisma.purchase.createMany({ data: purchases })
      await prisma.purchaseDetail.createMany({ data: purchaseDetails })
    })

    const result = await prisma.purchase.findMany({
      include: {
        purchaseDetails: true
      }
    })

    expect(result).toEqual(expected)
  })

  test('在庫を登録できる', async () => {
    await prisma.stock.createMany({ data: stocks })
    const result = await prisma.stock.findMany()
    expect(result).toEqual(stocks)
  })

  test('発注から仕入への流れを追跡できる', async () => {
    // 発注→仕入の一連の流れを登録
    await prisma.$transaction(async (prisma) => {
      await prisma.purchaseOrder.createMany({ data: purchaseOrders })
      await prisma.purchaseOrderDetail.createMany({ data: purchaseOrderDetails })
      await prisma.purchase.createMany({ data: purchases })
      await prisma.purchaseDetail.createMany({ data: purchaseDetails })
    })

    // 発注データを仕入データと一緒に取得
    const result = await prisma.purchaseOrder.findUnique({
      where: { poNo: purchaseOrders[0].poNo },
      include: {
        purchaseOrderDetails: true,
        purchases: {
          include: {
            purchaseDetails: true
          }
        }
      }
    })

    expect(result).toBeTruthy()
    expect(result?.poNo).toBe('PO0000001')
    expect(result?.purchaseOrderDetails).toHaveLength(1)
    expect(result?.purchases).toHaveLength(1)
    expect(result?.purchases[0].puNo).toBe('PU0000001')
    expect(result?.purchases[0].purchaseDetails).toHaveLength(1)
  })
})

// テスト用の入金口座データ
const bankAccounts = [
  BankAccountOptionalDefaultsSchema.parse({
    bankAcutCode: 'BANK0001',
    bankName: 'テスト銀行',
    branchName: '本店',
    accountType: 1,
    accountNo: '1234567',
    creator: 'TEST_USER',
    updater: 'TEST_USER'
  })
]

// テスト用の請求データ
const invoices = [
  InvoiceOptionalDefaultsSchema.parse({
    invoiceNo: 'INV0000001',
    invoicedDate: new Date('2023-05-01'),
    compCode: 'CMP00001',
    custSubNo: 0,
    lastReceived: 0,
    monthSales: 100000,
    monthReceived: 0,
    monthInvoice: 110000,
    cmpTax: 10000,
    invoiceReceived: 0,
    createDate: new Date('2025-11-05'),
    creator: 'TEST_USER',
    updateDate: new Date('2025-11-05'),
    updater: 'TEST_USER'
  })
]

// テスト用の請求明細データ
const invoiceDetails = [
  InvoiceDetailOptionalDefaultsSchema.parse({
    invoiceNo: 'INV0000001',
    rowNo: 1,
    salesNo: 'SAL0000001',
    salesRowNo: 1,
    salesAmnt: 100000,
    invoicedAmnt: 110000,
    createDate: new Date('2025-11-05'),
    creator: 'TEST_USER',
    updateDate: new Date('2025-11-05'),
    updater: 'TEST_USER'
  })
]

// テスト用の入金データ
const credits = [
  CreditOptionalDefaultsSchema.parse({
    creditNo: 'CRD0000001',
    creditDate: new Date('2023-05-15'),
    deptCode: '11101',
    startDate: new Date('2021-01-01'),
    custCode: 'CMP00001',
    custSubNo: 0,
    invoiceNo: 'INV0000001',
    payMethodType: 1,
    bankAcutCode: 'BANK0001',
    receivedAmnt: 110000,
    received: 110000,
    createDate: new Date('2025-11-05'),
    creator: 'TEST_USER',
    updateDate: new Date('2025-11-05'),
    updater: 'TEST_USER',
    updatePlgDate: new Date('2025-11-05'),
    updatePgm: null
  })
]

// テスト用の支払データ
const payments = [
  PaymentOptionalDefaultsSchema.parse({
    payNo: 'PAY0000001',
    payDate: 20230515,
    deptCode: '11101',
    startDate: new Date('2021-01-01'),
    supCode: 'SUP00001',
    supSubNo: 0,
    payMethodType: 1,
    payAmnt: 50000,
    cmpTax: 5000,
    completeFlg: 1,
    createDate: new Date('2025-11-05'),
    creator: 'TEST_USER',
    updateDate: new Date('2025-11-05'),
    updater: 'TEST_USER'
  })
]

describe('請求・入金・支払管理', () => {
  beforeEach(async () => {
    // 各テストの前にテーブルをクリーンな状態にする
    await prisma.payment.deleteMany()
    await prisma.credit.deleteMany()
    await prisma.invoiceDetail.deleteMany()
    await prisma.invoice.deleteMany()
    await prisma.bankAccount.deleteMany()
    await prisma.salesDetail.deleteMany()
    await prisma.sales.deleteMany()
    await prisma.orderDetail.deleteMany()
    await prisma.order.deleteMany()
    await prisma.customer.deleteMany()
    await prisma.supplier.deleteMany()
    await prisma.companyCategoryGroup.deleteMany()
    await prisma.companyCategory.deleteMany()
    await prisma.categoryType.deleteMany()
    await prisma.company.deleteMany()
    await prisma.companyGroup.deleteMany()
    await prisma.employee.deleteMany()
    await prisma.department.deleteMany()

    // 前提データの登録
    await prisma.$transaction(async (prisma) => {
      await prisma.department.createMany({ data: departments })
      await prisma.employee.createMany({ data: employees })
      await prisma.companyGroup.createMany({ data: companyGroups })
      await prisma.company.createMany({ data: companies })
      await prisma.customer.createMany({ data: customers })
      await prisma.supplier.createMany({ data: suppliers })
      await prisma.bankAccount.createMany({ data: bankAccounts })
      // 請求のために売上データも登録
      await prisma.order.createMany({ data: orders })
      await prisma.orderDetail.createMany({ data: orderDetails })
      await prisma.sales.createMany({ data: sales })
      await prisma.salesDetail.createMany({ data: salesDetails })
    })
  })

  test('請求を登録できる', async () => {
    const expected = invoices.map((i) => {
      return {
        ...i,
        invoiceDetails: invoiceDetails.filter((id) => id.invoiceNo === i.invoiceNo)
      }
    })

    await prisma.$transaction(async (prisma) => {
      await prisma.invoice.createMany({ data: invoices })
      await prisma.invoiceDetail.createMany({ data: invoiceDetails })
    })

    const result = await prisma.invoice.findMany({
      include: {
        invoiceDetails: true
      }
    })

    expect(result).toEqual(expected)
  })

  test('入金を登録できる', async () => {
    // 前提: 請求データを登録
    await prisma.$transaction(async (prisma) => {
      await prisma.invoice.createMany({ data: invoices })
      await prisma.invoiceDetail.createMany({ data: invoiceDetails })
    })

    await prisma.credit.createMany({ data: credits })
    const result = await prisma.credit.findMany()
    expect(result).toEqual(credits)
  })

  test('支払を登録できる', async () => {
    await prisma.payment.createMany({ data: payments })
    const result = await prisma.payment.findMany()
    expect(result).toEqual(payments)
  })

  test('売上から請求、入金への流れを追跡できる', async () => {
    // 売上→請求→入金の一連の流れを登録
    await prisma.$transaction(async (prisma) => {
      await prisma.invoice.createMany({ data: invoices })
      await prisma.invoiceDetail.createMany({ data: invoiceDetails })
      await prisma.credit.createMany({ data: credits })
    })

    // 売上データを請求、入金データと一緒に取得
    const salesResult = await prisma.sales.findUnique({
      where: { salesNo: sales[0].salesNo },
      include: {
        salesDetails: {
          include: {
            invoiceDetails: {
              include: {
                invoice: {
                  include: {
                    credits: true
                  }
                }
              }
            }
          }
        }
      }
    })

    expect(salesResult).toBeTruthy()
    expect(salesResult?.salesNo).toBe('SAL0000001')
    expect(salesResult?.salesDetails).toHaveLength(1)
    expect(salesResult?.salesDetails[0].invoiceDetails).toHaveLength(1)
    expect(salesResult?.salesDetails[0].invoiceDetails[0].invoiceNo).toBe('INV0000001')
    expect(salesResult?.salesDetails[0].invoiceDetails[0].invoice.credits).toHaveLength(1)
    expect(salesResult?.salesDetails[0].invoiceDetails[0].invoice.credits[0].creditNo).toBe(
      'CRD0000001'
    )
  })
})
