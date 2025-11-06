/* eslint-disable no-undef */
import { describe, test, expect, beforeAll, afterAll } from 'vitest'
import {
  PrismaClient,
  Department,
  Employee,
  Product,
  Company,
  CompanyGroup,
  Order,
  OrderDetail,
  Warehouse,
  PurchaseOrder,
  PurchaseOrderDetail,
  Purchase,
  PurchaseDetail,
  Stock,
  Invoice,
  InvoiceDetail,
  Credit,
  Payment
} from '@prisma/client'

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

// テスト用の商品データ
const products: Product[] = [
  {
    prodCode: 'PROD001',
    fullname: 'テスト商品',
    name: 'テスト',
    kana: 'テストショウヒン',
    prodType: '1',
    serialNo: 'SN001',
    unitprice: 1000,
    poPrice: 800,
    primeCost: 700,
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
  }
]

describe('商品マスタ', () => {
  // 各テストの前に、テーブルをクリーンな状態にする
  beforeAll(async () => {
    await prisma.product.deleteMany()
  })

  afterAll(async () => {
    await prisma.$disconnect()
  })

  test('商品を登録できる', async () => {
    // 1. テストデータを作成
    await prisma.product.create({ data: products[0] })
    // 2. 取得したデータが期待通りか検証
    const result = await prisma.product.findMany()
    expect(result).toEqual(products)
  })

  test('商品を更新できる', async () => {
    // 1. データを「更新商品」という名前に更新
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
    // 1. データを削除
    await prisma.product.delete({ where: { prodCode: products[0].prodCode } })
    // 2. テーブルが空になったか検証
    const result = await prisma.product.findMany()
    expect(result).toEqual([])
  })
})

// テスト用の取引先グループデータ
const companyGroups: CompanyGroup[] = [
  {
    compGroupCode: 'GRP1',
    groupName: 'テストグループ',
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  }
]

// テスト用の取引先データ
const companies: Company[] = [
  {
    compCode: 'COMP001',
    name: 'テスト取引先',
    kana: 'テストトリヒキサキ',
    supType: 1,
    zipCode: '1000001',
    state: '東京都',
    address1: 'テスト区',
    address2: 'テスト町1-1-1',
    noSalesFlg: 0,
    wideUseType: 0,
    compGroupCode: 'GRP1',
    maxCredit: 1000000,
    tempCreditUp: 100000,
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  }
]

describe('取引先マスタ', () => {
  beforeAll(async () => {
    await prisma.company.deleteMany()
    await prisma.companyGroup.deleteMany()
    await prisma.companyGroup.create({ data: companyGroups[0] })
  })

  afterAll(async () => {
    await prisma.company.deleteMany()
    await prisma.companyGroup.deleteMany()
    await prisma.$disconnect()
  })

  test('取引先を登録できる', async () => {
    await prisma.company.create({ data: companies[0] })
    const result = await prisma.company.findMany()
    expect(result).toEqual(companies)
  })

  test('取引先を更新できる', async () => {
    await prisma.company.update({
      where: { compCode: companies[0].compCode },
      data: { name: '更新取引先' }
    })
    const result = await prisma.company.findUnique({
      where: { compCode: companies[0].compCode }
    })
    expect(result?.name).toEqual('更新取引先')
    expect(result?.compCode).toEqual(companies[0].compCode)
  })

  test('取引先を削除できる', async () => {
    await prisma.company.delete({ where: { compCode: companies[0].compCode } })
    const result = await prisma.company.findMany()
    expect(result).toEqual([])
  })
})

// テスト用の受注データ
const orders: Order[] = [
  {
    orderNo: 'ORD0001',
    orderDate: new Date('2021-01-01'),
    deptCode: 'DEPT01',
    startDate: new Date('2021-01-01'),
    custCode: 'COMP001',
    custSubNo: 1,
    empCode: 'EMP001',
    requiredDate: new Date('2021-01-15'),
    custorderNo: 'CUST-ORD-001',
    whCode: 'WH1',
    orderAmnt: 10000,
    cmpTax: 1000,
    slipComment: 'テスト受注',
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  }
]

// テスト用の受注明細データ
const orderDetails: OrderDetail[] = [
  {
    orderNo: 'ORD0001',
    soRowNo: 1,
    prodCode: 'PROD001',
    prodName: 'テスト商品',
    unitprice: 1000,
    quantity: 10,
    cmpTaxRate: 10,
    reserveQty: 10,
    deliveryOrderQty: 10,
    deliveredQty: 0,
    completeFlg: 0,
    discount: 0,
    deliveryDate: new Date('2021-01-15'),
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  }
]

describe('受注マスタ', () => {
  beforeAll(async () => {
    await prisma.orderDetail.deleteMany()
    await prisma.order.deleteMany()
  })

  afterAll(async () => {
    await prisma.orderDetail.deleteMany()
    await prisma.order.deleteMany()
    await prisma.$disconnect()
  })

  test('受注を登録できる', async () => {
    await prisma.order.create({ data: orders[0] })
    const result = await prisma.order.findMany()
    expect(result).toEqual(orders)
  })

  test('受注を更新できる', async () => {
    const expected = { ...orders[0], orderAmnt: 20000 }
    await prisma.order.update({
      where: { orderNo: orders[0].orderNo },
      data: { orderAmnt: 20000 }
    })
    const result = await prisma.order.findUnique({
      where: { orderNo: orders[0].orderNo }
    })
    expect(result).toEqual(expected)
  })

  test('受注を削除できる', async () => {
    await prisma.order.delete({ where: { orderNo: orders[0].orderNo } })
    const result = await prisma.order.findMany()
    expect(result).toEqual([])
  })
})

describe('受注明細', () => {
  beforeAll(async () => {
    await prisma.orderDetail.deleteMany()
    await prisma.order.deleteMany()
    await prisma.order.create({ data: orders[0] })
  })

  afterAll(async () => {
    await prisma.orderDetail.deleteMany()
    await prisma.order.deleteMany()
    await prisma.$disconnect()
  })

  test('受注明細を登録できる', async () => {
    await prisma.orderDetail.create({ data: orderDetails[0] })
    const result = await prisma.orderDetail.findMany()
    expect(result).toEqual(orderDetails)
  })

  test('受注明細を更新できる', async () => {
    const expected = { ...orderDetails[0], quantity: 20 }
    await prisma.orderDetail.update({
      where: {
        orderNo_soRowNo: {
          orderNo: orderDetails[0].orderNo,
          soRowNo: orderDetails[0].soRowNo
        }
      },
      data: { quantity: 20 }
    })
    const result = await prisma.orderDetail.findUnique({
      where: {
        orderNo_soRowNo: {
          orderNo: orderDetails[0].orderNo,
          soRowNo: orderDetails[0].soRowNo
        }
      }
    })
    expect(result).toEqual(expected)
  })

  test('受注明細を削除できる', async () => {
    await prisma.orderDetail.delete({
      where: {
        orderNo_soRowNo: {
          orderNo: orderDetails[0].orderNo,
          soRowNo: orderDetails[0].soRowNo
        }
      }
    })
    const result = await prisma.orderDetail.findMany()
    expect(result).toEqual([])
  })
})

// テスト用の倉庫データ
const warehouses: Warehouse[] = [
  {
    whCode: 'WH1',
    name: 'メイン倉庫',
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  }
]

// テスト用の発注データ
const purchaseOrders: PurchaseOrder[] = [
  {
    poNo: 'PO0001',
    poDate: new Date('2021-01-01'),
    orderNo: 'ORD0001',
    supCode: 'COMP001',
    supSubNo: 1,
    empCode: 'EMP001',
    dueDate: new Date('2021-01-15'),
    whCode: 'WH1',
    poAmnt: 8000,
    cmpTax: 800,
    slipComment: 'テスト発注',
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  }
]

// テスト用の発注明細データ
const purchaseOrderDetails: PurchaseOrderDetail[] = [
  {
    poNo: 'PO0001',
    poRowNo: 1,
    prodCode: 'PROD001',
    prodName: 'テスト商品',
    unitprice: 800,
    quantity: 10,
    cmpTaxRate: 10,
    receivedQty: 0,
    completeFlg: 0,
    discount: 0,
    deliveryDate: new Date('2021-01-15'),
    createDate: new Date('2021-01-01'),
    creator: 'admin',
    updateDate: new Date('2021-01-01'),
    updater: 'admin'
  }
]

// テスト用の仕入データ
const purchases: Purchase[] = [
  {
    puNo: 'PU0001',
    puDate: new Date('2021-01-10'),
    supCode: 'COMP001',
    supSubNo: 1,
    empCode: 'EMP001',
    startDate: new Date('2021-01-01'),
    poNo: 'PO0001',
    deptCode: 'DEPT01',
    puAmmount: 8000,
    cmpTax: 800,
    slipComment: 'テスト仕入',
    createDate: new Date('2021-01-10'),
    creator: 'admin',
    updateDate: new Date('2021-01-10'),
    updater: 'admin'
  }
]

// テスト用の仕入明細データ
const purchaseDetails: PurchaseDetail[] = [
  {
    puNo: 'PU0001',
    puRowNo: 1,
    prodCode: 'PROD001',
    prodName: 'テスト商品',
    unitprice: 800,
    quantity: 10,
    cmpTaxRate: 10,
    discount: 0,
    rotNo: 'LOT001',
    createDate: new Date('2021-01-10'),
    creator: 'admin',
    updateDate: new Date('2021-01-10'),
    updater: 'admin'
  }
]

// テスト用の在庫データ
const stocks: Stock[] = [
  {
    whCode: 'WH1',
    prodCode: 'PROD001',
    rotNo: 'LOT001',
    stockType: '1',
    qualityType: '1',
    actual: 10,
    valid: 10,
    lastDeliveryDate: null,
    createDate: new Date('2021-01-10'),
    creator: 'admin',
    updateDate: new Date('2021-01-10'),
    updater: 'admin'
  }
]

describe('倉庫マスタ', () => {
  beforeAll(async () => {
    await prisma.warehouse.deleteMany()
  })

  afterAll(async () => {
    await prisma.warehouse.deleteMany()
    await prisma.$disconnect()
  })

  test('倉庫を登録できる', async () => {
    await prisma.warehouse.create({ data: warehouses[0] })
    const result = await prisma.warehouse.findMany()
    expect(result).toEqual(warehouses)
  })

  test('倉庫を更新できる', async () => {
    const expected = { ...warehouses[0], name: '更新倉庫' }
    await prisma.warehouse.update({
      where: { whCode: warehouses[0].whCode },
      data: { name: '更新倉庫' }
    })
    const result = await prisma.warehouse.findUnique({
      where: { whCode: warehouses[0].whCode }
    })
    expect(result).toEqual(expected)
  })

  test('倉庫を削除できる', async () => {
    await prisma.warehouse.delete({ where: { whCode: warehouses[0].whCode } })
    const result = await prisma.warehouse.findMany()
    expect(result).toEqual([])
  })
})

describe('発注マスタ', () => {
  beforeAll(async () => {
    await prisma.purchaseOrderDetail.deleteMany()
    await prisma.purchaseOrder.deleteMany()
  })

  afterAll(async () => {
    await prisma.purchaseOrderDetail.deleteMany()
    await prisma.purchaseOrder.deleteMany()
    await prisma.$disconnect()
  })

  test('発注を登録できる', async () => {
    await prisma.purchaseOrder.create({ data: purchaseOrders[0] })
    const result = await prisma.purchaseOrder.findMany()
    expect(result).toEqual(purchaseOrders)
  })

  test('発注を更新できる', async () => {
    const expected = { ...purchaseOrders[0], poAmnt: 9000 }
    await prisma.purchaseOrder.update({
      where: { poNo: purchaseOrders[0].poNo },
      data: { poAmnt: 9000 }
    })
    const result = await prisma.purchaseOrder.findUnique({
      where: { poNo: purchaseOrders[0].poNo }
    })
    expect(result).toEqual(expected)
  })

  test('発注を削除できる', async () => {
    await prisma.purchaseOrder.delete({ where: { poNo: purchaseOrders[0].poNo } })
    const result = await prisma.purchaseOrder.findMany()
    expect(result).toEqual([])
  })
})

describe('発注明細', () => {
  beforeAll(async () => {
    await prisma.purchaseOrderDetail.deleteMany()
    await prisma.purchaseOrder.deleteMany()
    await prisma.purchaseOrder.create({ data: purchaseOrders[0] })
  })

  afterAll(async () => {
    await prisma.purchaseOrderDetail.deleteMany()
    await prisma.purchaseOrder.deleteMany()
    await prisma.$disconnect()
  })

  test('発注明細を登録できる', async () => {
    await prisma.purchaseOrderDetail.create({ data: purchaseOrderDetails[0] })
    const result = await prisma.purchaseOrderDetail.findMany()
    expect(result).toEqual(purchaseOrderDetails)
  })

  test('発注明細を更新できる', async () => {
    const expected = { ...purchaseOrderDetails[0], quantity: 20 }
    await prisma.purchaseOrderDetail.update({
      where: {
        poNo_poRowNo: {
          poNo: purchaseOrderDetails[0].poNo,
          poRowNo: purchaseOrderDetails[0].poRowNo
        }
      },
      data: { quantity: 20 }
    })
    const result = await prisma.purchaseOrderDetail.findUnique({
      where: {
        poNo_poRowNo: {
          poNo: purchaseOrderDetails[0].poNo,
          poRowNo: purchaseOrderDetails[0].poRowNo
        }
      }
    })
    expect(result).toEqual(expected)
  })

  test('発注明細を削除できる', async () => {
    await prisma.purchaseOrderDetail.delete({
      where: {
        poNo_poRowNo: {
          poNo: purchaseOrderDetails[0].poNo,
          poRowNo: purchaseOrderDetails[0].poRowNo
        }
      }
    })
    const result = await prisma.purchaseOrderDetail.findMany()
    expect(result).toEqual([])
  })
})

describe('仕入マスタ', () => {
  beforeAll(async () => {
    await prisma.purchaseDetail.deleteMany()
    await prisma.purchase.deleteMany()
  })

  afterAll(async () => {
    await prisma.purchaseDetail.deleteMany()
    await prisma.purchase.deleteMany()
    await prisma.$disconnect()
  })

  test('仕入を登録できる', async () => {
    await prisma.purchase.create({ data: purchases[0] })
    const result = await prisma.purchase.findMany()
    expect(result).toEqual(purchases)
  })

  test('仕入を更新できる', async () => {
    const expected = { ...purchases[0], puAmmount: 9000 }
    await prisma.purchase.update({
      where: { puNo: purchases[0].puNo },
      data: { puAmmount: 9000 }
    })
    const result = await prisma.purchase.findUnique({
      where: { puNo: purchases[0].puNo }
    })
    expect(result).toEqual(expected)
  })

  test('仕入を削除できる', async () => {
    await prisma.purchase.delete({ where: { puNo: purchases[0].puNo } })
    const result = await prisma.purchase.findMany()
    expect(result).toEqual([])
  })
})

describe('仕入明細', () => {
  beforeAll(async () => {
    await prisma.purchaseDetail.deleteMany()
    await prisma.purchase.deleteMany()
    await prisma.purchase.create({ data: purchases[0] })
  })

  afterAll(async () => {
    await prisma.purchaseDetail.deleteMany()
    await prisma.purchase.deleteMany()
    await prisma.$disconnect()
  })

  test('仕入明細を登録できる', async () => {
    await prisma.purchaseDetail.create({ data: purchaseDetails[0] })
    const result = await prisma.purchaseDetail.findMany()
    expect(result).toEqual(purchaseDetails)
  })

  test('仕入明細を更新できる', async () => {
    const expected = { ...purchaseDetails[0], quantity: 20 }
    await prisma.purchaseDetail.update({
      where: {
        puNo_puRowNo: {
          puNo: purchaseDetails[0].puNo,
          puRowNo: purchaseDetails[0].puRowNo
        }
      },
      data: { quantity: 20 }
    })
    const result = await prisma.purchaseDetail.findUnique({
      where: {
        puNo_puRowNo: {
          puNo: purchaseDetails[0].puNo,
          puRowNo: purchaseDetails[0].puRowNo
        }
      }
    })
    expect(result).toEqual(expected)
  })

  test('仕入明細を削除できる', async () => {
    await prisma.purchaseDetail.delete({
      where: {
        puNo_puRowNo: {
          puNo: purchaseDetails[0].puNo,
          puRowNo: purchaseDetails[0].puRowNo
        }
      }
    })
    const result = await prisma.purchaseDetail.findMany()
    expect(result).toEqual([])
  })
})

describe('在庫マスタ', () => {
  beforeAll(async () => {
    await prisma.stock.deleteMany()
    await prisma.warehouse.deleteMany()
    await prisma.warehouse.create({ data: warehouses[0] })
  })

  afterAll(async () => {
    await prisma.stock.deleteMany()
    await prisma.warehouse.deleteMany()
    await prisma.$disconnect()
  })

  test('在庫を登録できる', async () => {
    await prisma.stock.create({ data: stocks[0] })
    const result = await prisma.stock.findMany()
    expect(result).toEqual(stocks)
  })

  test('在庫を更新できる', async () => {
    const expected = { ...stocks[0], actual: 20, valid: 20 }
    await prisma.stock.update({
      where: {
        whCode_prodCode_rotNo_stockType_qualityType: {
          whCode: stocks[0].whCode,
          prodCode: stocks[0].prodCode,
          rotNo: stocks[0].rotNo,
          stockType: stocks[0].stockType,
          qualityType: stocks[0].qualityType
        }
      },
      data: { actual: 20, valid: 20 }
    })
    const result = await prisma.stock.findUnique({
      where: {
        whCode_prodCode_rotNo_stockType_qualityType: {
          whCode: stocks[0].whCode,
          prodCode: stocks[0].prodCode,
          rotNo: stocks[0].rotNo,
          stockType: stocks[0].stockType,
          qualityType: stocks[0].qualityType
        }
      }
    })
    expect(result).toEqual(expected)
  })

  test('在庫を削除できる', async () => {
    await prisma.stock.delete({
      where: {
        whCode_prodCode_rotNo_stockType_qualityType: {
          whCode: stocks[0].whCode,
          prodCode: stocks[0].prodCode,
          rotNo: stocks[0].rotNo,
          stockType: stocks[0].stockType,
          qualityType: stocks[0].qualityType
        }
      }
    })
    const result = await prisma.stock.findMany()
    expect(result).toEqual([])
  })
})

// テスト用の請求データ
const invoices: Invoice[] = [
  {
    invoiceNo: 'INV0001',
    invoicedDate: new Date('2021-02-01'),
    compCode: 'COMP001',
    custSubNo: 1,
    lastReceived: 50000,
    monthSales: 100000,
    monthReceived: 80000,
    monthInvoice: 120000,
    cmpTax: 12000,
    invoiceReceived: 100000,
    createDate: new Date('2021-02-01'),
    creator: 'admin',
    updateDate: new Date('2021-02-01'),
    updater: 'admin'
  }
]

// テスト用の請求明細データ
const invoiceDetails: InvoiceDetail[] = [
  {
    invoiceNo: 'INV0001',
    rowNo: 1,
    salesNo: 'SAL0001',
    salesRowNo: 1,
    prodCode: 'PROD001',
    quantity: 10,
    unitprice: 1000,
    amount: 10000,
    createDate: new Date('2021-02-01'),
    creator: 'admin',
    updateDate: new Date('2021-02-01'),
    updater: 'admin'
  }
]

// テスト用の入金データ
const credits: Credit[] = [
  {
    creditNo: 'CRE0001',
    creditDate: new Date('2021-02-05'),
    deptCode: 'DEPT01',
    startDate: new Date('2021-02-01'),
    custCode: 'COMP001',
    custSubNo: 1,
    payMethodType: 1,
    bankAcutCode: 'BANK001',
    receivedAmnt: 100000,
    received: 100000,
    createDate: new Date('2021-02-05'),
    creator: 'admin',
    updateDate: new Date('2021-02-05'),
    updater: 'admin',
    updatePlgDate: new Date('2021-02-05'),
    updatePgm: 'credit_input'
  }
]

// テスト用の支払データ
const payments: Payment[] = [
  {
    payNo: 'PAY0001',
    payDate: 20210210,
    deptCode: 'DEPT01',
    startDate: new Date('2021-02-01'),
    supCode: 'COMP001',
    supSubNo: 1,
    payMethodType: 1,
    payAmnt: 80000,
    cmpTax: 8000,
    completeFlg: 1,
    createDate: new Date('2021-02-10'),
    creator: 'admin',
    updateDate: new Date('2021-02-10'),
    updater: 'admin'
  }
]

describe('請求マスタ', () => {
  beforeAll(async () => {
    await prisma.invoiceDetail.deleteMany()
    await prisma.invoice.deleteMany()
  })

  afterAll(async () => {
    await prisma.invoiceDetail.deleteMany()
    await prisma.invoice.deleteMany()
    await prisma.$disconnect()
  })

  test('請求を登録できる', async () => {
    await prisma.invoice.create({ data: invoices[0] })
    const result = await prisma.invoice.findMany()
    expect(result).toEqual(invoices)
  })

  test('請求を更新できる', async () => {
    const expected = { ...invoices[0], monthInvoice: 150000 }
    await prisma.invoice.update({
      where: { invoiceNo: invoices[0].invoiceNo },
      data: { monthInvoice: 150000 }
    })
    const result = await prisma.invoice.findUnique({
      where: { invoiceNo: invoices[0].invoiceNo }
    })
    expect(result).toEqual(expected)
  })

  test('請求を削除できる', async () => {
    await prisma.invoice.delete({ where: { invoiceNo: invoices[0].invoiceNo } })
    const result = await prisma.invoice.findMany()
    expect(result).toEqual([])
  })
})

describe('請求明細', () => {
  beforeAll(async () => {
    await prisma.invoiceDetail.deleteMany()
    await prisma.invoice.deleteMany()
    await prisma.invoice.create({ data: invoices[0] })
  })

  afterAll(async () => {
    await prisma.invoiceDetail.deleteMany()
    await prisma.invoice.deleteMany()
    await prisma.$disconnect()
  })

  test('請求明細を登録できる', async () => {
    await prisma.invoiceDetail.create({ data: invoiceDetails[0] })
    const result = await prisma.invoiceDetail.findMany()
    expect(result).toEqual(invoiceDetails)
  })

  test('請求明細を更新できる', async () => {
    const expected = { ...invoiceDetails[0], quantity: 20 }
    await prisma.invoiceDetail.update({
      where: {
        invoiceNo_rowNo: {
          invoiceNo: invoiceDetails[0].invoiceNo,
          rowNo: invoiceDetails[0].rowNo
        }
      },
      data: { quantity: 20 }
    })
    const result = await prisma.invoiceDetail.findUnique({
      where: {
        invoiceNo_rowNo: {
          invoiceNo: invoiceDetails[0].invoiceNo,
          rowNo: invoiceDetails[0].rowNo
        }
      }
    })
    expect(result).toEqual(expected)
  })

  test('請求明細を削除できる', async () => {
    await prisma.invoiceDetail.delete({
      where: {
        invoiceNo_rowNo: {
          invoiceNo: invoiceDetails[0].invoiceNo,
          rowNo: invoiceDetails[0].rowNo
        }
      }
    })
    const result = await prisma.invoiceDetail.findMany()
    expect(result).toEqual([])
  })
})

describe('入金マスタ', () => {
  beforeAll(async () => {
    await prisma.credit.deleteMany()
  })

  afterAll(async () => {
    await prisma.credit.deleteMany()
    await prisma.$disconnect()
  })

  test('入金を登録できる', async () => {
    await prisma.credit.create({ data: credits[0] })
    const result = await prisma.credit.findMany()
    expect(result).toEqual(credits)
  })

  test('入金を更新できる', async () => {
    const expected = { ...credits[0], receivedAmnt: 120000 }
    await prisma.credit.update({
      where: { creditNo: credits[0].creditNo },
      data: { receivedAmnt: 120000 }
    })
    const result = await prisma.credit.findUnique({
      where: { creditNo: credits[0].creditNo }
    })
    expect(result).toEqual(expected)
  })

  test('入金を削除できる', async () => {
    await prisma.credit.delete({ where: { creditNo: credits[0].creditNo } })
    const result = await prisma.credit.findMany()
    expect(result).toEqual([])
  })
})

describe('支払マスタ', () => {
  beforeAll(async () => {
    await prisma.payment.deleteMany()
  })

  afterAll(async () => {
    await prisma.payment.deleteMany()
    await prisma.$disconnect()
  })

  test('支払を登録できる', async () => {
    await prisma.payment.create({ data: payments[0] })
    const result = await prisma.payment.findMany()
    expect(result).toEqual(payments)
  })

  test('支払を更新できる', async () => {
    const expected = { ...payments[0], payAmnt: 90000 }
    await prisma.payment.update({
      where: { payNo: payments[0].payNo },
      data: { payAmnt: 90000 }
    })
    const result = await prisma.payment.findUnique({
      where: { payNo: payments[0].payNo }
    })
    expect(result).toEqual(expected)
  })

  test('支払を削除できる', async () => {
    await prisma.payment.delete({ where: { payNo: payments[0].payNo } })
    const result = await prisma.payment.findMany()
    expect(result).toEqual([])
  })
})
