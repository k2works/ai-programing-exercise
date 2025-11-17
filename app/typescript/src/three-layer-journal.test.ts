// src/three-layer-journal.test.ts
import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { TestDatabase } from './test-setup/database'

describe('3層構造の仕訳管理', () => {
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

  test('基本的な3層構造の仕訳を作成できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    // テスト用の勘定科目を作成
    await testDb.prisma.account.createMany({
      data: [
        {
          accountCode: '1100',
          accountName: '現金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        },
        {
          accountCode: '4100',
          accountName: '売上',
          accountType: '収益',
          sumAccount: false,
          aggregationTarget: true
        }
      ]
    })

    // 3層構造の仕訳を作成
    const journal = await testDb.prisma.journal.create({
      data: {
        voucherNo: 'J2024001',
        journalDate: new Date('2024-01-15'),
        inputDate: new Date('2024-01-15'),
        settlementFlag: 0,
        singleFlag: 1,
        voucherType: 1,
        recurringFlag: 0,
        redSlipFlag: 0,
        details: {
          create: [
            {
              lineNo: 1,
              lineSummary: '商品売上',
              items: {
                create: [
                  {
                    debitCredit: 'D',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '1100',
                    amount: 110000,
                    baseAmount: 110000
                  },
                  {
                    debitCredit: 'C',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '4100',
                    amount: 110000,
                    baseAmount: 110000
                  }
                ]
              }
            }
          ]
        }
      },
      include: {
        details: {
          include: {
            items: true
          }
        }
      }
    })

    // 作成されたデータの確認
    expect(journal.voucherNo).toBe('J2024001')
    expect(journal.details).toHaveLength(1)
    expect(journal.details[0].items).toHaveLength(2)

    // 借方・貸方の合計確認
    const debitTotal = journal.details
      .flatMap((d) => d.items)
      .filter((i) => i.debitCredit === 'D')
      .reduce((sum, i) => sum + Number(i.amount), 0)

    const creditTotal = journal.details
      .flatMap((d) => d.items)
      .filter((i) => i.debitCredit === 'C')
      .reduce((sum, i) => sum + Number(i.amount), 0)

    expect(debitTotal).toBe(creditTotal) // 複式簿記の原理
    expect(debitTotal).toBe(110000)
  })

  test('決算仕訳フラグを設定できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    await testDb.prisma.account.create({
      data: {
        accountCode: '9999',
        accountName: '損益',
        accountType: '純資産',
        sumAccount: false,
        aggregationTarget: true
      }
    })

    const journal = await testDb.prisma.journal.create({
      data: {
        voucherNo: 'J2024002',
        journalDate: new Date('2024-03-31'),
        inputDate: new Date('2024-03-31'),
        settlementFlag: 1, // 決算仕訳
        singleFlag: 1,
        voucherType: 1,
        recurringFlag: 0,
        redSlipFlag: 0,
        details: {
          create: [
            {
              lineNo: 1,
              lineSummary: '決算整理仕訳',
              items: {
                create: [
                  {
                    debitCredit: 'D',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '9999',
                    amount: 100000,
                    baseAmount: 100000
                  },
                  {
                    debitCredit: 'C',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '9999',
                    amount: 100000,
                    baseAmount: 100000
                  }
                ]
              }
            }
          ]
        }
      }
    })

    expect(journal.settlementFlag).toBe(1)
  })

  test('赤伝票フラグと赤黒伝票番号を設定できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    await testDb.prisma.account.create({
      data: {
        accountCode: '1100',
        accountName: '現金',
        accountType: '資産',
        sumAccount: false,
        aggregationTarget: true
      }
    })

    const journal = await testDb.prisma.journal.create({
      data: {
        voucherNo: 'J2024003',
        journalDate: new Date('2024-01-20'),
        inputDate: new Date('2024-01-20'),
        settlementFlag: 0,
        singleFlag: 1,
        voucherType: 1,
        recurringFlag: 0,
        redSlipFlag: 1, // 赤伝票
        redBlackVoucherNo: 2024001, // 元の伝票番号
        details: {
          create: [
            {
              lineNo: 1,
              lineSummary: '誤記訂正（赤伝）',
              items: {
                create: [
                  {
                    debitCredit: 'D',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '1100',
                    amount: -50000, // 赤伝票はマイナス金額
                    baseAmount: -50000
                  },
                  {
                    debitCredit: 'C',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '1100',
                    amount: -50000,
                    baseAmount: -50000
                  }
                ]
              }
            }
          ]
        }
      }
    })

    expect(journal.redSlipFlag).toBe(1)
    expect(journal.redBlackVoucherNo).toBe(2024001)
  })

  test('多通貨対応の仕訳を作成できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    await testDb.prisma.account.createMany({
      data: [
        {
          accountCode: '1200',
          accountName: '普通預金（USD）',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        },
        {
          accountCode: '4100',
          accountName: '売上',
          accountType: '収益',
          sumAccount: false,
          aggregationTarget: true
        }
      ]
    })

    const exchangeRate = 150.5 // 1USD = 150.5円

    const journal = await testDb.prisma.journal.create({
      data: {
        voucherNo: 'J2024004',
        journalDate: new Date('2024-01-15'),
        inputDate: new Date('2024-01-15'),
        settlementFlag: 0,
        singleFlag: 1,
        voucherType: 1,
        recurringFlag: 0,
        redSlipFlag: 0,
        details: {
          create: [
            {
              lineNo: 1,
              lineSummary: 'USD売上',
              items: {
                create: [
                  {
                    debitCredit: 'D',
                    currencyCode: 'USD',
                    exchangeRate: exchangeRate,
                    accountCode: '1200',
                    amount: 1000, // 1000 USD
                    baseAmount: 150500 // 150,500円
                  },
                  {
                    debitCredit: 'C',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '4100',
                    amount: 150500,
                    baseAmount: 150500
                  }
                ]
              }
            }
          ]
        }
      },
      include: {
        details: {
          include: {
            items: true
          }
        }
      }
    })

    const usdItem = journal.details[0].items.find((i) => i.currencyCode === 'USD')
    expect(usdItem?.currencyCode).toBe('USD')
    expect(Number(usdItem?.exchangeRate)).toBe(exchangeRate)
    expect(Number(usdItem?.amount)).toBe(1000)
    expect(Number(usdItem?.baseAmount)).toBe(150500)
  })

  test('多次元管理（部門・プロジェクト）の仕訳を作成できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    await testDb.prisma.account.createMany({
      data: [
        {
          accountCode: '6100',
          accountName: '旅費交通費',
          accountType: '費用',
          sumAccount: false,
          aggregationTarget: true
        },
        {
          accountCode: '1100',
          accountName: '現金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        }
      ]
    })

    const journal = await testDb.prisma.journal.create({
      data: {
        voucherNo: 'J2024005',
        journalDate: new Date('2024-01-20'),
        inputDate: new Date('2024-01-20'),
        settlementFlag: 0,
        singleFlag: 1,
        voucherType: 1,
        recurringFlag: 0,
        redSlipFlag: 0,
        departmentCode: '101', // 営業部
        details: {
          create: [
            {
              lineNo: 1,
              lineSummary: 'プロジェクトA 出張費',
              items: {
                create: [
                  {
                    debitCredit: 'D',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '6100',
                    departmentCode: '101',
                    projectCode: 'PJ-A001',
                    segmentCode: 'SEG001',
                    amount: 30000,
                    baseAmount: 30000
                  },
                  {
                    debitCredit: 'C',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '1100',
                    amount: 30000,
                    baseAmount: 30000
                  }
                ]
              }
            }
          ]
        }
      },
      include: {
        details: {
          include: {
            items: true
          }
        }
      }
    })

    expect(journal.departmentCode).toBe('101')

    const debitItem = journal.details[0].items.find((i) => i.debitCredit === 'D')
    expect(debitItem?.departmentCode).toBe('101')
    expect(debitItem?.projectCode).toBe('PJ-A001')
    expect(debitItem?.segmentCode).toBe('SEG001')
  })

  test('複合仕訳（複数行）を作成できる', async () => {
    if (!testDb.prisma) {
      throw new Error('Prisma client not initialized')
    }

    await testDb.prisma.account.createMany({
      data: [
        {
          accountCode: '1100',
          accountName: '現金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        },
        {
          accountCode: '1200',
          accountName: '普通預金',
          accountType: '資産',
          sumAccount: false,
          aggregationTarget: true
        },
        {
          accountCode: '4100',
          accountName: '売上',
          accountType: '収益',
          sumAccount: false,
          aggregationTarget: true
        }
      ]
    })

    const journal = await testDb.prisma.journal.create({
      data: {
        voucherNo: 'J2024006',
        journalDate: new Date('2024-01-25'),
        inputDate: new Date('2024-01-25'),
        settlementFlag: 0,
        singleFlag: 0, // 複合仕訳
        voucherType: 1,
        recurringFlag: 0,
        redSlipFlag: 0,
        details: {
          create: [
            {
              lineNo: 1,
              lineSummary: '現金売上',
              items: {
                create: [
                  {
                    debitCredit: 'D',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '1100',
                    amount: 50000,
                    baseAmount: 50000
                  },
                  {
                    debitCredit: 'C',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '4100',
                    amount: 50000,
                    baseAmount: 50000
                  }
                ]
              }
            },
            {
              lineNo: 2,
              lineSummary: '預金売上',
              items: {
                create: [
                  {
                    debitCredit: 'D',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '1200',
                    amount: 75000,
                    baseAmount: 75000
                  },
                  {
                    debitCredit: 'C',
                    currencyCode: 'JPY',
                    exchangeRate: 1.0,
                    accountCode: '4100',
                    amount: 75000,
                    baseAmount: 75000
                  }
                ]
              }
            }
          ]
        }
      },
      include: {
        details: {
          include: {
            items: true
          }
        }
      }
    })

    expect(journal.singleFlag).toBe(0) // 複合仕訳
    expect(journal.details).toHaveLength(2)

    // 全体の貸借平衡確認
    const allItems = journal.details.flatMap((d) => d.items)
    const debitTotal = allItems
      .filter((i) => i.debitCredit === 'D')
      .reduce((sum, i) => sum + Number(i.amount), 0)
    const creditTotal = allItems
      .filter((i) => i.debitCredit === 'C')
      .reduce((sum, i) => sum + Number(i.amount), 0)

    expect(debitTotal).toBe(creditTotal)
    expect(debitTotal).toBe(125000)
  })
})
