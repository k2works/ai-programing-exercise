// src/seed.ts
/* eslint-disable no-console */
import { getPrismaClient } from './infrastructure/persistence/prisma-client'
import { getEventPublisher, closeEventPublisher } from './infrastructure/messaging/EventPublisher'

const prisma = getPrismaClient()
const eventPublisher = getEventPublisher()

async function cleanupDatabase() {
  await prisma.journalDetailItem.deleteMany()
  await prisma.journal.deleteMany()
  await prisma.account.deleteMany()
  console.log('✅ Cleaned up existing data')
}

async function seedAccounts() {
  // 勘定科目マスタの投入
  const accounts = [
    // 資産の部
    { accountCode: '1', accountName: '資産', accountType: '資産' },
    { accountCode: '11', accountName: '流動資産', accountType: '資産' },
    { accountCode: '111', accountName: '現金預金', accountType: '資産' },
    { accountCode: '112', accountName: '売掛金', accountType: '資産' },
    { accountCode: '113', accountName: '売上債権', accountType: '資産' },
    { accountCode: '114', accountName: '棚卸資産', accountType: '資産' },
    { accountCode: '115', accountName: 'その他流動資産', accountType: '資産' },
    { accountCode: '12', accountName: '固定資産', accountType: '資産' },
    { accountCode: '121', accountName: '有形固定資産', accountType: '資産' },
    { accountCode: '1211', accountName: '建物及び構築物', accountType: '資産' },
    { accountCode: '1212', accountName: '機械装置及び運搬具', accountType: '資産' },
    { accountCode: '1213', accountName: '工具器具備品', accountType: '資産' },
    { accountCode: '1214', accountName: '土地', accountType: '資産' },
    { accountCode: '122', accountName: '無形固定資産', accountType: '資産' },
    { accountCode: '123', accountName: '投資その他の資産', accountType: '資産' },

    // 負債の部
    { accountCode: '2', accountName: '負債', accountType: '負債' },
    { accountCode: '21', accountName: '流動負債', accountType: '負債' },
    { accountCode: '211', accountName: '買掛金', accountType: '負債' },
    { accountCode: '212', accountName: '短期借入金', accountType: '負債' },
    { accountCode: '213', accountName: '未払金', accountType: '負債' },
    { accountCode: '214', accountName: '未払法人税等', accountType: '負債' },
    { accountCode: '22', accountName: '固定負債', accountType: '負債' },
    { accountCode: '221', accountName: '長期借入金', accountType: '負債' },
    { accountCode: '222', accountName: 'リース債務', accountType: '負債' },

    // 純資産の部
    { accountCode: '3', accountName: '純資産', accountType: '純資産' },
    { accountCode: '31', accountName: '資本金', accountType: '純資産' },
    { accountCode: '32', accountName: '資本剰余金', accountType: '純資産' },
    { accountCode: '33', accountName: '利益剰余金', accountType: '純資産' },

    // 収益の部
    { accountCode: '4', accountName: '収益', accountType: '収益' },
    { accountCode: '41', accountName: '売上高', accountType: '収益' },
    { accountCode: '42', accountName: '営業外収益', accountType: '収益' },
    { accountCode: '43', accountName: '特別利益', accountType: '収益' },

    // 費用の部
    { accountCode: '5', accountName: '費用', accountType: '費用' },
    { accountCode: '51', accountName: '売上原価', accountType: '費用' },
    { accountCode: '52', accountName: '販売費及び一般管理費', accountType: '費用' },
    { accountCode: '53', accountName: '営業外費用', accountType: '費用' },
    { accountCode: '54', accountName: '特別損失', accountType: '費用' },
    { accountCode: '55', accountName: '法人税等', accountType: '費用' },
    { accountCode: '56', accountName: '当期純利益', accountType: '費用' }
  ]

  for (const account of accounts) {
    await prisma.account.create({ data: account })
  }

  console.log(`✅ Created ${accounts.length} accounts`)
}

async function seedFY2024Journals() {
  // 2024年度サンプル仕訳の投入
  console.log('Creating FY2024 journals...')

  // 1月: 期首仕訳（前期繰越）
  const journal1 = await prisma.journal.create({
    data: {
      fiscalYear: 2024,
      journalDate: new Date('2024-01-01'),
      description: '期首残高仕訳'
    }
  })

  await prisma.journalDetailItem.createMany({
    data: [
      {
        journalId: journal1.id,
        accountCode: '111',
        debitAmount: 5000000,
        creditAmount: 0,
        description: '現金預金（期首残高）'
      },
      {
        journalId: journal1.id,
        accountCode: '112',
        debitAmount: 2000000,
        creditAmount: 0,
        description: '売掛金（期首残高）'
      },
      {
        journalId: journal1.id,
        accountCode: '211',
        debitAmount: 0,
        creditAmount: 1500000,
        description: '買掛金（期首残高）'
      },
      {
        journalId: journal1.id,
        accountCode: '31',
        debitAmount: 0,
        creditAmount: 3000000,
        description: '資本金（期首残高）'
      },
      {
        journalId: journal1.id,
        accountCode: '33',
        debitAmount: 0,
        creditAmount: 2500000,
        description: '利益剰余金（期首残高）'
      }
    ]
  })

  // JournalCreated イベントを発行
  await eventPublisher.publish('journal.created', {
    eventType: 'JournalCreated',
    occurredAt: new Date(),
    payload: {
      journalId: journal1.id.toString(),
      fiscalYear: journal1.fiscalYear,
      journalDate: journal1.journalDate.toISOString(),
      totalDebitAmount: 7000000,
      totalCreditAmount: 7000000
    }
  })

  // 2月: 売上と仕入
  const journal2 = await prisma.journal.create({
    data: {
      fiscalYear: 2024,
      journalDate: new Date('2024-02-15'),
      description: '商品販売'
    }
  })

  await prisma.journalDetailItem.createMany({
    data: [
      {
        journalId: journal2.id,
        accountCode: '112',
        debitAmount: 1000000,
        creditAmount: 0,
        description: '売掛金'
      },
      {
        journalId: journal2.id,
        accountCode: '41',
        debitAmount: 0,
        creditAmount: 1000000,
        description: '売上高'
      }
    ]
  })

  await eventPublisher.publish('journal.created', {
    eventType: 'JournalCreated',
    occurredAt: new Date(),
    payload: {
      journalId: journal2.id.toString(),
      fiscalYear: journal2.fiscalYear,
      journalDate: journal2.journalDate.toISOString(),
      totalDebitAmount: 1000000,
      totalCreditAmount: 1000000
    }
  })

  const journal3 = await prisma.journal.create({
    data: {
      fiscalYear: 2024,
      journalDate: new Date('2024-02-20'),
      description: '商品仕入'
    }
  })

  await prisma.journalDetailItem.createMany({
    data: [
      {
        journalId: journal3.id,
        accountCode: '51',
        debitAmount: 600000,
        creditAmount: 0,
        description: '売上原価'
      },
      {
        journalId: journal3.id,
        accountCode: '211',
        debitAmount: 0,
        creditAmount: 600000,
        description: '買掛金'
      }
    ]
  })

  await eventPublisher.publish('journal.created', {
    eventType: 'JournalCreated',
    occurredAt: new Date(),
    payload: {
      journalId: journal3.id.toString(),
      fiscalYear: journal3.fiscalYear,
      journalDate: journal3.journalDate.toISOString(),
      totalDebitAmount: 600000,
      totalCreditAmount: 600000
    }
  })

  // 3月: 経費支払い
  const journal4 = await prisma.journal.create({
    data: {
      fiscalYear: 2024,
      journalDate: new Date('2024-03-10'),
      description: '給与支払い'
    }
  })

  await prisma.journalDetailItem.createMany({
    data: [
      {
        journalId: journal4.id,
        accountCode: '52',
        debitAmount: 500000,
        creditAmount: 0,
        description: '販売費及び一般管理費（給与）'
      },
      {
        journalId: journal4.id,
        accountCode: '111',
        debitAmount: 0,
        creditAmount: 500000,
        description: '現金預金'
      }
    ]
  })

  await eventPublisher.publish('journal.created', {
    eventType: 'JournalCreated',
    occurredAt: new Date(),
    payload: {
      journalId: journal4.id.toString(),
      fiscalYear: journal4.fiscalYear,
      journalDate: journal4.journalDate.toISOString(),
      totalDebitAmount: 500000,
      totalCreditAmount: 500000
    }
  })

  // 4月: 入金と支払い
  const journal5 = await prisma.journal.create({
    data: {
      fiscalYear: 2024,
      journalDate: new Date('2024-04-05'),
      description: '売掛金回収'
    }
  })

  await prisma.journalDetailItem.createMany({
    data: [
      {
        journalId: journal5.id,
        accountCode: '111',
        debitAmount: 1500000,
        creditAmount: 0,
        description: '現金預金'
      },
      {
        journalId: journal5.id,
        accountCode: '112',
        debitAmount: 0,
        creditAmount: 1500000,
        description: '売掛金'
      }
    ]
  })

  await eventPublisher.publish('journal.created', {
    eventType: 'JournalCreated',
    occurredAt: new Date(),
    payload: {
      journalId: journal5.id.toString(),
      fiscalYear: journal5.fiscalYear,
      journalDate: journal5.journalDate.toISOString(),
      totalDebitAmount: 1500000,
      totalCreditAmount: 1500000
    }
  })

  const journal6 = await prisma.journal.create({
    data: {
      fiscalYear: 2024,
      journalDate: new Date('2024-04-15'),
      description: '買掛金支払い'
    }
  })

  await prisma.journalDetailItem.createMany({
    data: [
      {
        journalId: journal6.id,
        accountCode: '211',
        debitAmount: 1000000,
        creditAmount: 0,
        description: '買掛金'
      },
      {
        journalId: journal6.id,
        accountCode: '111',
        debitAmount: 0,
        creditAmount: 1000000,
        description: '現金預金'
      }
    ]
  })

  await eventPublisher.publish('journal.created', {
    eventType: 'JournalCreated',
    occurredAt: new Date(),
    payload: {
      journalId: journal6.id.toString(),
      fiscalYear: journal6.fiscalYear,
      journalDate: journal6.journalDate.toISOString(),
      totalDebitAmount: 1000000,
      totalCreditAmount: 1000000
    }
  })

  console.log('✅ Created 6 journals for FY2024')
}

async function main() {
  console.log('🌱 Seeding financial accounting database...')

  // EventPublisher に接続
  await eventPublisher.connect()

  await cleanupDatabase()
  await seedAccounts()
  await seedFY2024Journals()

  console.log('🎉 Seeding completed!')
}

main()
  .catch((e) => {
    console.error('❌ Error seeding database:', e)
    process.exit(1)
  })
  .finally(async () => {
    await closeEventPublisher()
    await prisma.$disconnect()
  })
