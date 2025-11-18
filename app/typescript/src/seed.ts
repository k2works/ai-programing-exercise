// src/seed.ts
/* eslint-disable no-console */
import { PrismaClient } from '@prisma/client'

const prisma = new PrismaClient()

async function main() {
  console.log('🌱 Seeding database...')

  // 既存データのクリーンアップ
  await prisma.journalDetailItem.deleteMany()
  await prisma.journalDetail.deleteMany()
  await prisma.journal.deleteMany()
  await prisma.accountStructure.deleteMany()
  await prisma.account.deleteMany()

  console.log('✅ Cleaned up existing data')

  // 勘定科目マスタの投入
  const accounts = [
    // 資産の部
    { accountCode: '1', accountName: '資産', accountType: '資産' },
    { accountCode: '11', accountName: '流動資産', accountType: '資産', sumAccount: true },
    { accountCode: '111', accountName: '現金預金', accountType: '資産' },
    { accountCode: '112', accountName: '売掛金', accountType: '資産' },
    { accountCode: '113', accountName: '売上債権', accountType: '資産' },
    { accountCode: '114', accountName: '棚卸資産', accountType: '資産' },
    { accountCode: '115', accountName: 'その他流動資産', accountType: '資産' },
    { accountCode: '12', accountName: '固定資産', accountType: '資産', sumAccount: true },
    { accountCode: '121', accountName: '有形固定資産', accountType: '資産', sumAccount: true },
    { accountCode: '1211', accountName: '建物及び構築物', accountType: '資産' },
    { accountCode: '1212', accountName: '機械装置及び運搬具', accountType: '資産' },
    { accountCode: '1213', accountName: '工具器具備品', accountType: '資産' },
    { accountCode: '1214', accountName: '土地', accountType: '資産' },
    { accountCode: '1215', accountName: 'その他有形固定資産', accountType: '資産' },
    { accountCode: '122', accountName: '無形固定資産', accountType: '資産' },
    { accountCode: '123', accountName: '投資その他の資産', accountType: '資産' },

    // 負債の部
    { accountCode: '2', accountName: '負債', accountType: '負債' },
    { accountCode: '21', accountName: '流動負債', accountType: '負債', sumAccount: true },
    { accountCode: '211', accountName: '買掛金', accountType: '負債' },
    { accountCode: '212', accountName: '短期借入金', accountType: '負債' },
    { accountCode: '213', accountName: '未払金', accountType: '負債' },
    { accountCode: '214', accountName: '未払法人税等', accountType: '負債' },
    { accountCode: '215', accountName: 'その他流動負債', accountType: '負債' },
    { accountCode: '22', accountName: '固定負債', accountType: '負債', sumAccount: true },
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

  // 勘定科目構成（階層構造）の投入
  const structures = [
    { accountCode: '1', accountPath: '1' },
    { accountCode: '11', accountPath: '1~11' },
    { accountCode: '111', accountPath: '1~11~111' },
    { accountCode: '112', accountPath: '1~11~112' },
    { accountCode: '113', accountPath: '1~11~113' },
    { accountCode: '114', accountPath: '1~11~114' },
    { accountCode: '115', accountPath: '1~11~115' },
    { accountCode: '12', accountPath: '1~12' },
    { accountCode: '121', accountPath: '1~12~121' },
    { accountCode: '1211', accountPath: '1~12~121~1211' },
    { accountCode: '1212', accountPath: '1~12~121~1212' },
    { accountCode: '1213', accountPath: '1~12~121~1213' },
    { accountCode: '1214', accountPath: '1~12~121~1214' },
    { accountCode: '1215', accountPath: '1~12~121~1215' },
    { accountCode: '122', accountPath: '1~12~122' },
    { accountCode: '123', accountPath: '1~12~123' },
    { accountCode: '2', accountPath: '2' },
    { accountCode: '21', accountPath: '2~21' },
    { accountCode: '211', accountPath: '2~21~211' },
    { accountCode: '212', accountPath: '2~21~212' },
    { accountCode: '213', accountPath: '2~21~213' },
    { accountCode: '214', accountPath: '2~21~214' },
    { accountCode: '215', accountPath: '2~21~215' },
    { accountCode: '22', accountPath: '2~22' },
    { accountCode: '221', accountPath: '2~22~221' },
    { accountCode: '222', accountPath: '2~22~222' },
    { accountCode: '3', accountPath: '3' },
    { accountCode: '31', accountPath: '3~31' },
    { accountCode: '32', accountPath: '3~32' },
    { accountCode: '33', accountPath: '3~33' },
    { accountCode: '4', accountPath: '4' },
    { accountCode: '41', accountPath: '4~41' },
    { accountCode: '42', accountPath: '4~42' },
    { accountCode: '43', accountPath: '4~43' },
    { accountCode: '5', accountPath: '5' },
    { accountCode: '51', accountPath: '5~51' },
    { accountCode: '52', accountPath: '5~52' },
    { accountCode: '53', accountPath: '5~53' },
    { accountCode: '54', accountPath: '5~54' },
    { accountCode: '55', accountPath: '5~55' },
    { accountCode: '56', accountPath: '5~56' }
  ]

  for (const structure of structures) {
    await prisma.accountStructure.create({ data: structure })
  }

  console.log(`✅ Created ${structures.length} account structures`)

  console.log('🎉 Seeding completed!')
}

main()
  .catch((e) => {
    console.error('❌ Error seeding database:', e)
    process.exit(1)
  })
  .finally(async () => {
    await prisma.$disconnect()
  })
