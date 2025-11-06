/* eslint-disable no-console */
import { PrismaClient } from '@prisma/client'

const prisma = new PrismaClient()

async function main() {
  console.log('🌱 Seeding database...')

  // 既存データを削除
  console.log('🗑️  Cleaning existing data...')
  await prisma.stock.deleteMany()
  await prisma.warehouse.deleteMany()
  await prisma.product.deleteMany()
  await prisma.creditBalance.deleteMany()
  await prisma.autoNumber.deleteMany()
  await prisma.payment.deleteMany()
  await prisma.credit.deleteMany()
  await prisma.invoiceDetail.deleteMany()
  await prisma.invoice.deleteMany()
  await prisma.purchaseDetail.deleteMany()
  await prisma.purchase.deleteMany()
  await prisma.purchaseOrderDetail.deleteMany()
  await prisma.purchaseOrder.deleteMany()
  await prisma.orderDetail.deleteMany()
  await prisma.order.deleteMany()
  await prisma.company.deleteMany()
  await prisma.companyGroup.deleteMany()
  await prisma.employee.deleteMany()
  await prisma.department.deleteMany()

  // 部門マスタ（21件）
  console.log('📁 Creating departments...')
  const baseDate = new Date('2021-01-01')

  const departments = [
    // 本社（レベル1）
    { deptCode: '000000', name: '本社', psth: '/000000', startDate: baseDate, layer: 1, lowestType: 0, slitYn: 0 },
    // 食肉製造・販売事業（レベル2）
    { deptCode: '100000', name: '食肉製造・販売事業', psth: '/000000/100000', startDate: baseDate, layer: 2, lowestType: 0, slitYn: 0 },
    // 食肉加工部門（レベル3）
    { deptCode: '110000', name: '食肉加工部門', psth: '/000000/100000/110000', startDate: baseDate, layer: 3, lowestType: 0, slitYn: 0 },
    // 食肉加工部門の課（レベル4）
    { deptCode: '111000', name: '牛肉・豚肉・鶏肉課', psth: '/000000/100000/110000/111000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 },
    { deptCode: '112000', name: '食肉加工品課', psth: '/000000/100000/110000/112000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 },
    // 小売販売部門（レベル3）
    { deptCode: '120000', name: '小売販売部門', psth: '/000000/100000/120000', startDate: baseDate, layer: 3, lowestType: 0, slitYn: 0 },
    // 小売販売部門の課（レベル4）
    { deptCode: '121000', name: '直営小売店課', psth: '/000000/100000/120000/121000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 },
    { deptCode: '122000', name: '百貨店・スーパー向け販売課', psth: '/000000/100000/120000/122000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 },
    // 新規取引先開拓部門（レベル3）
    { deptCode: '130000', name: '新規取引先開拓部門', psth: '/000000/100000/130000', startDate: baseDate, layer: 3, lowestType: 0, slitYn: 0 },
    // 新規取引先開拓部門の課（レベル4）
    { deptCode: '131000', name: 'ホテル・旅館向け課', psth: '/000000/100000/130000/131000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 },
    { deptCode: '132000', name: '飲食店向け課', psth: '/000000/100000/130000/132000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 },
    // 食肉加工品事業（レベル2）
    { deptCode: '200000', name: '食肉加工品事業', psth: '/000000/200000', startDate: baseDate, layer: 2, lowestType: 0, slitYn: 0 },
    // 自社ブランド部門（レベル3）
    { deptCode: '210000', name: '自社ブランド部門', psth: '/000000/200000/210000', startDate: baseDate, layer: 3, lowestType: 0, slitYn: 0 },
    // 自社ブランド部門の課（レベル4）
    { deptCode: '211000', name: '贈答用製品製造課', psth: '/000000/200000/210000/211000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 },
    { deptCode: '212000', name: '道の駅・土産物製品販売課', psth: '/000000/200000/210000/212000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 },
    // 相手先ブランド製造(OEM)部門（レベル3）
    { deptCode: '220000', name: '相手先ブランド製造(OEM)部門', psth: '/000000/200000/220000', startDate: baseDate, layer: 3, lowestType: 0, slitYn: 0 },
    // OEM部門の課（レベル4）
    { deptCode: '221000', name: '客先要望対応課', psth: '/000000/200000/220000/221000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 },
    // コンサルティング事業（レベル2）
    { deptCode: '300000', name: 'コンサルティング事業', psth: '/000000/300000', startDate: baseDate, layer: 2, lowestType: 0, slitYn: 0 },
    // 顧客対応部門（レベル3）
    { deptCode: '310000', name: '顧客対応部門', psth: '/000000/300000/310000', startDate: baseDate, layer: 3, lowestType: 0, slitYn: 0 },
    // 顧客対応部門の課（レベル4）
    { deptCode: '311000', name: 'メニュー提案課', psth: '/000000/300000/310000/311000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 },
    { deptCode: '312000', name: '半加工商品提供課', psth: '/000000/300000/310000/312000', startDate: baseDate, layer: 4, lowestType: 1, slitYn: 1 }
  ]

  for (const dept of departments) {
    await prisma.department.create({
      data: {
        ...dept,
        createDate: new Date(),
        creator: 'seed',
        updateDate: new Date(),
        updater: 'seed'
      }
    })
  }
  console.log(`✅ Created ${departments.length} departments`)

  // 社員マスタ（47件：正社員24名、パート21名）
  console.log('👥 Creating employees...')
  const employees = [
    // 経営層（本社）：2名
    { empCode: 'EMP0001', name: '山田太郎', occuCode: '01', approvalCode: '01', deptCode: '000000', startDate: baseDate }, // 社長
    { empCode: 'EMP0002', name: '佐藤花子', occuCode: '02', approvalCode: '02', deptCode: '000000', startDate: baseDate }, // 専務

    // 食肉製造・販売事業（15名：正社員8名、パート7名）
    // 食肉加工部門
    { empCode: 'EMP0003', name: '鈴木一郎', occuCode: '03', approvalCode: '03', deptCode: '110000', startDate: baseDate }, // 部門長
    { empCode: 'EMP0004', name: '高橋次郎', occuCode: '04', approvalCode: '04', deptCode: '111000', startDate: baseDate }, // 課長
    { empCode: 'EMP0005', name: '田中三郎', occuCode: '05', approvalCode: '05', deptCode: '111000', startDate: baseDate }, // 一般社員
    { empCode: 'EMP0006', name: '伊藤四郎', occuCode: '05', approvalCode: '05', deptCode: '111000', startDate: baseDate },
    { empCode: 'EMP0007', name: '渡辺五郎', occuCode: '06', approvalCode: '06', deptCode: '111000', startDate: baseDate }, // パート
    { empCode: 'EMP0008', name: '山本六郎', occuCode: '06', approvalCode: '06', deptCode: '111000', startDate: baseDate },
    { empCode: 'EMP0009', name: '中村七郎', occuCode: '04', approvalCode: '04', deptCode: '112000', startDate: baseDate }, // 課長
    { empCode: 'EMP0010', name: '小林八郎', occuCode: '05', approvalCode: '05', deptCode: '112000', startDate: baseDate },
    { empCode: 'EMP0011', name: '加藤九郎', occuCode: '06', approvalCode: '06', deptCode: '112000', startDate: baseDate }, // パート
    { empCode: 'EMP0012', name: '吉田十郎', occuCode: '06', approvalCode: '06', deptCode: '112000', startDate: baseDate },

    // 小売販売部門
    { empCode: 'EMP0013', name: '森田和子', occuCode: '03', approvalCode: '03', deptCode: '120000', startDate: baseDate }, // 部門長
    { empCode: 'EMP0014', name: '林美咲', occuCode: '04', approvalCode: '04', deptCode: '121000', startDate: baseDate }, // 課長
    { empCode: 'EMP0015', name: '木村愛', occuCode: '06', approvalCode: '06', deptCode: '121000', startDate: baseDate }, // パート
    { empCode: 'EMP0016', name: '斎藤恵', occuCode: '06', approvalCode: '06', deptCode: '121000', startDate: baseDate },
    { empCode: 'EMP0017', name: '松本優', occuCode: '04', approvalCode: '04', deptCode: '122000', startDate: baseDate }, // 課長

    // 新規取引先開拓部門
    { empCode: 'EMP0018', name: '井上健', occuCode: '03', approvalCode: '03', deptCode: '130000', startDate: baseDate }, // 部門長
    { empCode: 'EMP0019', name: '木下誠', occuCode: '05', approvalCode: '05', deptCode: '131000', startDate: baseDate },
    { empCode: 'EMP0020', name: '山口勇', occuCode: '05', approvalCode: '05', deptCode: '132000', startDate: baseDate },
    { empCode: 'EMP0021', name: '清水智', occuCode: '06', approvalCode: '06', deptCode: '132000', startDate: baseDate }, // パート

    // 食肉加工品事業（14名：正社員6名、パート8名）
    // 自社ブランド部門
    { empCode: 'EMP0022', name: '阿部明', occuCode: '03', approvalCode: '03', deptCode: '210000', startDate: baseDate }, // 部門長
    { empCode: 'EMP0023', name: '前田光', occuCode: '04', approvalCode: '04', deptCode: '211000', startDate: baseDate }, // 課長
    { empCode: 'EMP0024', name: '岡田優子', occuCode: '05', approvalCode: '05', deptCode: '211000', startDate: baseDate },
    { empCode: 'EMP0025', name: '長谷川葵', occuCode: '06', approvalCode: '06', deptCode: '211000', startDate: baseDate }, // パート
    { empCode: 'EMP0026', name: '西村さくら', occuCode: '06', approvalCode: '06', deptCode: '211000', startDate: baseDate },
    { empCode: 'EMP0027', name: '石川桜', occuCode: '06', approvalCode: '06', deptCode: '211000', startDate: baseDate },
    { empCode: 'EMP0028', name: '村上梅', occuCode: '04', approvalCode: '04', deptCode: '212000', startDate: baseDate }, // 課長
    { empCode: 'EMP0029', name: '近藤椿', occuCode: '05', approvalCode: '05', deptCode: '212000', startDate: baseDate },
    { empCode: 'EMP0030', name: '後藤菫', occuCode: '06', approvalCode: '06', deptCode: '212000', startDate: baseDate }, // パート
    { empCode: 'EMP0031', name: '藤田蘭', occuCode: '06', approvalCode: '06', deptCode: '212000', startDate: baseDate },

    // OEM部門
    { empCode: 'EMP0032', name: '岡崎薫', occuCode: '03', approvalCode: '03', deptCode: '220000', startDate: baseDate }, // 部門長
    { empCode: 'EMP0033', name: '青木杏', occuCode: '04', approvalCode: '04', deptCode: '221000', startDate: baseDate }, // 課長
    { empCode: 'EMP0034', name: '福田柚', occuCode: '05', approvalCode: '05', deptCode: '221000', startDate: baseDate },
    { empCode: 'EMP0035', name: '山崎橘', occuCode: '06', approvalCode: '06', deptCode: '221000', startDate: baseDate }, // パート
    { empCode: 'EMP0036', name: '太田百合', occuCode: '06', approvalCode: '06', deptCode: '221000', startDate: baseDate },

    // コンサルティング事業（12名：正社員6名、パート6名）
    // 顧客対応部門
    { empCode: 'EMP0037', name: '金子菖蒲', occuCode: '03', approvalCode: '03', deptCode: '310000', startDate: baseDate }, // 部門長
    { empCode: 'EMP0038', name: '藤井牡丹', occuCode: '04', approvalCode: '04', deptCode: '311000', startDate: baseDate }, // 課長
    { empCode: 'EMP0039', name: '増田菊', occuCode: '05', approvalCode: '05', deptCode: '311000', startDate: baseDate },
    { empCode: 'EMP0040', name: '大野紫陽花', occuCode: '05', approvalCode: '05', deptCode: '311000', startDate: baseDate },
    { empCode: 'EMP0041', name: '石井朝顔', occuCode: '06', approvalCode: '06', deptCode: '311000', startDate: baseDate }, // パート
    { empCode: 'EMP0042', name: '宮崎向日葵', occuCode: '06', approvalCode: '06', deptCode: '311000', startDate: baseDate },
    { empCode: 'EMP0043', name: '池田コスモス', occuCode: '04', approvalCode: '04', deptCode: '312000', startDate: baseDate }, // 課長
    { empCode: 'EMP0044', name: '橋本カトレア', occuCode: '05', approvalCode: '05', deptCode: '312000', startDate: baseDate },
    { empCode: 'EMP0045', name: '坂本スミレ', occuCode: '06', approvalCode: '06', deptCode: '312000', startDate: baseDate }, // パート

    // その他（経理・総務等）：2名
    { empCode: 'EMP0046', name: '原カーネーション', occuCode: '05', approvalCode: '05', deptCode: '000000', startDate: baseDate }, // 経理
    { empCode: 'EMP0047', name: '内田ダリア', occuCode: '05', approvalCode: '05', deptCode: '000000', startDate: baseDate } // 総務
  ]

  for (const emp of employees) {
    await prisma.employee.create({
      data: {
        ...emp,
        createDate: new Date(),
        creator: 'seed',
        updateDate: new Date(),
        updater: 'seed'
      }
    })
  }
  console.log(`✅ Created ${employees.length} employees`)

  // 取引先グループ（7件）
  console.log('🏢 Creating company groups...')
  const companyGroups = [
    { compGroupCode: 'CG01', groupName: '百貨店グループ' },
    { compGroupCode: 'CG02', groupName: 'スーパーグループ' },
    { compGroupCode: 'CG03', groupName: 'ホテル・旅館グループ' },
    { compGroupCode: 'CG04', groupName: '飲食店グループ' },
    { compGroupCode: 'CG05', groupName: '観光施設グループ' },
    { compGroupCode: 'CG06', groupName: '食肉卸グループ' },
    { compGroupCode: 'CG07', groupName: '畜産業者グループ' }
  ]

  for (const group of companyGroups) {
    await prisma.companyGroup.create({
      data: {
        ...group,
        createDate: new Date(),
        creator: 'seed',
        updateDate: new Date(),
        updater: 'seed'
      }
    })
  }
  console.log(`✅ Created ${companyGroups.length} company groups`)

  // 取引先マスタ（14件：得意先10件、仕入先4件）
  console.log('🤝 Creating companies...')
  const companies = [
    // 百貨店グループ（得意先）
    { compCode: 'COMP0001', name: 'X県地域百貨店', compGroupCode: 'CG01', supType: 0 },
    { compCode: 'COMP0002', name: 'X県有名百貨店', compGroupCode: 'CG01', supType: 0 },
    // スーパーグループ（得意先）
    { compCode: 'COMP0003', name: '地域スーパーチェーン', compGroupCode: 'CG02', supType: 0 },
    { compCode: 'COMP0004', name: '広域スーパーチェーン', compGroupCode: 'CG02', supType: 0 },
    // ホテル・旅館グループ（得意先）
    { compCode: 'COMP0005', name: 'X県シティホテル', compGroupCode: 'CG03', supType: 0 },
    { compCode: 'COMP0006', name: 'X県温泉旅館', compGroupCode: 'CG03', supType: 0 },
    // 飲食店グループ（得意先）
    { compCode: 'COMP0007', name: '高級焼肉レストラン', compGroupCode: 'CG04', supType: 0 },
    { compCode: 'COMP0008', name: '地域イタリアンレストラン', compGroupCode: 'CG04', supType: 0 },
    // 観光施設グループ（得意先）
    { compCode: 'COMP0009', name: 'X県道の駅', compGroupCode: 'CG05', supType: 0 },
    { compCode: 'COMP0010', name: 'X県観光センター', compGroupCode: 'CG05', supType: 0 },
    // 食肉卸グループ（仕入先）
    { compCode: 'COMP0011', name: '地域食肉卸A社', compGroupCode: 'CG06', supType: 1 },
    { compCode: 'COMP0012', name: '地域食肉卸B社', compGroupCode: 'CG06', supType: 1 },
    // 畜産業者グループ（仕入先）
    { compCode: 'COMP0013', name: 'X県畜産農家', compGroupCode: 'CG07', supType: 1 },
    { compCode: 'COMP0014', name: 'X県畜産組合', compGroupCode: 'CG07', supType: 1 }
  ]

  for (const company of companies) {
    await prisma.company.create({
      data: {
        ...company,
        createDate: new Date(),
        creator: 'seed',
        updateDate: new Date(),
        updater: 'seed'
      }
    })
  }
  console.log(`✅ Created ${companies.length} companies`)

  // 商品マスタ（20件）
  console.log('🥩 Creating products...')
  const products = [
    // 牛肉製品（5件）
    { prodCode: 'PROD0001', fullname: '黒毛和牛サーロイン', name: '和牛サーロイン', kana: 'クロゲワギュウサーロイン', unitprice: 15000, primeCost: 12000, taxType: 1, supCode: 'COMP0013' },
    { prodCode: 'PROD0002', fullname: '黒毛和牛ロース', name: '和牛ロース', kana: 'クロゲワギュウロース', unitprice: 12000, primeCost: 9600, taxType: 1, supCode: 'COMP0013' },
    { prodCode: 'PROD0003', fullname: '黒毛和牛カルビ', name: '和牛カルビ', kana: 'クロゲワギュウカルビ', unitprice: 10000, primeCost: 8000, taxType: 1, supCode: 'COMP0013' },
    { prodCode: 'PROD0004', fullname: '黒毛和牛ヒレ', name: '和牛ヒレ', kana: 'クロゲワギュウヒレ', unitprice: 18000, primeCost: 14400, taxType: 1, supCode: 'COMP0013' },
    { prodCode: 'PROD0005', fullname: '国産牛切り落とし', name: '国産牛切落', kana: 'コクサンギュウキリオトシ', unitprice: 3000, primeCost: 2400, taxType: 1, supCode: 'COMP0011' },
    // 豚肉製品（5件）
    { prodCode: 'PROD0006', fullname: '国産豚ロース', name: '国産豚ロース', kana: 'コクサンブタロース', unitprice: 2500, primeCost: 2000, taxType: 1, supCode: 'COMP0011' },
    { prodCode: 'PROD0007', fullname: '国産豚バラ', name: '国産豚バラ', kana: 'コクサンブタバラ', unitprice: 2000, primeCost: 1600, taxType: 1, supCode: 'COMP0011' },
    { prodCode: 'PROD0008', fullname: '国産豚ヒレ', name: '国産豚ヒレ', kana: 'コクサンブタヒレ', unitprice: 3500, primeCost: 2800, taxType: 1, supCode: 'COMP0011' },
    { prodCode: 'PROD0009', fullname: '国産豚コマ', name: '国産豚コマ', kana: 'コクサンブタコマ', unitprice: 1500, primeCost: 1200, taxType: 1, supCode: 'COMP0011' },
    { prodCode: 'PROD0010', fullname: '国産豚肩ロース', name: '豚肩ロース', kana: 'コクサンブタカタロース', unitprice: 2200, primeCost: 1760, taxType: 1, supCode: 'COMP0011' },
    // 鶏肉製品（5件）
    { prodCode: 'PROD0011', fullname: '国産鶏もも肉', name: '鶏もも肉', kana: 'コクサンニワトリモモニク', unitprice: 1800, primeCost: 1440, taxType: 1, supCode: 'COMP0012' },
    { prodCode: 'PROD0012', fullname: '国産鶏むね肉', name: '鶏むね肉', kana: 'コクサンニワトリムネニク', unitprice: 1200, primeCost: 960, taxType: 1, supCode: 'COMP0012' },
    { prodCode: 'PROD0013', fullname: '国産鶏手羽先', name: '鶏手羽先', kana: 'コクサンニワトリテバサキ', unitprice: 1000, primeCost: 800, taxType: 1, supCode: 'COMP0012' },
    { prodCode: 'PROD0014', fullname: '国産鶏手羽元', name: '鶏手羽元', kana: 'コクサンニワトリテバモト', unitprice: 900, primeCost: 720, taxType: 1, supCode: 'COMP0012' },
    { prodCode: 'PROD0015', fullname: '国産鶏ささみ', name: '鶏ささみ', kana: 'コクサンニワトリササミ', unitprice: 1500, primeCost: 1200, taxType: 1, supCode: 'COMP0012' },
    // 加工品（5件）
    { prodCode: 'PROD0016', fullname: '特製ローストビーフ', name: 'ﾛｰｽﾄﾋﾞｰﾌ', kana: 'トクセイローストビーフ', unitprice: 8000, primeCost: 6400, taxType: 1, supCode: 'COMP0011' },
    { prodCode: 'PROD0017', fullname: '手作りハム', name: '手作りハム', kana: 'テヅクリハム', unitprice: 5000, primeCost: 4000, taxType: 1, supCode: 'COMP0011' },
    { prodCode: 'PROD0018', fullname: '手作りソーセージ', name: 'ｿｰｾｰｼﾞ', kana: 'テヅクリソーセージ', unitprice: 4000, primeCost: 3200, taxType: 1, supCode: 'COMP0011' },
    { prodCode: 'PROD0019', fullname: '特製ベーコン', name: 'ベーコン', kana: 'トクセイベーコン', unitprice: 4500, primeCost: 3600, taxType: 1, supCode: 'COMP0011' },
    { prodCode: 'PROD0020', fullname: '揚げたてコロッケ', name: 'コロッケ', kana: 'アゲタテコロッケ', unitprice: 2000, primeCost: 1600, taxType: 1, supCode: 'COMP0011' }
  ]

  for (const product of products) {
    await prisma.product.create({
      data: {
        ...product,
        createDate: new Date(),
        creator: 'seed',
        updateDate: new Date(),
        updater: 'seed'
      }
    })
  }
  console.log(`✅ Created ${products.length} products`)

  // 倉庫マスタ（2件）
  console.log('🏭 Creating warehouses...')
  const warehouses = [
    { whCode: 'WH1', name: '本社倉庫' },
    { whCode: 'WH2', name: '工場倉庫' }
  ]

  for (const warehouse of warehouses) {
    await prisma.warehouse.create({
      data: {
        ...warehouse,
        createDate: new Date(),
        creator: 'seed',
        updateDate: new Date(),
        updater: 'seed'
      }
    })
  }
  console.log(`✅ Created ${warehouses.length} warehouses`)

  console.log('✨ Seeding completed!')
}

main()
  .then(async () => {
    await prisma.$disconnect()
  })
  .catch(async (e) => {
    console.error('❌ Error seeding database:', e)
    await prisma.$disconnect()
    process.exit(1)
  })
