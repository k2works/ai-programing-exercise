/* eslint-disable no-console, no-undef */
/**
 * B社（食肉と食肉加工品の製造・販売業）のサンプルデータ生成スクリプト
 *
 * B社の概要：
 * - 資本金3,000万円、従業員45名（パート21名）
 * - 食肉と食肉加工品の製造・販売
 * - 事業所：本社、工場、直営小売店1店舗
 * - 2021年度販売額：約9億円
 * - 取扱商品：牛肉・豚肉・鶏肉・食肉加工品
 */

import { PrismaClient } from '@prisma/client'

const prisma = new PrismaClient()

// 取引先グループデータ
const companyGroups = [
  { compGroupCode: 'G001', groupName: '百貨店グループ' },
  { compGroupCode: 'G002', groupName: 'スーパーグループ' },
  { compGroupCode: 'G003', groupName: 'ホテル・旅館グループ' },
  { compGroupCode: 'G004', groupName: '飲食店グループ' },
  { compGroupCode: 'G005', groupName: '観光施設グループ' },
  { compGroupCode: 'G006', groupName: '畜産業者グループ' },
  { compGroupCode: 'G007', groupName: '食材卸業者グループ' }
]

// 取引先データ
const companies = [
  // 得意先
  {
    compCode: 'CUS00001',
    name: '高島屋 横浜店',
    zipCode: '2200011',
    state: '神奈川県',
    address1: '横浜市西区高島',
    address2: '2-18-1',
    compGroupCode: 'G001'
  },
  {
    compCode: 'CUS00002',
    name: '伊勢丹 新宿本店',
    zipCode: '1600022',
    state: '東京都',
    address1: '新宿区新宿',
    address2: '3-14-1',
    compGroupCode: 'G001'
  },
  {
    compCode: 'CUS00003',
    name: 'イオン 幕張店',
    zipCode: '2610024',
    state: '千葉県',
    address1: '千葉市美浜区豊砂',
    address2: '1-1',
    compGroupCode: 'G002'
  },
  {
    compCode: 'CUS00004',
    name: '西友 池袋店',
    zipCode: '1710022',
    state: '東京都',
    address1: '豊島区南池袋',
    address2: '2-28-1',
    compGroupCode: 'G002'
  },
  {
    compCode: 'CUS00005',
    name: 'ホテルオークラ東京',
    zipCode: '1058582',
    state: '東京都',
    address1: '港区虎ノ門',
    address2: '2-10-4',
    compGroupCode: 'G003'
  },
  {
    compCode: 'CUS00006',
    name: '箱根湯本温泉 天成園',
    zipCode: '2500311',
    state: '神奈川県',
    address1: '足柄下郡箱根町湯本',
    address2: '682',
    compGroupCode: 'G003'
  },
  {
    compCode: 'CUS00007',
    name: 'レストラン シェ・松尾',
    zipCode: '1060032',
    state: '東京都',
    address1: '港区六本木',
    address2: '7-3-13',
    compGroupCode: 'G004'
  },
  {
    compCode: 'CUS00008',
    name: '焼肉 叙々苑 新宿店',
    zipCode: '1600022',
    state: '東京都',
    address1: '新宿区新宿',
    address2: '3-38-1',
    compGroupCode: 'G004'
  },
  {
    compCode: 'CUS00009',
    name: '道の駅 箱根峠',
    zipCode: '2500311',
    state: '神奈川県',
    address1: '足柄下郡箱根町',
    address2: '湯本256',
    compGroupCode: 'G005'
  },
  {
    compCode: 'CUS00010',
    name: '伊豆高原 お土産の里',
    zipCode: '4130232',
    state: '静岡県',
    address1: '伊東市八幡野',
    address2: '1183',
    compGroupCode: 'G005'
  },
  // 仕入先
  {
    compCode: 'SUP00001',
    name: '鈴木畜産',
    supType: 1,
    zipCode: '3200811',
    state: '栃木県',
    address1: '宇都宮市大通り',
    address2: '1-1-1',
    compGroupCode: 'G006'
  },
  {
    compCode: 'SUP00002',
    name: '山田養豚場',
    supType: 1,
    zipCode: '3270003',
    state: '栃木県',
    address1: '佐野市大橋町',
    address2: '1-1',
    compGroupCode: 'G006'
  },
  {
    compCode: 'SUP00003',
    name: '伊藤養鶏',
    supType: 1,
    zipCode: '3290205',
    state: '栃木県',
    address1: '小山市間々田',
    address2: '1-1-1',
    compGroupCode: 'G006'
  },
  {
    compCode: 'SUP00004',
    name: '食材卸センター 東京',
    supType: 1,
    zipCode: '1350061',
    state: '東京都',
    address1: '江東区豊洲',
    address2: '6-5-1',
    compGroupCode: 'G007'
  }
]

// 分類区分マスタ
const categoryTypes = [
  { categoryTypeCode: '01', categoryTypeName: '業種分類' },
  { categoryTypeCode: '02', categoryTypeName: '売上規模分類' },
  { categoryTypeCode: '03', categoryTypeName: '取引実績分類' }
]

// 取引先分類マスタ
const companyCategories = [
  { categoryTypeCode: '01', compCateCode: 'C0101', compCateName: '百貨店' },
  { categoryTypeCode: '01', compCateCode: 'C0102', compCateName: 'スーパー' },
  {
    categoryTypeCode: '01',
    compCateCode: 'C0103',
    compCateName: 'ホテル・旅館'
  },
  { categoryTypeCode: '01', compCateCode: 'C0104', compCateName: '飲食店' },
  { categoryTypeCode: '01', compCateCode: 'C0105', compCateName: '観光施設' },
  { categoryTypeCode: '01', compCateCode: 'C0106', compCateName: '畜産業者' },
  {
    categoryTypeCode: '01',
    compCateCode: 'C0107',
    compCateName: '食材卸業者'
  },
  {
    categoryTypeCode: '02',
    compCateCode: 'C0201',
    compCateName: '1億円以上'
  },
  {
    categoryTypeCode: '02',
    compCateCode: 'C0202',
    compCateName: '5千万円以上1億円未満'
  },
  {
    categoryTypeCode: '02',
    compCateCode: 'C0203',
    compCateName: '5千万円未満'
  },
  {
    categoryTypeCode: '03',
    compCateCode: 'C0301',
    compCateName: '10年以上の取引実績'
  },
  {
    categoryTypeCode: '03',
    compCateCode: 'C0302',
    compCateName: '5年以上10年未満の取引実績'
  },
  {
    categoryTypeCode: '03',
    compCateCode: 'C0303',
    compCateName: '5年未満の取引実績'
  }
]

// 取引先と分類の関連
const companyCategoryGroups = [
  // 百貨店
  {
    compCode: 'CUS00001',
    categoryTypeCode: '01',
    compCateCode: 'C0101'
  },
  {
    compCode: 'CUS00001',
    categoryTypeCode: '02',
    compCateCode: 'C0201'
  },
  {
    compCode: 'CUS00001',
    categoryTypeCode: '03',
    compCateCode: 'C0301'
  },
  {
    compCode: 'CUS00002',
    categoryTypeCode: '01',
    compCateCode: 'C0101'
  },
  {
    compCode: 'CUS00002',
    categoryTypeCode: '02',
    compCateCode: 'C0201'
  },
  {
    compCode: 'CUS00002',
    categoryTypeCode: '03',
    compCateCode: 'C0301'
  },
  // スーパー
  {
    compCode: 'CUS00003',
    categoryTypeCode: '01',
    compCateCode: 'C0102'
  },
  {
    compCode: 'CUS00003',
    categoryTypeCode: '02',
    compCateCode: 'C0202'
  },
  {
    compCode: 'CUS00003',
    categoryTypeCode: '03',
    compCateCode: 'C0302'
  },
  {
    compCode: 'CUS00004',
    categoryTypeCode: '01',
    compCateCode: 'C0102'
  },
  {
    compCode: 'CUS00004',
    categoryTypeCode: '02',
    compCateCode: 'C0202'
  },
  {
    compCode: 'CUS00004',
    categoryTypeCode: '03',
    compCateCode: 'C0302'
  },
  // ホテル・旅館
  {
    compCode: 'CUS00005',
    categoryTypeCode: '01',
    compCateCode: 'C0103'
  },
  {
    compCode: 'CUS00005',
    categoryTypeCode: '02',
    compCateCode: 'C0201'
  },
  {
    compCode: 'CUS00005',
    categoryTypeCode: '03',
    compCateCode: 'C0301'
  },
  {
    compCode: 'CUS00006',
    categoryTypeCode: '01',
    compCateCode: 'C0103'
  },
  {
    compCode: 'CUS00006',
    categoryTypeCode: '02',
    compCateCode: 'C0202'
  },
  {
    compCode: 'CUS00006',
    categoryTypeCode: '03',
    compCateCode: 'C0302'
  },
  // 飲食店
  {
    compCode: 'CUS00007',
    categoryTypeCode: '01',
    compCateCode: 'C0104'
  },
  {
    compCode: 'CUS00007',
    categoryTypeCode: '02',
    compCateCode: 'C0203'
  },
  {
    compCode: 'CUS00007',
    categoryTypeCode: '03',
    compCateCode: 'C0302'
  },
  {
    compCode: 'CUS00008',
    categoryTypeCode: '01',
    compCateCode: 'C0104'
  },
  {
    compCode: 'CUS00008',
    categoryTypeCode: '02',
    compCateCode: 'C0202'
  },
  {
    compCode: 'CUS00008',
    categoryTypeCode: '03',
    compCateCode: 'C0302'
  },
  // 観光施設
  {
    compCode: 'CUS00009',
    categoryTypeCode: '01',
    compCateCode: 'C0105'
  },
  {
    compCode: 'CUS00009',
    categoryTypeCode: '02',
    compCateCode: 'C0203'
  },
  {
    compCode: 'CUS00009',
    categoryTypeCode: '03',
    compCateCode: 'C0303'
  },
  {
    compCode: 'CUS00010',
    categoryTypeCode: '01',
    compCateCode: 'C0105'
  },
  {
    compCode: 'CUS00010',
    categoryTypeCode: '02',
    compCateCode: 'C0203'
  },
  {
    compCode: 'CUS00010',
    categoryTypeCode: '03',
    compCateCode: 'C0303'
  },
  // 畜産業者
  {
    compCode: 'SUP00001',
    categoryTypeCode: '01',
    compCateCode: 'C0106'
  },
  {
    compCode: 'SUP00001',
    categoryTypeCode: '02',
    compCateCode: 'C0202'
  },
  {
    compCode: 'SUP00001',
    categoryTypeCode: '03',
    compCateCode: 'C0301'
  },
  {
    compCode: 'SUP00002',
    categoryTypeCode: '01',
    compCateCode: 'C0106'
  },
  {
    compCode: 'SUP00002',
    categoryTypeCode: '02',
    compCateCode: 'C0202'
  },
  {
    compCode: 'SUP00002',
    categoryTypeCode: '03',
    compCateCode: 'C0301'
  },
  {
    compCode: 'SUP00003',
    categoryTypeCode: '01',
    compCateCode: 'C0106'
  },
  {
    compCode: 'SUP00003',
    categoryTypeCode: '02',
    compCateCode: 'C0203'
  },
  {
    compCode: 'SUP00003',
    categoryTypeCode: '03',
    compCateCode: 'C0302'
  },
  // 食材卸業者
  {
    compCode: 'SUP00004',
    categoryTypeCode: '01',
    compCateCode: 'C0107'
  },
  {
    compCode: 'SUP00004',
    categoryTypeCode: '02',
    compCateCode: 'C0202'
  },
  {
    compCode: 'SUP00004',
    categoryTypeCode: '03',
    compCateCode: 'C0301'
  }
]

// 得意先データ
const customers = [
  {
    custCode: 'CUS00001',
    custSubNo: 1,
    arCode: 'CUS00001',
    arSubNo: 1,
    payerCode: 'CUS00001',
    payerSubNo: 1,
    name: '高島屋 横浜店',
    empCode: 'EMP0000001',
    custCloseDate1: 31,
    custCloseDate2: 31
  },
  {
    custCode: 'CUS00002',
    custSubNo: 1,
    arCode: 'CUS00002',
    arSubNo: 1,
    payerCode: 'CUS00002',
    payerSubNo: 1,
    name: '伊勢丹 新宿本店',
    empCode: 'EMP0000001',
    custCloseDate1: 31,
    custCloseDate2: 31
  },
  {
    custCode: 'CUS00003',
    custSubNo: 1,
    arCode: 'CUS00003',
    arSubNo: 1,
    payerCode: 'CUS00003',
    payerSubNo: 1,
    name: 'イオン 幕張店',
    empCode: 'EMP0000001',
    custCloseDate1: 20,
    custCloseDate2: 20
  },
  {
    custCode: 'CUS00004',
    custSubNo: 1,
    arCode: 'CUS00004',
    arSubNo: 1,
    payerCode: 'CUS00004',
    payerSubNo: 1,
    name: '西友 池袋店',
    empCode: 'EMP0000001',
    custCloseDate1: 20,
    custCloseDate2: 20
  },
  {
    custCode: 'CUS00005',
    custSubNo: 1,
    arCode: 'CUS00005',
    arSubNo: 1,
    payerCode: 'CUS00005',
    payerSubNo: 1,
    name: 'ホテルオークラ東京',
    empCode: 'EMP0000001',
    custCloseDate1: 31,
    custCloseDate2: 31
  },
  {
    custCode: 'CUS00006',
    custSubNo: 1,
    arCode: 'CUS00006',
    arSubNo: 1,
    payerCode: 'CUS00006',
    payerSubNo: 1,
    name: '箱根湯本温泉 天成園',
    empCode: 'EMP0000001',
    custCloseDate1: 31,
    custCloseDate2: 31
  },
  {
    custCode: 'CUS00007',
    custSubNo: 1,
    arCode: 'CUS00007',
    arSubNo: 1,
    payerCode: 'CUS00007',
    payerSubNo: 1,
    name: 'レストラン シェ・松尾',
    empCode: 'EMP0000001',
    custCloseDate1: 31,
    custCloseDate2: 31
  },
  {
    custCode: 'CUS00008',
    custSubNo: 1,
    arCode: 'CUS00008',
    arSubNo: 1,
    payerCode: 'CUS00008',
    payerSubNo: 1,
    name: '焼肉 叙々苑 新宿店',
    empCode: 'EMP0000001',
    custCloseDate1: 31,
    custCloseDate2: 31
  },
  {
    custCode: 'CUS00009',
    custSubNo: 1,
    arCode: 'CUS00009',
    arSubNo: 1,
    payerCode: 'CUS00009',
    payerSubNo: 1,
    name: '道の駅 箱根峠',
    empCode: 'EMP0000001',
    custCloseDate1: 31,
    custCloseDate2: 31
  },
  {
    custCode: 'CUS00010',
    custSubNo: 1,
    arCode: 'CUS00010',
    arSubNo: 1,
    payerCode: 'CUS00010',
    payerSubNo: 1,
    name: '伊豆高原 お土産の里',
    empCode: 'EMP0000001',
    custCloseDate1: 31,
    custCloseDate2: 31
  }
]

// 仕入先データ
const suppliers = [
  {
    supCode: 'SUP00001',
    supSubNo: 1,
    name: '鈴木畜産',
    supCloseDate: 31
  },
  {
    supCode: 'SUP00002',
    supSubNo: 1,
    name: '山田養豚場',
    supCloseDate: 31
  },
  {
    supCode: 'SUP00003',
    supSubNo: 1,
    name: '伊藤養鶏',
    supCloseDate: 31
  },
  {
    supCode: 'SUP00004',
    supSubNo: 1,
    name: '食材卸センター 東京',
    supCloseDate: 20
  }
]

// 商品データ
const products = [
  // 牛肉
  {
    prodCode: 'PROD00001',
    fullname: '黒毛和牛サーロインステーキ 200g',
    name: 'サーロイン',
    kana: 'クロゲワギュウサーロイン',
    unitprice: 5000,
    primeCost: 3500,
    supCode: 'SUP00001'
  },
  {
    prodCode: 'PROD00002',
    fullname: '黒毛和牛リブロース 200g',
    name: 'リブロース',
    kana: 'クロゲワギュウリブロース',
    unitprice: 4500,
    primeCost: 3200,
    supCode: 'SUP00001'
  },
  {
    prodCode: 'PROD00003',
    fullname: '黒毛和牛ヒレ 150g',
    name: 'ヒレ',
    kana: 'クロゲワギュウヒレ',
    unitprice: 6000,
    primeCost: 4200,
    supCode: 'SUP00001'
  },
  {
    prodCode: 'PROD00004',
    fullname: '国産牛カルビ 300g',
    name: 'カルビ',
    kana: 'コクサンギュウカルビ',
    unitprice: 2500,
    primeCost: 1800,
    supCode: 'SUP00001'
  },
  {
    prodCode: 'PROD00005',
    fullname: '国産牛もも肉スライス 200g',
    name: 'もも肉',
    kana: 'コクサンギュウモモニク',
    unitprice: 1200,
    primeCost: 850,
    supCode: 'SUP00001'
  },
  // 豚肉
  {
    prodCode: 'PROD00006',
    fullname: 'ブランド豚ロース 200g',
    name: '豚ロース',
    kana: 'ブランドブタロース',
    unitprice: 800,
    primeCost: 560,
    supCode: 'SUP00002'
  },
  {
    prodCode: 'PROD00007',
    fullname: 'ブランド豚バラ 200g',
    name: '豚バラ',
    kana: 'ブランドブタバラ',
    unitprice: 700,
    primeCost: 490,
    supCode: 'SUP00002'
  },
  {
    prodCode: 'PROD00008',
    fullname: '国産豚もも肉 200g',
    name: '豚もも',
    kana: 'コクサンブタモモニク',
    unitprice: 600,
    primeCost: 420,
    supCode: 'SUP00002'
  },
  {
    prodCode: 'PROD00009',
    fullname: '国産豚ひき肉 300g',
    name: '豚ひき肉',
    kana: 'コクサンブタヒキニク',
    unitprice: 500,
    primeCost: 350,
    supCode: 'SUP00002'
  },
  // 鶏肉
  {
    prodCode: 'PROD00010',
    fullname: '地鶏もも肉 300g',
    name: '鶏もも',
    kana: 'ジドリモモニク',
    unitprice: 900,
    primeCost: 630,
    supCode: 'SUP00003'
  },
  {
    prodCode: 'PROD00011',
    fullname: '地鶏むね肉 300g',
    name: '鶏むね',
    kana: 'ジドリムネニク',
    unitprice: 700,
    primeCost: 490,
    supCode: 'SUP00003'
  },
  {
    prodCode: 'PROD00012',
    fullname: '国産鶏ささみ 200g',
    name: '鶏ささみ',
    kana: 'コクサンケイササミ',
    unitprice: 500,
    primeCost: 350,
    supCode: 'SUP00003'
  },
  {
    prodCode: 'PROD00013',
    fullname: '国産鶏手羽元 500g',
    name: '鶏手羽元',
    kana: 'コクサンケイテバモト',
    unitprice: 600,
    primeCost: 420,
    supCode: 'SUP00003'
  },
  // 加工品
  {
    prodCode: 'PROD00014',
    fullname: '自家製ロースハム 200g',
    name: 'ロースハム',
    kana: 'ジカセイロースハム',
    unitprice: 1500,
    primeCost: 900,
    supCode: 'SUP00004'
  },
  {
    prodCode: 'PROD00015',
    fullname: '自家製ウインナーソーセージ 300g',
    name: 'ウインナー',
    kana: 'ジカセイウインナーソーセージ',
    unitprice: 1200,
    primeCost: 720,
    supCode: 'SUP00004'
  },
  {
    prodCode: 'PROD00016',
    fullname: '自家製ローストビーフ 150g',
    name: 'ローストビーフ',
    kana: 'ジカセイローストビーフ',
    unitprice: 2800,
    primeCost: 1680,
    supCode: 'SUP00001'
  },
  {
    prodCode: 'PROD00017',
    fullname: '自家製コロッケ（牛肉）5個入',
    name: 'コロッケ',
    kana: 'ジカセイコロッケ',
    unitprice: 600,
    primeCost: 360,
    supCode: 'SUP00004'
  },
  {
    prodCode: 'PROD00018',
    fullname: '贈答用ハム・ソーセージセット',
    name: 'ギフトセット',
    kana: 'ゾウトウヨウハムソーセージセット',
    unitprice: 5000,
    primeCost: 3000,
    supCode: 'SUP00004'
  },
  {
    prodCode: 'PROD00019',
    fullname: '贈答用特選和牛セット',
    name: '和牛セット',
    kana: 'ゾウトウヨウトクセンワギュウセット',
    unitprice: 15000,
    primeCost: 9000,
    supCode: 'SUP00001'
  },
  {
    prodCode: 'PROD00020',
    fullname: 'お土産用ビーフジャーキー 80g',
    name: 'ジャーキー',
    kana: 'オミヤゲヨウビーフジャーキー',
    unitprice: 800,
    primeCost: 480,
    supCode: 'SUP00004'
  }
]

// 倉庫データ
const warehouses = [
  {
    whCode: 'WH1',
    name: '本社倉庫'
  },
  {
    whCode: 'WH2',
    name: '工場倉庫'
  }
]

/**
 * メイン処理
 */
async function main(): Promise<void> {
  console.log('🌱 シードデータの投入を開始します...')

  // 既存データの削除
  console.log('📦 既存データを削除中...')
  await prisma.creditBalance.deleteMany()
  await prisma.autoNumber.deleteMany()
  await prisma.payment.deleteMany()
  await prisma.credit.deleteMany()
  await prisma.invoiceDetail.deleteMany()
  await prisma.invoice.deleteMany()
  await prisma.stock.deleteMany()
  await prisma.purchaseDetail.deleteMany()
  await prisma.purchase.deleteMany()
  await prisma.purchaseOrderDetail.deleteMany()
  await prisma.purchaseOrder.deleteMany()
  await prisma.salesDetail.deleteMany()
  await prisma.sales.deleteMany()
  await prisma.orderDetail.deleteMany()
  await prisma.order.deleteMany()
  await prisma.warehouse.deleteMany()
  await prisma.product.deleteMany()
  await prisma.customer.deleteMany()
  await prisma.supplier.deleteMany()
  await prisma.companyCategoryGroup.deleteMany()
  await prisma.companyCategory.deleteMany()
  await prisma.categoryType.deleteMany()
  await prisma.company.deleteMany()
  await prisma.companyGroup.deleteMany()

  // マスタデータの投入
  console.log('📝 マスタデータを投入中...')

  await prisma.companyGroup.createMany({ data: companyGroups })
  console.log(`✅ 取引先グループ: ${companyGroups.length}件`)

  await prisma.company.createMany({ data: companies })
  console.log(`✅ 取引先: ${companies.length}件`)

  await prisma.categoryType.createMany({ data: categoryTypes })
  console.log(`✅ 分類区分: ${categoryTypes.length}件`)

  await prisma.companyCategory.createMany({ data: companyCategories })
  console.log(`✅ 取引先分類: ${companyCategories.length}件`)

  await prisma.companyCategoryGroup.createMany({ data: companyCategoryGroups })
  console.log(`✅ 取引先分類グループ: ${companyCategoryGroups.length}件`)

  await prisma.customer.createMany({ data: customers })
  console.log(`✅ 得意先: ${customers.length}件`)

  await prisma.supplier.createMany({ data: suppliers })
  console.log(`✅ 仕入先: ${suppliers.length}件`)

  await prisma.product.createMany({ data: products })
  console.log(`✅ 商品: ${products.length}件`)

  await prisma.warehouse.createMany({ data: warehouses })
  console.log(`✅ 倉庫: ${warehouses.length}件`)

  console.log('✨ シードデータの投入が完了しました！')
}

main()
  .catch((e) => {
    console.error('❌ エラーが発生しました:', e)
    process.exit(1)
  })
  .finally(async () => {
    await prisma.$disconnect()
  })
