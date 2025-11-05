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

// 部門データ（B社の組織構造）
const departments = [
  {
    deptCode: '000000',
    startDate: new Date('2020-01-01'),
    name: '本社',
    layer: 0,
    psth: '/000000',
    lowestType: 0,
    slitYn: 1
  },
  {
    deptCode: '100000',
    startDate: new Date('2020-01-01'),
    name: '食肉製造・販売事業',
    layer: 1,
    psth: '/000000/100000',
    lowestType: 0,
    slitYn: 1
  },
  {
    deptCode: '110000',
    startDate: new Date('2020-01-01'),
    name: '食肉加工部門',
    layer: 2,
    psth: '/000000/100000/110000',
    lowestType: 0,
    slitYn: 1
  },
  {
    deptCode: '111000',
    startDate: new Date('2020-01-01'),
    name: '牛肉・豚肉・鶏肉部門',
    layer: 3,
    psth: '/000000/100000/110000/111000',
    lowestType: 1,
    slitYn: 1
  },
  {
    deptCode: '112000',
    startDate: new Date('2020-01-01'),
    name: '食肉加工品部門',
    layer: 3,
    psth: '/000000/100000/110000/112000',
    lowestType: 1,
    slitYn: 1
  },
  {
    deptCode: '120000',
    startDate: new Date('2020-01-01'),
    name: '小売販売部門',
    layer: 2,
    psth: '/000000/100000/120000',
    lowestType: 0,
    slitYn: 1
  },
  {
    deptCode: '121000',
    startDate: new Date('2020-01-01'),
    name: '直営小売店課',
    layer: 3,
    psth: '/000000/100000/120000/121000',
    lowestType: 1,
    slitYn: 1
  },
  {
    deptCode: '122000',
    startDate: new Date('2020-01-01'),
    name: '百貨店・スーパー向け販売課',
    layer: 3,
    psth: '/000000/100000/120000/122000',
    lowestType: 1,
    slitYn: 1
  },
  {
    deptCode: '130000',
    startDate: new Date('2020-01-01'),
    name: '新規取引先開拓部門',
    layer: 2,
    psth: '/000000/100000/130000',
    lowestType: 0,
    slitYn: 1
  },
  {
    deptCode: '131000',
    startDate: new Date('2020-01-01'),
    name: 'ホテル・旅館向け課',
    layer: 3,
    psth: '/000000/100000/130000/131000',
    lowestType: 1,
    slitYn: 1
  },
  {
    deptCode: '132000',
    startDate: new Date('2020-01-01'),
    name: '飲食店向け課',
    layer: 3,
    psth: '/000000/100000/130000/132000',
    lowestType: 1,
    slitYn: 1
  },
  {
    deptCode: '200000',
    startDate: new Date('2020-01-01'),
    name: '食肉加工品事業',
    layer: 1,
    psth: '/000000/200000',
    lowestType: 0,
    slitYn: 1
  },
  {
    deptCode: '210000',
    startDate: new Date('2020-01-01'),
    name: '自社ブランド部門',
    layer: 2,
    psth: '/000000/200000/210000',
    lowestType: 0,
    slitYn: 1
  },
  {
    deptCode: '211000',
    startDate: new Date('2020-01-01'),
    name: '贈答用製品製造課',
    layer: 3,
    psth: '/000000/200000/210000/211000',
    lowestType: 1,
    slitYn: 1
  },
  {
    deptCode: '212000',
    startDate: new Date('2020-01-01'),
    name: '道の駅・土産物製品販売課',
    layer: 3,
    psth: '/000000/200000/210000/212000',
    lowestType: 1,
    slitYn: 1
  },
  {
    deptCode: '220000',
    startDate: new Date('2020-01-01'),
    name: '相手先ブランド製造(OEM)部門',
    layer: 2,
    psth: '/000000/200000/220000',
    lowestType: 0,
    slitYn: 1
  },
  {
    deptCode: '221000',
    startDate: new Date('2020-01-01'),
    name: '客先要望対応課',
    layer: 3,
    psth: '/000000/200000/220000/221000',
    lowestType: 1,
    slitYn: 1
  },
  {
    deptCode: '300000',
    startDate: new Date('2020-01-01'),
    name: 'コンサルティング事業',
    layer: 1,
    psth: '/000000/300000',
    lowestType: 0,
    slitYn: 1
  },
  {
    deptCode: '310000',
    startDate: new Date('2020-01-01'),
    name: '顧客対応部門',
    layer: 2,
    psth: '/000000/300000/310000',
    lowestType: 0,
    slitYn: 1
  },
  {
    deptCode: '311000',
    startDate: new Date('2020-01-01'),
    name: 'メニュー提案課',
    layer: 3,
    psth: '/000000/300000/310000/311000',
    lowestType: 1,
    slitYn: 1
  },
  {
    deptCode: '312000',
    startDate: new Date('2020-01-01'),
    name: '半加工商品提供課',
    layer: 3,
    psth: '/000000/300000/310000/312000',
    lowestType: 1,
    slitYn: 1
  }
]

// 社員データ（従業員45名：正社員24名、パート21名）
const employees = [
  // 経営層（2名）
  {
    empCode: 'EMP0000001',
    name: '佐藤 太郎',
    kana: 'サトウ タロウ',
    loginPassword: 'pass0001',
    tel: '0459001001',
    deptCode: '000000',
    startDate: new Date('2020-01-01'),
    occuCode: '01',
    approvalCode: '01'
  },
  {
    empCode: 'EMP0000002',
    name: '鈴木 次郎',
    kana: 'スズキ ジロウ',
    loginPassword: 'pass0002',
    tel: '0459001002',
    deptCode: '000000',
    startDate: new Date('2020-01-01'),
    occuCode: '02',
    approvalCode: '02'
  },
  // 食肉製造・販売事業（正社員8名、パート7名）
  {
    empCode: 'EMP0000003',
    name: '田中 一郎',
    kana: 'タナカ イチロウ',
    loginPassword: 'pass0003',
    tel: '0459001003',
    deptCode: '111000',
    startDate: new Date('2020-01-01'),
    occuCode: '03',
    approvalCode: '03'
  },
  {
    empCode: 'EMP0000004',
    name: '高橋 健太',
    kana: 'タカハシ ケンタ',
    loginPassword: 'pass0004',
    tel: '0459001004',
    deptCode: '111000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000005',
    name: '伊藤 真由美',
    kana: 'イトウ マユミ',
    loginPassword: 'pass0005',
    tel: '0459001005',
    deptCode: '111000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000006',
    name: '渡辺 浩二',
    kana: 'ワタナベ コウジ',
    loginPassword: 'pass0006',
    tel: '0459001006',
    deptCode: '112000',
    startDate: new Date('2020-01-01'),
    occuCode: '03',
    approvalCode: '03'
  },
  {
    empCode: 'EMP0000007',
    name: '山本 美咲',
    kana: 'ヤマモト ミサキ',
    loginPassword: 'pass0007',
    tel: '0459001007',
    deptCode: '112000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000008',
    name: '中村 隆',
    kana: 'ナカムラ タカシ',
    loginPassword: 'pass0008',
    tel: '0459001008',
    deptCode: '121000',
    startDate: new Date('2020-01-01'),
    occuCode: '03',
    approvalCode: '03'
  },
  {
    empCode: 'EMP0000009',
    name: '小林 愛子',
    kana: 'コバヤシ アイコ',
    loginPassword: 'pass0009',
    tel: '0459001009',
    deptCode: '121000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000010',
    name: '加藤 大輔',
    kana: 'カトウ ダイスケ',
    loginPassword: 'pass0010',
    tel: '0459001010',
    deptCode: '122000',
    startDate: new Date('2020-01-01'),
    occuCode: '03',
    approvalCode: '03'
  },
  {
    empCode: 'EMP0000011',
    name: '吉田 麻衣',
    kana: 'ヨシダ マイ',
    loginPassword: 'pass0011',
    tel: '0459001011',
    deptCode: '122000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000012',
    name: '山田 修',
    kana: 'ヤマダ オサム',
    loginPassword: 'pass0012',
    tel: '0459001012',
    deptCode: '131000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000013',
    name: '佐々木 由美',
    kana: 'ササキ ユミ',
    loginPassword: 'pass0013',
    tel: '0459001013',
    deptCode: '132000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  // パート社員（食肉製造・販売事業）
  {
    empCode: 'EMP0000014',
    name: '松本 花子',
    kana: 'マツモト ハナコ',
    loginPassword: 'pass0014',
    tel: '0459001014',
    deptCode: '111000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000015',
    name: '井上 幸子',
    kana: 'イノウエ サチコ',
    loginPassword: 'pass0015',
    tel: '0459001015',
    deptCode: '111000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000016',
    name: '木村 恵子',
    kana: 'キムラ ケイコ',
    loginPassword: 'pass0016',
    tel: '0459001016',
    deptCode: '112000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000017',
    name: '林 久美子',
    kana: 'ハヤシ クミコ',
    loginPassword: 'pass0017',
    tel: '0459001017',
    deptCode: '112000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000018',
    name: '斎藤 典子',
    kana: 'サイトウ ノリコ',
    loginPassword: 'pass0018',
    tel: '0459001018',
    deptCode: '121000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000019',
    name: '清水 明美',
    kana: 'シミズ アケミ',
    loginPassword: 'pass0019',
    tel: '0459001019',
    deptCode: '121000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000020',
    name: '山口 美穂',
    kana: 'ヤマグチ ミホ',
    loginPassword: 'pass0020',
    tel: '0459001020',
    deptCode: '121000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  // 食肉加工品事業（正社員6名、パート8名）
  {
    empCode: 'EMP0000021',
    name: '森 健一',
    kana: 'モリ ケンイチ',
    loginPassword: 'pass0021',
    tel: '0459001021',
    deptCode: '211000',
    startDate: new Date('2020-01-01'),
    occuCode: '03',
    approvalCode: '03'
  },
  {
    empCode: 'EMP0000022',
    name: '池田 隆志',
    kana: 'イケダ タカシ',
    loginPassword: 'pass0022',
    tel: '0459001022',
    deptCode: '211000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000023',
    name: '橋本 美香',
    kana: 'ハシモト ミカ',
    loginPassword: 'pass0023',
    tel: '0459001023',
    deptCode: '211000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000024',
    name: '坂本 英樹',
    kana: 'サカモト ヒデキ',
    loginPassword: 'pass0024',
    tel: '0459001024',
    deptCode: '212000',
    startDate: new Date('2020-01-01'),
    occuCode: '03',
    approvalCode: '03'
  },
  {
    empCode: 'EMP0000025',
    name: '青木 千鶴',
    kana: 'アオキ チヅル',
    loginPassword: 'pass0025',
    tel: '0459001025',
    deptCode: '212000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000026',
    name: '藤田 誠',
    kana: 'フジタ マコト',
    loginPassword: 'pass0026',
    tel: '0459001026',
    deptCode: '221000',
    startDate: new Date('2020-01-01'),
    occuCode: '03',
    approvalCode: '03'
  },
  {
    empCode: 'EMP0000027',
    name: '西村 真理子',
    kana: 'ニシムラ マリコ',
    loginPassword: 'pass0027',
    tel: '0459001027',
    deptCode: '221000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  // パート社員（食肉加工品事業）
  {
    empCode: 'EMP0000028',
    name: '岡田 和子',
    kana: 'オカダ カズコ',
    loginPassword: 'pass0028',
    tel: '0459001028',
    deptCode: '211000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000029',
    name: '長谷川 文子',
    kana: 'ハセガワ フミコ',
    loginPassword: 'pass0029',
    tel: '0459001029',
    deptCode: '211000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000030',
    name: '石川 良子',
    kana: 'イシカワ ヨシコ',
    loginPassword: 'pass0030',
    tel: '0459001030',
    deptCode: '211000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000031',
    name: '前田 京子',
    kana: 'マエダ キョウコ',
    loginPassword: 'pass0031',
    tel: '0459001031',
    deptCode: '211000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000032',
    name: '藤井 静香',
    kana: 'フジイ シズカ',
    loginPassword: 'pass0032',
    tel: '0459001032',
    deptCode: '212000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000033',
    name: '岡本 春子',
    kana: 'オカモト ハルコ',
    loginPassword: 'pass0033',
    tel: '0459001033',
    deptCode: '212000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000034',
    name: '村上 綾子',
    kana: 'ムラカミ アヤコ',
    loginPassword: 'pass0034',
    tel: '0459001034',
    deptCode: '221000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000035',
    name: '近藤 悦子',
    kana: 'コンドウ エツコ',
    loginPassword: 'pass0035',
    tel: '0459001035',
    deptCode: '221000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  // コンサルティング事業（正社員6名、パート6名）
  {
    empCode: 'EMP0000036',
    name: '遠藤 秀樹',
    kana: 'エンドウ ヒデキ',
    loginPassword: 'pass0036',
    tel: '0459001036',
    deptCode: '311000',
    startDate: new Date('2020-01-01'),
    occuCode: '03',
    approvalCode: '03'
  },
  {
    empCode: 'EMP0000037',
    name: '久保 裕太',
    kana: 'クボ ユウタ',
    loginPassword: 'pass0037',
    tel: '0459001037',
    deptCode: '311000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000038',
    name: '原田 美穂',
    kana: 'ハラダ ミホ',
    loginPassword: 'pass0038',
    tel: '0459001038',
    deptCode: '311000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000039',
    name: '福田 剛',
    kana: 'フクダ ツヨシ',
    loginPassword: 'pass0039',
    tel: '0459001039',
    deptCode: '312000',
    startDate: new Date('2020-01-01'),
    occuCode: '03',
    approvalCode: '03'
  },
  {
    empCode: 'EMP0000040',
    name: '太田 恵',
    kana: 'オオタ メグミ',
    loginPassword: 'pass0040',
    tel: '0459001040',
    deptCode: '312000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  {
    empCode: 'EMP0000041',
    name: '上田 智子',
    kana: 'ウエダ トモコ',
    loginPassword: 'pass0041',
    tel: '0459001041',
    deptCode: '312000',
    startDate: new Date('2020-01-01'),
    occuCode: '04',
    approvalCode: '04'
  },
  // パート社員（コンサルティング事業）
  {
    empCode: 'EMP0000042',
    name: '松田 直美',
    kana: 'マツダ ナオミ',
    loginPassword: 'pass0042',
    tel: '0459001042',
    deptCode: '311000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000043',
    name: '竹内 香織',
    kana: 'タケウチ カオリ',
    loginPassword: 'pass0043',
    tel: '0459001043',
    deptCode: '311000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000044',
    name: '中島 理恵',
    kana: 'ナカジマ リエ',
    loginPassword: 'pass0044',
    tel: '0459001044',
    deptCode: '312000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  },
  {
    empCode: 'EMP0000045',
    name: '小川 美和',
    kana: 'オガワ ミワ',
    loginPassword: 'pass0045',
    tel: '0459001045',
    deptCode: '312000',
    startDate: new Date('2020-01-01'),
    occuCode: '05',
    approvalCode: '05'
  }
]

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
  await prisma.employee.deleteMany()
  await prisma.department.deleteMany()

  // マスタデータの投入
  console.log('📝 マスタデータを投入中...')

  await prisma.department.createMany({ data: departments })
  console.log(`✅ 部門: ${departments.length}件`)

  await prisma.employee.createMany({ data: employees })
  console.log(`✅ 社員: ${employees.length}件`)

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
