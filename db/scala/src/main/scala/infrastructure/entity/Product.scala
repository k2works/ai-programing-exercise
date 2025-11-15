package infrastructure.entity

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 商品マスタのドメインモデル
 *
 * 販売する商品の基本情報を保持する
 *
 * @param prodCode 商品コード
 * @param fullName 正式商品名
 * @param name 商品略称
 * @param kana 商品名カナ
 * @param prodType 商品種別（1:製品、2:半製品、3:原材料）
 * @param serialNo 製造番号
 * @param unitPrice 標準販売単価
 * @param poPrice 標準仕入単価
 * @param primeCost 標準原価
 * @param taxType 税区分（1:課税、2:非課税、3:免税）
 * @param categoryCode 商品分類コード
 * @param wideUseType 汎用区分
 * @param stockManageType 在庫管理区分（0:管理しない、1:管理する）
 * @param stockReserveType 在庫引当区分（0:引当しない、1:引当する）
 * @param supCode 仕入先コード
 * @param supSubNo 仕入先枝番
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class Product(
  prodCode: String,
  fullName: String,
  name: String,
  kana: Option[String] = None,
  prodType: Option[String] = None,
  serialNo: Option[String] = None,
  unitPrice: Int = 0,
  poPrice: Int = 0,
  primeCost: Int = 0,
  taxType: Int = 1,
  categoryCode: Option[String] = None,
  wideUseType: Option[Int] = None,
  stockManageType: Int = 1,
  stockReserveType: Option[Int] = None,
  supCode: Option[String] = None,
  supSubNo: Option[Int] = None,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = "",
)

object Product extends SQLSyntaxSupport[Product] {
  override val tableName = "商品マスタ"

  override val columns   = Seq(
    "商品コード",
    "商品正式名",
    "商品略称",
    "商品名カナ",
    "商品区分",
    "製品型番",
    "販売単価",
    "仕入単価",
    "売上原価",
    "税区分",
    "商品分類コード",
    "雑区分",
    "在庫管理対象区分",
    "在庫引当区分",
    "仕入先コード",
    "仕入先枝番",
    "作成日時",
    "作成者名",
    "更新日時",
    "更新者名",
  )

  def apply(rs: WrappedResultSet): Product = Product(
    prodCode = rs.string("商品コード"),
    fullName = rs.string("商品正式名"),
    name = rs.string("商品略称"),
    kana = rs.stringOpt("商品名カナ"),
    prodType = rs.stringOpt("商品区分"),
    serialNo = rs.stringOpt("製品型番"),
    unitPrice = rs.int("販売単価"),
    poPrice = rs.int("仕入単価"),
    primeCost = rs.int("売上原価"),
    taxType = rs.int("税区分"),
    categoryCode = rs.stringOpt("商品分類コード"),
    wideUseType = rs.intOpt("雑区分"),
    stockManageType = rs.int("在庫管理対象区分"),
    stockReserveType = rs.intOpt("在庫引当区分"),
    supCode = rs.stringOpt("仕入先コード"),
    supSubNo = rs.intOpt("仕入先枝番"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名"),
  )

}
