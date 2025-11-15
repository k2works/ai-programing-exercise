package infrastructure.entity

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 倉庫マスタのドメインモデル
 *
 * 商品を保管する倉庫の情報を管理
 *
 * @param whCode 倉庫コード
 * @param name 倉庫名
 * @param zipCode 郵便番号
 * @param state 都道府県
 * @param address1 住所1
 * @param address2 住所2
 * @param tel 電話番号
 * @param fax FAX番号
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class Warehouse(
  whCode: String,
  name: String,
  zipCode: Option[String] = None,
  state: Option[String] = None,
  address1: Option[String] = None,
  address2: Option[String] = None,
  tel: Option[String] = None,
  fax: Option[String] = None,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = "",
)

object Warehouse extends SQLSyntaxSupport[Warehouse] {
  override val tableName = "倉庫マスタ"

  override val columns   = Seq(
    "倉庫コード",
    "倉庫名",
    "郵便番号",
    "都道府県",
    "住所１",
    "住所２",
    "電話番号",
    "FAX番号",
    "作成日時",
    "作成者名",
    "更新日時",
    "更新者名",
  )

  def apply(rs: WrappedResultSet): Warehouse = Warehouse(
    whCode = rs.string("倉庫コード"),
    name = rs.string("倉庫名"),
    zipCode = rs.stringOpt("郵便番号"),
    state = rs.stringOpt("都道府県"),
    address1 = rs.stringOpt("住所１"),
    address2 = rs.stringOpt("住所２"),
    tel = rs.stringOpt("電話番号"),
    fax = rs.stringOpt("FAX番号"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名"),
  )

}
