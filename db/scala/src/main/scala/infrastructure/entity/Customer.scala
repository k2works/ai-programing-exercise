package infrastructure.entity

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 顧客マスタのドメインモデル
 *
 * 取引先のうち、顧客としての詳細情報を保持する（パーティモデル）
 *
 * @param custCode 顧客コード（取引先コード）
 * @param custSubNo 顧客枝番
 * @param custType 顧客区分
 * @param arCode 請求先コード
 * @param arSubNo 請求先枝番
 * @param payerCode 回収先コード
 * @param payerSubNo 回収先枝番
 * @param name 顧客名
 * @param kana 顧客名カナ
 * @param empCode 担当社員コード
 * @param contactPerson 顧客担当者名
 * @param contactDept 顧客担当部署名
 * @param zipCode 顧客郵便番号
 * @param state 顧客都道府県
 * @param address1 顧客住所１
 * @param tel 顧客電話番号
 * @param email 顧客メールアドレス
 * @param closeDate1 顧客締日１
 * @param payMonths1 顧客支払月数１
 * @param payDate1 顧客支払日１
 * @param payMethod1 顧客支払方法１
 * @param closeDate2 顧客締日２
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class Customer(
  custCode: String,
  custSubNo: Int,
  custType: Int = 0,
  arCode: String,
  arSubNo: Int,
  payerCode: String,
  payerSubNo: Int,
  name: Option[String] = None,
  kana: Option[String] = None,
  empCode: Option[String] = None,
  contactPerson: Option[String] = None,
  contactDept: Option[String] = None,
  zipCode: Option[String] = None,
  state: Option[String] = None,
  address1: Option[String] = None,
  tel: Option[String] = None,
  email: Option[String] = None,
  closeDate1: Option[Int] = None,
  payMonths1: Option[Int] = None,
  payDate1: Option[Int] = None,
  payMethod1: Option[Int] = None,
  closeDate2: Option[Int] = None,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = "",
)

object Customer extends SQLSyntaxSupport[Customer] {
  override val tableName = "顧客マスタ"

  override val columns   = Seq(
    "顧客コード",
    "顧客枝番",
    "顧客区分",
    "請求先コード",
    "請求先枝番",
    "回収先コード",
    "回収先枝番",
    "顧客名",
    "顧客名カナ",
    "担当社員コード",
    "顧客担当者名",
    "顧客担当部署名",
    "顧客郵便番号",
    "顧客都道府県",
    "顧客住所１",
    "顧客電話番号",
    "顧客メールアドレス",
    "顧客締日１",
    "顧客支払月数１",
    "顧客支払日１",
    "顧客支払方法１",
    "顧客締日２",
    "作成日時",
    "作成者名",
    "更新日時",
    "更新者名",
  )

  def apply(rs: WrappedResultSet): Customer = Customer(
    custCode = rs.string("顧客コード"),
    custSubNo = rs.int("顧客枝番"),
    custType = rs.int("顧客区分"),
    arCode = rs.string("請求先コード"),
    arSubNo = rs.int("請求先枝番"),
    payerCode = rs.string("回収先コード"),
    payerSubNo = rs.int("回収先枝番"),
    name = rs.stringOpt("顧客名"),
    kana = rs.stringOpt("顧客名カナ"),
    empCode = rs.stringOpt("担当社員コード"),
    contactPerson = rs.stringOpt("顧客担当者名"),
    contactDept = rs.stringOpt("顧客担当部署名"),
    zipCode = rs.stringOpt("顧客郵便番号"),
    state = rs.stringOpt("顧客都道府県"),
    address1 = rs.stringOpt("顧客住所１"),
    tel = rs.stringOpt("顧客電話番号"),
    email = rs.stringOpt("顧客メールアドレス"),
    closeDate1 = rs.intOpt("顧客締日１"),
    payMonths1 = rs.intOpt("顧客支払月数１"),
    payDate1 = rs.intOpt("顧客支払日１"),
    payMethod1 = rs.intOpt("顧客支払方法１"),
    closeDate2 = rs.intOpt("顧客締日２"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名"),
  )

}
