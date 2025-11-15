package infrastructure.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 取引先マスタのドメインモデル
 *
 * すべての取引先の基本情報を保持する
 * 顧客、仕入先などの役割に応じた詳細情報は別テーブルで管理（パーティモデル）
 *
 * @param compCode 取引先コード
 * @param name 取引先名
 * @param kana 取引先名カナ
 * @param supType 仕入先区分（0:通常、1:メーカー、2:商社）
 * @param zipCode 郵便番号
 * @param state 都道府県
 * @param address1 住所１
 * @param address2 住所２
 * @param noSalesFlg 取引禁止フラグ（0:許可、1:禁止）
 * @param wideUseType 雑区分
 * @param compGroupCode 取引先グループコード
 * @param maxCredit 与信限度額
 * @param tempCreditUp 与信一時増加枠
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class Company(
  compCode: String,
  name: String,
  kana: Option[String] = None,
  supType: Int = 0,
  zipCode: Option[String] = None,
  state: Option[String] = None,
  address1: Option[String] = None,
  address2: Option[String] = None,
  noSalesFlg: Int = 0,
  wideUseType: Int = 0,
  compGroupCode: String,
  maxCredit: Int = 0,
  tempCreditUp: Int = 0,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object Company extends SQLSyntaxSupport[Company] {
  override val tableName = "取引先マスタ"
  override val columns = Seq(
    "取引先コード", "取引先名", "取引先名カナ", "仕入先区分",
    "郵便番号", "都道府県", "住所１", "住所２",
    "取引禁止フラグ", "雑区分", "取引先グループコード",
    "与信限度額", "与信一時増加枠",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): Company = Company(
    compCode = rs.string("取引先コード"),
    name = rs.string("取引先名"),
    kana = rs.stringOpt("取引先名カナ"),
    supType = rs.int("仕入先区分"),
    zipCode = rs.stringOpt("郵便番号"),
    state = rs.stringOpt("都道府県"),
    address1 = rs.stringOpt("住所１"),
    address2 = rs.stringOpt("住所２"),
    noSalesFlg = rs.int("取引禁止フラグ"),
    wideUseType = rs.int("雑区分"),
    compGroupCode = rs.string("取引先グループコード"),
    maxCredit = rs.int("与信限度額"),
    tempCreditUp = rs.int("与信一時増加枠"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}
