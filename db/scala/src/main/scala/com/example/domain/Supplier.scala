package com.example.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 仕入先マスタのドメインモデル
 *
 * 取引先のうち、仕入先としての詳細情報を保持する（パーティモデル）
 *
 * @param supCode 仕入先コード（取引先コード）
 * @param supSubNo 仕入先枝番
 * @param supType 仕入先区分
 * @param name 仕入先名
 * @param kana 仕入先名カナ
 * @param empCode 担当社員コード
 * @param contactPerson 仕入先担当者名
 * @param contactDept 仕入先担当部署名
 * @param zipCode 仕入先郵便番号
 * @param state 仕入先都道府県
 * @param address1 仕入先住所１
 * @param tel 仕入先電話番号
 * @param email 仕入先メールアドレス
 * @param closeDate 仕入先締日
 * @param payMonths 仕入先支払月数
 * @param payDate 仕入先支払日
 * @param payMethod 仕入先支払方法
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class Supplier(
  supCode: String,
  supSubNo: Int,
  supType: Int = 0,
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
  closeDate: Option[Int] = None,
  payMonths: Option[Int] = None,
  payDate: Option[Int] = None,
  payMethod: Option[Int] = None,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object Supplier extends SQLSyntaxSupport[Supplier] {
  override val tableName = "仕入先マスタ"
  override val columns = Seq(
    "仕入先コード", "仕入先枝番", "仕入先区分",
    "仕入先名", "仕入先名カナ", "担当社員コード",
    "仕入先担当者名", "仕入先担当部署名", "仕入先郵便番号",
    "仕入先都道府県", "仕入先住所１", "仕入先電話番号", "仕入先メールアドレス",
    "仕入先締日", "仕入先支払月数", "仕入先支払日", "仕入先支払方法",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): Supplier = Supplier(
    supCode = rs.string("仕入先コード"),
    supSubNo = rs.int("仕入先枝番"),
    supType = rs.int("仕入先区分"),
    name = rs.stringOpt("仕入先名"),
    kana = rs.stringOpt("仕入先名カナ"),
    empCode = rs.stringOpt("担当社員コード"),
    contactPerson = rs.stringOpt("仕入先担当者名"),
    contactDept = rs.stringOpt("仕入先担当部署名"),
    zipCode = rs.stringOpt("仕入先郵便番号"),
    state = rs.stringOpt("仕入先都道府県"),
    address1 = rs.stringOpt("仕入先住所１"),
    tel = rs.stringOpt("仕入先電話番号"),
    email = rs.stringOpt("仕入先メールアドレス"),
    closeDate = rs.intOpt("仕入先締日"),
    payMonths = rs.intOpt("仕入先支払月数"),
    payDate = rs.intOpt("仕入先支払日"),
    payMethod = rs.intOpt("仕入先支払方法"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}
