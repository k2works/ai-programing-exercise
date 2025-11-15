package infrastructure.repository

import infrastructure.domain.Customer
import scalikejdbc._

/**
 * 顧客マスタのリポジトリ
 */
trait CustomerRepository {
  def create(customer: Customer)(implicit session: DBSession): Int
  def findById(custCode: String, custSubNo: Int)(implicit session: DBSession): Option[Customer]
  def findByCompany(custCode: String)(implicit session: DBSession): List[Customer]
  def findAll()(implicit session: DBSession): List[Customer]
  def update(customer: Customer)(implicit session: DBSession): Int
  def delete(custCode: String, custSubNo: Int)(implicit session: DBSession): Int
}

/**
 * CustomerRepository の実装
 */
class CustomerRepositoryImpl extends CustomerRepository {

  override def create(customer: Customer)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 顧客マスタ (
        顧客コード, 顧客枝番, 顧客区分,
        請求先コード, 請求先枝番, 回収先コード, 回収先枝番,
        顧客名, 顧客名カナ, 担当社員コード,
        顧客担当者名, 顧客担当部署名, 顧客郵便番号,
        顧客都道府県, 顧客住所１, 顧客電話番号, 顧客メールアドレス,
        顧客締日１, 顧客支払月数１, 顧客支払日１, 顧客支払方法１, 顧客締日２,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${customer.custCode}, ${customer.custSubNo}, ${customer.custType},
        ${customer.arCode}, ${customer.arSubNo}, ${customer.payerCode}, ${customer.payerSubNo},
        ${customer.name}, ${customer.kana}, ${customer.empCode},
        ${customer.contactPerson}, ${customer.contactDept}, ${customer.zipCode},
        ${customer.state}, ${customer.address1}, ${customer.tel}, ${customer.email},
        ${customer.closeDate1}, ${customer.payMonths1}, ${customer.payDate1}, ${customer.payMethod1}, ${customer.closeDate2},
        ${customer.createDate}, ${customer.creator}, ${customer.updateDate}, ${customer.updater}
      )
    """.update.apply()
  }

  override def findById(custCode: String, custSubNo: Int)(implicit session: DBSession): Option[Customer] = {
    sql"""
      SELECT 顧客コード, 顧客枝番, 顧客区分,
             請求先コード, 請求先枝番, 回収先コード, 回収先枝番,
             顧客名, 顧客名カナ, 担当社員コード,
             顧客担当者名, 顧客担当部署名, 顧客郵便番号,
             顧客都道府県, 顧客住所１, 顧客電話番号, 顧客メールアドレス,
             顧客締日１, 顧客支払月数１, 顧客支払日１, 顧客支払方法１, 顧客締日２,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 顧客マスタ
      WHERE 顧客コード = $custCode AND 顧客枝番 = $custSubNo
    """.map(Customer.apply).single.apply()
  }

  override def findByCompany(custCode: String)(implicit session: DBSession): List[Customer] = {
    sql"""
      SELECT 顧客コード, 顧客枝番, 顧客区分,
             請求先コード, 請求先枝番, 回収先コード, 回収先枝番,
             顧客名, 顧客名カナ, 担当社員コード,
             顧客担当者名, 顧客担当部署名, 顧客郵便番号,
             顧客都道府県, 顧客住所１, 顧客電話番号, 顧客メールアドレス,
             顧客締日１, 顧客支払月数１, 顧客支払日１, 顧客支払方法１, 顧客締日２,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 顧客マスタ
      WHERE 顧客コード = $custCode
      ORDER BY 顧客枝番
    """.map(Customer.apply).list.apply()
  }

  override def findAll()(implicit session: DBSession): List[Customer] = {
    sql"""
      SELECT 顧客コード, 顧客枝番, 顧客区分,
             請求先コード, 請求先枝番, 回収先コード, 回収先枝番,
             顧客名, 顧客名カナ, 担当社員コード,
             顧客担当者名, 顧客担当部署名, 顧客郵便番号,
             顧客都道府県, 顧客住所１, 顧客電話番号, 顧客メールアドレス,
             顧客締日１, 顧客支払月数１, 顧客支払日１, 顧客支払方法１, 顧客締日２,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 顧客マスタ
      ORDER BY 顧客コード, 顧客枝番
    """.map(Customer.apply).list.apply()
  }

  override def update(customer: Customer)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 顧客マスタ
      SET 顧客区分 = ${customer.custType},
          請求先コード = ${customer.arCode},
          請求先枝番 = ${customer.arSubNo},
          回収先コード = ${customer.payerCode},
          回収先枝番 = ${customer.payerSubNo},
          顧客名 = ${customer.name},
          顧客名カナ = ${customer.kana},
          担当社員コード = ${customer.empCode},
          顧客担当者名 = ${customer.contactPerson},
          顧客担当部署名 = ${customer.contactDept},
          顧客郵便番号 = ${customer.zipCode},
          顧客都道府県 = ${customer.state},
          顧客住所１ = ${customer.address1},
          顧客電話番号 = ${customer.tel},
          顧客メールアドレス = ${customer.email},
          顧客締日１ = ${customer.closeDate1},
          顧客支払月数１ = ${customer.payMonths1},
          顧客支払日１ = ${customer.payDate1},
          顧客支払方法１ = ${customer.payMethod1},
          顧客締日２ = ${customer.closeDate2},
          更新日時 = ${customer.updateDate},
          更新者名 = ${customer.updater}
      WHERE 顧客コード = ${customer.custCode} AND 顧客枝番 = ${customer.custSubNo}
    """.update.apply()
  }

  override def delete(custCode: String, custSubNo: Int)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 顧客マスタ
      WHERE 顧客コード = $custCode AND 顧客枝番 = $custSubNo
    """.update.apply()
  }
}

object CustomerRepository {
  def apply(): CustomerRepository = new CustomerRepositoryImpl()
}
