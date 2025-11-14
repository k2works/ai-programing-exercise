package com.example.repository

import com.example.domain.Company
import scalikejdbc._

/**
 * 取引先マスタのリポジトリ
 */
trait CompanyRepository {
  def create(company: Company)(implicit session: DBSession): Int
  def findById(compCode: String)(implicit session: DBSession): Option[Company]
  def findAll()(implicit session: DBSession): List[Company]
  def findByGroup(compGroupCode: String)(implicit session: DBSession): List[Company]
  def update(company: Company)(implicit session: DBSession): Int
  def delete(compCode: String)(implicit session: DBSession): Int
}

/**
 * CompanyRepository の実装
 */
class CompanyRepositoryImpl extends CompanyRepository {

  override def create(company: Company)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 取引先マスタ (
        取引先コード, 取引先名, 取引先名カナ, 仕入先区分,
        郵便番号, 都道府県, 住所１, 住所２,
        取引禁止フラグ, 雑区分, 取引先グループコード,
        与信限度額, 与信一時増加枠,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${company.compCode}, ${company.name}, ${company.kana}, ${company.supType},
        ${company.zipCode}, ${company.state}, ${company.address1}, ${company.address2},
        ${company.noSalesFlg}, ${company.wideUseType}, ${company.compGroupCode},
        ${company.maxCredit}, ${company.tempCreditUp},
        ${company.createDate}, ${company.creator}, ${company.updateDate}, ${company.updater}
      )
    """.update.apply()
  }

  override def findById(compCode: String)(implicit session: DBSession): Option[Company] = {
    sql"""
      SELECT 取引先コード, 取引先名, 取引先名カナ, 仕入先区分,
             郵便番号, 都道府県, 住所１, 住所２,
             取引禁止フラグ, 雑区分, 取引先グループコード,
             与信限度額, 与信一時増加枠,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 取引先マスタ
      WHERE 取引先コード = $compCode
    """.map(Company.apply).single.apply()
  }

  override def findAll()(implicit session: DBSession): List[Company] = {
    sql"""
      SELECT 取引先コード, 取引先名, 取引先名カナ, 仕入先区分,
             郵便番号, 都道府県, 住所１, 住所２,
             取引禁止フラグ, 雑区分, 取引先グループコード,
             与信限度額, 与信一時増加枠,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 取引先マスタ
      ORDER BY 取引先コード
    """.map(Company.apply).list.apply()
  }

  override def findByGroup(compGroupCode: String)(implicit session: DBSession): List[Company] = {
    sql"""
      SELECT 取引先コード, 取引先名, 取引先名カナ, 仕入先区分,
             郵便番号, 都道府県, 住所１, 住所２,
             取引禁止フラグ, 雑区分, 取引先グループコード,
             与信限度額, 与信一時増加枠,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 取引先マスタ
      WHERE 取引先グループコード = $compGroupCode
      ORDER BY 取引先コード
    """.map(Company.apply).list.apply()
  }

  override def update(company: Company)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 取引先マスタ
      SET 取引先名 = ${company.name},
          取引先名カナ = ${company.kana},
          仕入先区分 = ${company.supType},
          郵便番号 = ${company.zipCode},
          都道府県 = ${company.state},
          住所１ = ${company.address1},
          住所２ = ${company.address2},
          取引禁止フラグ = ${company.noSalesFlg},
          雑区分 = ${company.wideUseType},
          取引先グループコード = ${company.compGroupCode},
          与信限度額 = ${company.maxCredit},
          与信一時増加枠 = ${company.tempCreditUp},
          更新日時 = ${company.updateDate},
          更新者名 = ${company.updater}
      WHERE 取引先コード = ${company.compCode}
    """.update.apply()
  }

  override def delete(compCode: String)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 取引先マスタ
      WHERE 取引先コード = $compCode
    """.update.apply()
  }
}

object CompanyRepository {
  def apply(): CompanyRepository = new CompanyRepositoryImpl()
}
