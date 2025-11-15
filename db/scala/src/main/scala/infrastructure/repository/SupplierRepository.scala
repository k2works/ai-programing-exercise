package infrastructure.repository

import infrastructure.domain.Supplier
import scalikejdbc._

/**
 * 仕入先マスタのリポジトリ
 */
trait SupplierRepository {
  def create(supplier: Supplier)(implicit session: DBSession): Int
  def findById(supCode: String, supSubNo: Int)(implicit session: DBSession): Option[Supplier]
  def findByCompany(supCode: String)(implicit session: DBSession): List[Supplier]
  def findAll()(implicit session: DBSession): List[Supplier]
  def update(supplier: Supplier)(implicit session: DBSession): Int
  def delete(supCode: String, supSubNo: Int)(implicit session: DBSession): Int
}

/**
 * SupplierRepository の実装
 */
class SupplierRepositoryImpl extends SupplierRepository {

  override def create(supplier: Supplier)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 仕入先マスタ (
        仕入先コード, 仕入先枝番, 仕入先区分,
        仕入先名, 仕入先名カナ, 担当社員コード,
        仕入先担当者名, 仕入先担当部署名, 仕入先郵便番号,
        仕入先都道府県, 仕入先住所１, 仕入先電話番号, 仕入先メールアドレス,
        仕入先締日, 仕入先支払月数, 仕入先支払日, 仕入先支払方法,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${supplier.supCode}, ${supplier.supSubNo}, ${supplier.supType},
        ${supplier.name}, ${supplier.kana}, ${supplier.empCode},
        ${supplier.contactPerson}, ${supplier.contactDept}, ${supplier.zipCode},
        ${supplier.state}, ${supplier.address1}, ${supplier.tel}, ${supplier.email},
        ${supplier.closeDate}, ${supplier.payMonths}, ${supplier.payDate}, ${supplier.payMethod},
        ${supplier.createDate}, ${supplier.creator}, ${supplier.updateDate}, ${supplier.updater}
      )
    """.update.apply()
  }

  override def findById(supCode: String, supSubNo: Int)(implicit session: DBSession): Option[Supplier] = {
    sql"""
      SELECT 仕入先コード, 仕入先枝番, 仕入先区分,
             仕入先名, 仕入先名カナ, 担当社員コード,
             仕入先担当者名, 仕入先担当部署名, 仕入先郵便番号,
             仕入先都道府県, 仕入先住所１, 仕入先電話番号, 仕入先メールアドレス,
             仕入先締日, 仕入先支払月数, 仕入先支払日, 仕入先支払方法,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 仕入先マスタ
      WHERE 仕入先コード = $supCode AND 仕入先枝番 = $supSubNo
    """.map(Supplier.apply).single.apply()
  }

  override def findByCompany(supCode: String)(implicit session: DBSession): List[Supplier] = {
    sql"""
      SELECT 仕入先コード, 仕入先枝番, 仕入先区分,
             仕入先名, 仕入先名カナ, 担当社員コード,
             仕入先担当者名, 仕入先担当部署名, 仕入先郵便番号,
             仕入先都道府県, 仕入先住所１, 仕入先電話番号, 仕入先メールアドレス,
             仕入先締日, 仕入先支払月数, 仕入先支払日, 仕入先支払方法,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 仕入先マスタ
      WHERE 仕入先コード = $supCode
      ORDER BY 仕入先枝番
    """.map(Supplier.apply).list.apply()
  }

  override def findAll()(implicit session: DBSession): List[Supplier] = {
    sql"""
      SELECT 仕入先コード, 仕入先枝番, 仕入先区分,
             仕入先名, 仕入先名カナ, 担当社員コード,
             仕入先担当者名, 仕入先担当部署名, 仕入先郵便番号,
             仕入先都道府県, 仕入先住所１, 仕入先電話番号, 仕入先メールアドレス,
             仕入先締日, 仕入先支払月数, 仕入先支払日, 仕入先支払方法,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 仕入先マスタ
      ORDER BY 仕入先コード, 仕入先枝番
    """.map(Supplier.apply).list.apply()
  }

  override def update(supplier: Supplier)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 仕入先マスタ
      SET 仕入先区分 = ${supplier.supType},
          仕入先名 = ${supplier.name},
          仕入先名カナ = ${supplier.kana},
          担当社員コード = ${supplier.empCode},
          仕入先担当者名 = ${supplier.contactPerson},
          仕入先担当部署名 = ${supplier.contactDept},
          仕入先郵便番号 = ${supplier.zipCode},
          仕入先都道府県 = ${supplier.state},
          仕入先住所１ = ${supplier.address1},
          仕入先電話番号 = ${supplier.tel},
          仕入先メールアドレス = ${supplier.email},
          仕入先締日 = ${supplier.closeDate},
          仕入先支払月数 = ${supplier.payMonths},
          仕入先支払日 = ${supplier.payDate},
          仕入先支払方法 = ${supplier.payMethod},
          更新日時 = ${supplier.updateDate},
          更新者名 = ${supplier.updater}
      WHERE 仕入先コード = ${supplier.supCode} AND 仕入先枝番 = ${supplier.supSubNo}
    """.update.apply()
  }

  override def delete(supCode: String, supSubNo: Int)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 仕入先マスタ
      WHERE 仕入先コード = $supCode AND 仕入先枝番 = $supSubNo
    """.update.apply()
  }
}

object SupplierRepository {
  def apply(): SupplierRepository = new SupplierRepositoryImpl()
}
