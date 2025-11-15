package infrastructure.repository

import infrastructure.entity.Warehouse
import scalikejdbc._

/**
 * 倉庫マスタのリポジトリ
 */
trait WarehouseRepository {
  def create(warehouse: Warehouse)(implicit session: DBSession): Int
  def findById(whCode: String)(implicit session: DBSession): Option[Warehouse]
  def findAll()(implicit session: DBSession): List[Warehouse]
  def update(warehouse: Warehouse)(implicit session: DBSession): Int
  def delete(whCode: String)(implicit session: DBSession): Int
}

/**
 * WarehouseRepository の実装
 */
class WarehouseRepositoryImpl extends WarehouseRepository {

  override def create(warehouse: Warehouse)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO 倉庫マスタ (
        倉庫コード, 倉庫名, 郵便番号, 都道府県, 住所１, 住所２,
        電話番号, FAX番号, 作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${warehouse.whCode}, ${warehouse.name}, ${warehouse.zipCode}, ${warehouse.state},
        ${warehouse.address1}, ${warehouse.address2}, ${warehouse.tel}, ${warehouse.fax},
        ${warehouse.createDate}, ${warehouse.creator}, ${warehouse.updateDate}, ${warehouse.updater}
      )
    """.update.apply()

  override def findById(whCode: String)(implicit session: DBSession): Option[Warehouse] =
    sql"""
      SELECT
        倉庫コード, 倉庫名, 郵便番号, 都道府県, 住所１, 住所２,
        電話番号, FAX番号, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 倉庫マスタ
      WHERE 倉庫コード = $whCode
    """.map(Warehouse.apply).single.apply()

  override def findAll()(implicit session: DBSession): List[Warehouse] =
    sql"""
      SELECT
        倉庫コード, 倉庫名, 郵便番号, 都道府県, 住所１, 住所２,
        電話番号, FAX番号, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 倉庫マスタ
      ORDER BY 倉庫コード
    """.map(Warehouse.apply).list.apply()

  override def update(warehouse: Warehouse)(implicit session: DBSession): Int =
    sql"""
      UPDATE 倉庫マスタ
      SET 倉庫名 = ${warehouse.name},
          郵便番号 = ${warehouse.zipCode},
          都道府県 = ${warehouse.state},
          住所１ = ${warehouse.address1},
          住所２ = ${warehouse.address2},
          電話番号 = ${warehouse.tel},
          FAX番号 = ${warehouse.fax},
          更新日時 = ${warehouse.updateDate},
          更新者名 = ${warehouse.updater}
      WHERE 倉庫コード = ${warehouse.whCode}
    """.update.apply()

  override def delete(whCode: String)(implicit session: DBSession): Int =
    sql"""
      DELETE FROM 倉庫マスタ WHERE 倉庫コード = $whCode
    """.update.apply()

}

object WarehouseRepository {
  def apply(): WarehouseRepository = new WarehouseRepositoryImpl()
}
