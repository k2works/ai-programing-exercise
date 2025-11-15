package infrastructure.repository

import infrastructure.domain.Department
import scalikejdbc._

import java.time.LocalDateTime

/**
 * 部門マスタのリポジトリ
 */
trait DepartmentRepository {
  def create(dept: Department)(implicit session: DBSession): Int
  def findById(deptCode: String)(implicit session: DBSession): Option[Department]
  def findAll()(implicit session: DBSession): List[Department]
  def update(dept: Department)(implicit session: DBSession): Int
  def delete(deptCode: String)(implicit session: DBSession): Int
}

/**
 * DepartmentRepository の実装
 */
class DepartmentRepositoryImpl extends DepartmentRepository {

  override def create(dept: Department)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 部門マスタ (
        部門コード, 開始日, 終了日, 部門名, 組織階層, 部門パス,
        最下層区分, 伝票入力可否, 作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${dept.deptCode}, ${dept.startDate}, ${dept.endDate},
        ${dept.name}, ${dept.layer}, ${dept.path},
        ${dept.lowestType}, ${dept.slitYn},
        ${dept.createDate}, ${dept.creator},
        ${dept.updateDate}, ${dept.updater}
      )
    """.update.apply()
  }

  override def findById(deptCode: String)(implicit session: DBSession): Option[Department] = {
    sql"""
      SELECT 部門コード, 開始日, 終了日, 部門名, 組織階層, 部門パス,
             最下層区分, 伝票入力可否, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 部門マスタ
      WHERE 部門コード = $deptCode
    """.map(Department.apply).single.apply()
  }

  override def findAll()(implicit session: DBSession): List[Department] = {
    sql"""
      SELECT 部門コード, 開始日, 終了日, 部門名, 組織階層, 部門パス,
             最下層区分, 伝票入力可否, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 部門マスタ
      ORDER BY 部門コード, 開始日
    """.map(Department.apply).list.apply()
  }

  override def update(dept: Department)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 部門マスタ
      SET 部門名 = ${dept.name},
          組織階層 = ${dept.layer},
          部門パス = ${dept.path},
          最下層区分 = ${dept.lowestType},
          伝票入力可否 = ${dept.slitYn},
          終了日 = ${dept.endDate},
          開始日 = ${dept.startDate},
          更新日時 = ${dept.updateDate},
          更新者名 = ${dept.updater}
      WHERE 部門コード = ${dept.deptCode}
    """.update.apply()
  }

  override def delete(deptCode: String)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 部門マスタ
      WHERE 部門コード = $deptCode
    """.update.apply()
  }
}

object DepartmentRepository {
  def apply(): DepartmentRepository = new DepartmentRepositoryImpl()
}
