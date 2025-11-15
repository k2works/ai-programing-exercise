package infrastructure.repository

import infrastructure.domain.CompanyGroup
import scalikejdbc._

/**
 * 取引先グループマスタのリポジトリ
 */
trait CompanyGroupRepository {
  def create(group: CompanyGroup)(implicit session: DBSession): Int
  def findById(compGroupCode: String)(implicit session: DBSession): Option[CompanyGroup]
  def findAll()(implicit session: DBSession): List[CompanyGroup]
  def update(group: CompanyGroup)(implicit session: DBSession): Int
  def delete(compGroupCode: String)(implicit session: DBSession): Int
}

/**
 * CompanyGroupRepository の実装
 */
class CompanyGroupRepositoryImpl extends CompanyGroupRepository {

  override def create(group: CompanyGroup)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 取引先グループマスタ (
        取引先グループコード, 取引先グループ名,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${group.compGroupCode}, ${group.groupName},
        ${group.createDate}, ${group.creator}, ${group.updateDate}, ${group.updater}
      )
    """.update.apply()
  }

  override def findById(compGroupCode: String)(implicit session: DBSession): Option[CompanyGroup] = {
    sql"""
      SELECT 取引先グループコード, 取引先グループ名,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 取引先グループマスタ
      WHERE 取引先グループコード = $compGroupCode
    """.map(CompanyGroup.apply).single.apply()
  }

  override def findAll()(implicit session: DBSession): List[CompanyGroup] = {
    sql"""
      SELECT 取引先グループコード, 取引先グループ名,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 取引先グループマスタ
      ORDER BY 取引先グループコード
    """.map(CompanyGroup.apply).list.apply()
  }

  override def update(group: CompanyGroup)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 取引先グループマスタ
      SET 取引先グループ名 = ${group.groupName},
          更新日時 = ${group.updateDate},
          更新者名 = ${group.updater}
      WHERE 取引先グループコード = ${group.compGroupCode}
    """.update.apply()
  }

  override def delete(compGroupCode: String)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 取引先グループマスタ
      WHERE 取引先グループコード = $compGroupCode
    """.update.apply()
  }
}

object CompanyGroupRepository {
  def apply(): CompanyGroupRepository = new CompanyGroupRepositoryImpl()
}
