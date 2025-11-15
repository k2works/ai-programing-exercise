package infrastructure.repository

import infrastructure.entity.CreditBalance
import scalikejdbc._

/**
 * 与信残高データのリポジトリ
 */
trait CreditBalanceRepository {
  def create(balance: CreditBalance)(implicit session: DBSession): Int
  def findByCompCode(compCode: String)(implicit session: DBSession): Option[CreditBalance]
  def findAll()(implicit session: DBSession): List[CreditBalance]
  def update(balance: CreditBalance)(implicit session: DBSession): Int
  def delete(compCode: String)(implicit session: DBSession): Int
}

/**
 * CreditBalanceRepository の実装
 */
class CreditBalanceRepositoryImpl extends CreditBalanceRepository {

  override def create(balance: CreditBalance)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO 与信残高 (
        取引先コード, 受注残高, 売掛残高, 買掛残高,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${balance.compCode}, ${balance.orderBalance}, ${balance.recBalance}, ${balance.payBalance},
        ${balance.createDate}, ${balance.creator}, ${balance.updateDate}, ${balance.updater}
      )
    """.update.apply()

  override def findByCompCode(compCode: String)(implicit
    session: DBSession
  ): Option[CreditBalance] =
    sql"""
      SELECT 取引先コード, 受注残高, 売掛残高, 買掛残高,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 与信残高
      WHERE 取引先コード = $compCode
    """.map(CreditBalance.apply).single.apply()

  override def findAll()(implicit session: DBSession): List[CreditBalance] =
    sql"""
      SELECT 取引先コード, 受注残高, 売掛残高, 買掛残高,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 与信残高
      ORDER BY 取引先コード
    """.map(CreditBalance.apply).list.apply()

  override def update(balance: CreditBalance)(implicit session: DBSession): Int =
    sql"""
      UPDATE 与信残高 SET
        受注残高 = ${balance.orderBalance},
        売掛残高 = ${balance.recBalance},
        買掛残高 = ${balance.payBalance},
        更新日時 = ${balance.updateDate},
        更新者名 = ${balance.updater}
      WHERE 取引先コード = ${balance.compCode}
    """.update.apply()

  override def delete(compCode: String)(implicit session: DBSession): Int =
    sql"""
      DELETE FROM 与信残高
      WHERE 取引先コード = $compCode
    """.update.apply()

}

object CreditBalanceRepository {
  def apply(): CreditBalanceRepository = new CreditBalanceRepositoryImpl()
}
