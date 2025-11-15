package infrastructure.repository

import infrastructure.entity.AutoNumber
import scalikejdbc._
import java.time.LocalDateTime

/**
 * 自動採番データのリポジトリ
 */
trait AutoNumberRepository {
  def create(autoNumber: AutoNumber)(implicit session: DBSession): Int

  def findById(slipType: String, yearMonth: LocalDateTime)(implicit
    session: DBSession
  ): Option[AutoNumber]

  def getNextNumber(slipType: String, yearMonth: LocalDateTime)(implicit session: DBSession): Int
  def delete(slipType: String, yearMonth: LocalDateTime)(implicit session: DBSession): Int
}

/**
 * AutoNumberRepository の実装
 */
class AutoNumberRepositoryImpl extends AutoNumberRepository {

  override def create(autoNumber: AutoNumber)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO 自動採番 (伝票種別, 年月, 最終伝票番号)
      VALUES (${autoNumber.slipType}, ${autoNumber.yearMonth}, ${autoNumber.lastSlipNo})
    """.update.apply()

  override def findById(slipType: String, yearMonth: LocalDateTime)(implicit
    session: DBSession
  ): Option[AutoNumber] =
    sql"""
      SELECT 伝票種別, 年月, 最終伝票番号
      FROM 自動採番
      WHERE 伝票種別 = $slipType AND 年月 = $yearMonth
    """.map(AutoNumber.apply).single.apply()

  override def getNextNumber(slipType: String, yearMonth: LocalDateTime)(implicit
    session: DBSession
  ): Int = {
    // FOR UPDATE で排他ロック
    val autoNumber = sql"""
      SELECT 伝票種別, 年月, 最終伝票番号
      FROM 自動採番
      WHERE 伝票種別 = $slipType AND 年月 = $yearMonth
      FOR UPDATE
    """.map(AutoNumber.apply).single.apply()

    autoNumber match {
      case Some(an) =>
        // 番号をインクリメント
        val newNo = an.lastSlipNo + 1

        // 更新
        sql"""
          UPDATE 自動採番 SET 最終伝票番号 = $newNo
          WHERE 伝票種別 = $slipType AND 年月 = $yearMonth
        """.update.apply()

        newNo

      case None =>
        // 初回は新規作成して 1 を返す
        val newAutoNumber = AutoNumber(
          slipType = slipType,
          yearMonth = yearMonth,
          lastSlipNo = 1,
        )
        create(newAutoNumber)
        1
    }
  }

  override def delete(slipType: String, yearMonth: LocalDateTime)(implicit
    session: DBSession
  ): Int =
    sql"""
      DELETE FROM 自動採番
      WHERE 伝票種別 = $slipType AND 年月 = $yearMonth
    """.update.apply()

}

object AutoNumberRepository {
  def apply(): AutoNumberRepository = new AutoNumberRepositoryImpl()
}
