package com.example.accounting.infrastructure.out.persistence.eventsourcing

import com.example.accounting.application.port.out.JournalReadModelRepository
import com.example.accounting.domain.eventsourcing.JournalReadModel
import scalikejdbc.*

import java.time.{Instant, LocalDate}

/**
 * 仕訳リードモデルリポジトリ実装（ScalikeJDBC）
 */
class JournalReadModelRepositoryImpl extends JournalReadModelRepository:

  override def save(model: JournalReadModel)(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "仕訳リードモデル" (
        "集約ID", "仕訳日付", "摘要", "借方合計", "貸方合計",
        "ステータス", "作成日時", "承認日時", "承認者", "バージョン"
      ) VALUES (
        ${model.aggregateId}, ${model.journalDate}, ${model.description},
        ${model.totalDebit}, ${model.totalCredit}, ${model.status},
        ${model.createdAt}, ${model.approvedAt}, ${model.approvedBy}, ${model.version}
      )
      ON CONFLICT ("集約ID")
      DO UPDATE SET
        "仕訳日付" = ${model.journalDate},
        "摘要" = ${model.description},
        "借方合計" = ${model.totalDebit},
        "貸方合計" = ${model.totalCredit},
        "ステータス" = ${model.status},
        "承認日時" = ${model.approvedAt},
        "承認者" = ${model.approvedBy},
        "バージョン" = ${model.version}
    """.update.apply()

  override def delete(aggregateId: String)(implicit session: DBSession): Unit =
    sql"""
      DELETE FROM "仕訳リードモデル"
      WHERE "集約ID" = ${aggregateId}
    """.update.apply()

  override def findById(aggregateId: String)(implicit session: DBSession): Option[JournalReadModel] =
    sql"""
      SELECT "集約ID", "仕訳日付", "摘要", "借方合計", "貸方合計",
             "ステータス", "作成日時", "承認日時", "承認者", "バージョン"
      FROM "仕訳リードモデル"
      WHERE "集約ID" = ${aggregateId}
    """.map(toModel).single.apply()

  override def findByDateRange(from: LocalDate, to: LocalDate)(implicit session: DBSession): List[JournalReadModel] =
    sql"""
      SELECT "集約ID", "仕訳日付", "摘要", "借方合計", "貸方合計",
             "ステータス", "作成日時", "承認日時", "承認者", "バージョン"
      FROM "仕訳リードモデル"
      WHERE "仕訳日付" BETWEEN ${from} AND ${to}
      ORDER BY "仕訳日付" ASC, "作成日時" ASC
    """.map(toModel).list.apply()

  override def findByStatus(status: String)(implicit session: DBSession): List[JournalReadModel] =
    sql"""
      SELECT "集約ID", "仕訳日付", "摘要", "借方合計", "貸方合計",
             "ステータス", "作成日時", "承認日時", "承認者", "バージョン"
      FROM "仕訳リードモデル"
      WHERE "ステータス" = ${status}
      ORDER BY "仕訳日付" ASC, "作成日時" ASC
    """.map(toModel).list.apply()

  override def findAll()(implicit session: DBSession): List[JournalReadModel] =
    sql"""
      SELECT "集約ID", "仕訳日付", "摘要", "借方合計", "貸方合計",
             "ステータス", "作成日時", "承認日時", "承認者", "バージョン"
      FROM "仕訳リードモデル"
      ORDER BY "仕訳日付" ASC, "作成日時" ASC
    """.map(toModel).list.apply()

  private def toModel(rs: WrappedResultSet): JournalReadModel =
    JournalReadModel(
      aggregateId = rs.string("集約ID"),
      journalDate = rs.localDate("仕訳日付"),
      description = rs.string("摘要"),
      totalDebit = rs.bigDecimal("借方合計"),
      totalCredit = rs.bigDecimal("貸方合計"),
      status = rs.string("ステータス"),
      createdAt = rs.timestamp("作成日時").toInstant,
      approvedAt = rs.timestampOpt("承認日時").map(_.toInstant),
      approvedBy = rs.stringOpt("承認者"),
      version = rs.long("バージョン"),
    )
