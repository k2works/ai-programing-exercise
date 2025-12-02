package com.example.accounting.application.port.out

import com.example.accounting.domain.eventsourcing.{JournalReadModel, JournalStatus}
import scalikejdbc.DBSession

import java.time.LocalDate

/**
 * 仕訳リードモデルリポジトリ（Output Port）
 */
trait JournalReadModelRepository:

  /**
   * リードモデルを保存（upsert）
   */
  def save(model: JournalReadModel)(implicit session: DBSession): Unit

  /**
   * リードモデルを削除
   */
  def delete(aggregateId: String)(implicit session: DBSession): Unit

  /**
   * 集約IDで取得
   */
  def findById(aggregateId: String)(implicit session: DBSession): Option[JournalReadModel]

  /**
   * 日付範囲で取得
   */
  def findByDateRange(from: LocalDate, to: LocalDate)(implicit session: DBSession): List[JournalReadModel]

  /**
   * ステータスで取得
   */
  def findByStatus(status: String)(implicit session: DBSession): List[JournalReadModel]

  /**
   * 全件取得
   */
  def findAll()(implicit session: DBSession): List[JournalReadModel]
