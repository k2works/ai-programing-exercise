package com.example.accounting.application.port.out

import com.example.accounting.domain.eventsourcing.{JournalEntryAggregate, JournalEntryEvent}
import scalikejdbc.DBSession

import java.time.Instant

/**
 * イベントストアリポジトリ（Output Port）
 */
trait EventStoreRepository:

  /**
   * イベントを追加
   */
  def append(aggregateId: String, event: JournalEntryEvent)(implicit session: DBSession): Unit

  /**
   * 集約のすべてのイベントを取得
   */
  def getEvents(aggregateId: String)(implicit session: DBSession): List[JournalEntryEvent]

  /**
   * 指定日時までのイベントを取得
   */
  def getEventsUntil(aggregateId: String, until: Instant)(implicit session: DBSession): List[JournalEntryEvent]

  /**
   * 指定バージョン以降のイベントを取得
   */
  def getEventsAfterVersion(aggregateId: String, afterVersion: Long)(implicit session: DBSession): List[JournalEntryEvent]

  /**
   * 集約種別ごとの全集約IDを取得
   */
  def getAllAggregateIds(aggregateType: String)(implicit session: DBSession): List[String]

  /**
   * スナップショットを保存
   */
  def saveSnapshot(aggregate: JournalEntryAggregate)(implicit session: DBSession): Unit

  /**
   * スナップショットを取得
   */
  def loadSnapshot(aggregateId: String)(implicit session: DBSession): Option[JournalEntryAggregate]

  /**
   * 集約を復元（スナップショット + 差分イベント）
   */
  def loadAggregate(aggregateId: String)(implicit session: DBSession): Option[JournalEntryAggregate]
