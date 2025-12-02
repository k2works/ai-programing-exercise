package com.example.accounting.infrastructure.out.persistence.eventsourcing

import com.example.accounting.application.port.out.EventStoreRepository
import com.example.accounting.domain.eventsourcing.*
import scalikejdbc.*
import spray.json.*
import DefaultJsonProtocol.*

import java.time.{Instant, LocalDate}

/**
 * イベントストアリポジトリ実装（ScalikeJDBC）
 */
class EventStoreRepositoryImpl extends EventStoreRepository:

  import EventStoreJsonProtocol.given

  override def append(aggregateId: String, event: JournalEntryEvent)(implicit session: DBSession): Unit =
    val eventType = event.getClass.getSimpleName
    val eventData = event.toJson.compactPrint

    sql"""
      INSERT INTO "イベントストア" (
        "集約ID", "集約種別", "シーケンス番号",
        "イベント種別", "イベントデータ", "発生日時"
      ) VALUES (
        ${aggregateId}, ${"JournalEntry"}, ${event.version},
        ${eventType}, ${eventData}::jsonb, ${event.timestamp}
      )
    """.update.apply()

  override def getEvents(aggregateId: String)(implicit session: DBSession): List[JournalEntryEvent] =
    sql"""
      SELECT "イベント種別", "イベントデータ"
      FROM "イベントストア"
      WHERE "集約ID" = ${aggregateId}
      ORDER BY "シーケンス番号" ASC
    """
      .map { rs =>
        val eventType = rs.string("イベント種別")
        val eventDataJson = rs.string("イベントデータ")
        parseEvent(eventType, eventDataJson)
      }
      .list
      .apply()
      .flatten

  override def getEventsUntil(aggregateId: String, until: Instant)(implicit session: DBSession): List[JournalEntryEvent] =
    sql"""
      SELECT "イベント種別", "イベントデータ"
      FROM "イベントストア"
      WHERE "集約ID" = ${aggregateId}
        AND "発生日時" <= ${until}
      ORDER BY "シーケンス番号" ASC
    """
      .map { rs =>
        val eventType = rs.string("イベント種別")
        val eventDataJson = rs.string("イベントデータ")
        parseEvent(eventType, eventDataJson)
      }
      .list
      .apply()
      .flatten

  override def getEventsAfterVersion(aggregateId: String, afterVersion: Long)(implicit session: DBSession): List[JournalEntryEvent] =
    sql"""
      SELECT "イベント種別", "イベントデータ"
      FROM "イベントストア"
      WHERE "集約ID" = ${aggregateId}
        AND "シーケンス番号" > ${afterVersion}
      ORDER BY "シーケンス番号" ASC
    """
      .map { rs =>
        val eventType = rs.string("イベント種別")
        val eventDataJson = rs.string("イベントデータ")
        parseEvent(eventType, eventDataJson)
      }
      .list
      .apply()
      .flatten

  override def getAllAggregateIds(aggregateType: String)(implicit session: DBSession): List[String] =
    sql"""
      SELECT DISTINCT "集約ID"
      FROM "イベントストア"
      WHERE "集約種別" = ${aggregateType}
    """
      .map(_.string("集約ID"))
      .list
      .apply()

  override def saveSnapshot(aggregate: JournalEntryAggregate)(implicit session: DBSession): Unit =
    val snapshotData = aggregate.toJson.compactPrint

    sql"""
      INSERT INTO "集約スナップショット" (
        "集約ID", "集約種別", "バージョン", "スナップショットデータ"
      ) VALUES (
        ${aggregate.id}, ${"JournalEntry"}, ${aggregate.version}, ${snapshotData}::jsonb
      )
      ON CONFLICT ("集約ID")
      DO UPDATE SET
        "バージョン" = ${aggregate.version},
        "スナップショットデータ" = ${snapshotData}::jsonb,
        "作成日時" = CURRENT_TIMESTAMP
    """.update.apply()

  override def loadSnapshot(aggregateId: String)(implicit session: DBSession): Option[JournalEntryAggregate] =
    sql"""
      SELECT "スナップショットデータ" FROM "集約スナップショット"
      WHERE "集約ID" = ${aggregateId}
    """
      .map { rs =>
        rs.string("スナップショットデータ").parseJson.convertTo[JournalEntryAggregate]
      }
      .single
      .apply()

  override def loadAggregate(aggregateId: String)(implicit session: DBSession): Option[JournalEntryAggregate] =
    loadSnapshot(aggregateId) match
      case Some(snapshot) =>
        val newEvents = getEventsAfterVersion(aggregateId, snapshot.version)
        Some(JournalEntryAggregate.replayFromSnapshot(snapshot, newEvents))
      case None =>
        val allEvents = getEvents(aggregateId)
        if allEvents.isEmpty then None
        else JournalEntryAggregate.replay(allEvents).toOption

  private def parseEvent(eventType: String, eventDataJson: String): Option[JournalEntryEvent] =
    try
      eventType match
        case "JournalEntryCreatedEvent"  => Some(eventDataJson.parseJson.convertTo[JournalEntryCreatedEvent])
        case "JournalEntryApprovedEvent" => Some(eventDataJson.parseJson.convertTo[JournalEntryApprovedEvent])
        case "JournalEntryRejectedEvent" => Some(eventDataJson.parseJson.convertTo[JournalEntryRejectedEvent])
        case "JournalEntryDeletedEvent"  => Some(eventDataJson.parseJson.convertTo[JournalEntryDeletedEvent])
        case _                           => None
    catch case _: Exception => None

/**
 * イベントの JSON シリアライズ/デシリアライズ用プロトコル
 */
object EventStoreJsonProtocol extends DefaultJsonProtocol:

  given instantFormat: JsonFormat[Instant] = new JsonFormat[Instant]:
    def write(instant: Instant): JsValue = JsString(instant.toString)
    def read(value: JsValue): Instant = value match
      case JsString(s) => Instant.parse(s)
      case _           => deserializationError("Instant expected")

  given localDateFormat: JsonFormat[LocalDate] = new JsonFormat[LocalDate]:
    def write(date: LocalDate): JsValue = JsString(date.toString)
    def read(value: JsValue): LocalDate = value match
      case JsString(s) => LocalDate.parse(s)
      case _           => deserializationError("LocalDate expected")

  given journalStatusFormat: JsonFormat[JournalStatus] = new JsonFormat[JournalStatus]:
    def write(status: JournalStatus): JsValue = JsString(status.toString)
    def read(value: JsValue): JournalStatus = value match
      case JsString("Draft")    => JournalStatus.Draft
      case JsString("Approved") => JournalStatus.Approved
      case JsString("Rejected") => JournalStatus.Rejected
      case JsString("Deleted")  => JournalStatus.Deleted
      case _                    => deserializationError("JournalStatus expected")

  given journalDetailDataFormat: RootJsonFormat[JournalDetailData] = jsonFormat5(JournalDetailData.apply)

  given journalEntryCreatedEventFormat: RootJsonFormat[JournalEntryCreatedEvent] = jsonFormat7(JournalEntryCreatedEvent.apply)
  given journalEntryApprovedEventFormat: RootJsonFormat[JournalEntryApprovedEvent] = jsonFormat5(JournalEntryApprovedEvent.apply)
  given journalEntryRejectedEventFormat: RootJsonFormat[JournalEntryRejectedEvent] = jsonFormat5(JournalEntryRejectedEvent.apply)
  given journalEntryDeletedEventFormat: RootJsonFormat[JournalEntryDeletedEvent] = jsonFormat5(JournalEntryDeletedEvent.apply)

  given journalEntryEventFormat: RootJsonFormat[JournalEntryEvent] = new RootJsonFormat[JournalEntryEvent]:
    def write(event: JournalEntryEvent): JsValue = event match
      case e: JournalEntryCreatedEvent  => e.toJson
      case e: JournalEntryApprovedEvent => e.toJson
      case e: JournalEntryRejectedEvent => e.toJson
      case e: JournalEntryDeletedEvent  => e.toJson

    def read(value: JsValue): JournalEntryEvent =
      deserializationError("JournalEntryEvent deserialization should use parseEvent method")

  given journalEntryAggregateFormat: RootJsonFormat[JournalEntryAggregate] = jsonFormat10(JournalEntryAggregate.apply)
