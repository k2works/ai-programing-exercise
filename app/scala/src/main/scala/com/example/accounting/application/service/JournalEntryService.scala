package com.example.accounting.application.service

import com.example.accounting.application.*
import com.example.accounting.application.port.in.*
import com.example.accounting.application.port.out.{EventStoreRepository, JournalReadModelRepository}
import com.example.accounting.domain.eventsourcing.*
import scalikejdbc.*

import java.time.Instant
import java.util.UUID

/**
 * 仕訳エントリサービス（JournalEntryUseCase の実装）
 *
 * イベントソーシングパターンを使用したコマンドハンドラー
 */
class JournalEntryService(
    eventStoreRepository: EventStoreRepository,
    journalReadModelRepository: JournalReadModelRepository,
    snapshotInterval: Int = 10, // スナップショット保存間隔（イベント数）
) extends JournalEntryUseCase:

  private val projectionService = new JournalProjectionService(eventStoreRepository, journalReadModelRepository)

  /**
   * 必要に応じてスナップショットを保存
   */
  private def saveSnapshotIfNeeded(aggregate: JournalEntryAggregate)(implicit session: DBSession): Unit =
    if aggregate.version > 0 && aggregate.version % snapshotInterval == 0 then
      eventStoreRepository.saveSnapshot(aggregate)

  override def createJournalEntry(command: CreateJournalEntryCommand): Either[AppError, JournalEntryAggregate] =
    try
      // バリデーション
      validateCreateCommand(command) match
        case Left(error) => Left(error)
        case Right(_) =>
          val aggregateId = UUID.randomUUID().toString
          val now = Instant.now()

          val event = JournalEntryCreatedEvent(
            aggregateId = aggregateId,
            version = 1,
            timestamp = now,
            journalDate = command.journalDate,
            description = command.description,
            details = command.details,
            createdBy = command.createdBy,
          )

          DB.localTx { implicit session =>
            eventStoreRepository.append(aggregateId, event)
            projectionService.applyEvent(event)
            val aggregate = JournalEntryAggregate.empty(aggregateId).apply(event)
            saveSnapshotIfNeeded(aggregate)
            Right(aggregate)
          }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳の作成に失敗しました", Some(e)))

  override def approveJournalEntry(command: ApproveJournalEntryCommand): Either[AppError, JournalEntryAggregate] =
    try
      DB.localTx { implicit session =>
        eventStoreRepository.loadAggregate(command.aggregateId) match
          case None =>
            Left(NotFoundError(s"仕訳が見つかりません: ${command.aggregateId}"))
          case Some(aggregate) =>
            if !aggregate.canApprove then
              Left(ValidationError(s"この仕訳は承認できません（現在のステータス: ${aggregate.status}）"))
            else
              val now = Instant.now()
              val event = JournalEntryApprovedEvent(
                aggregateId = command.aggregateId,
                version = aggregate.version + 1,
                timestamp = now,
                approvedBy = command.approvedBy,
                approvedAt = now,
              )

              eventStoreRepository.append(command.aggregateId, event)
              projectionService.applyEvent(event)
              val updated = aggregate.apply(event)
              saveSnapshotIfNeeded(updated)
              Right(updated)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳の承認に失敗しました", Some(e)))

  override def rejectJournalEntry(command: RejectJournalEntryCommand): Either[AppError, JournalEntryAggregate] =
    try
      DB.localTx { implicit session =>
        eventStoreRepository.loadAggregate(command.aggregateId) match
          case None =>
            Left(NotFoundError(s"仕訳が見つかりません: ${command.aggregateId}"))
          case Some(aggregate) =>
            if !aggregate.canReject then
              Left(ValidationError(s"この仕訳は却下できません（現在のステータス: ${aggregate.status}）"))
            else
              val now = Instant.now()
              val event = JournalEntryRejectedEvent(
                aggregateId = command.aggregateId,
                version = aggregate.version + 1,
                timestamp = now,
                rejectedBy = command.rejectedBy,
                reason = command.reason,
              )

              eventStoreRepository.append(command.aggregateId, event)
              projectionService.applyEvent(event)
              val updated = aggregate.apply(event)
              saveSnapshotIfNeeded(updated)
              Right(updated)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳の却下に失敗しました", Some(e)))

  override def deleteJournalEntry(command: DeleteJournalEntryCommand): Either[AppError, JournalEntryAggregate] =
    try
      DB.localTx { implicit session =>
        eventStoreRepository.loadAggregate(command.aggregateId) match
          case None =>
            Left(NotFoundError(s"仕訳が見つかりません: ${command.aggregateId}"))
          case Some(aggregate) =>
            if !aggregate.canDelete then
              Left(ValidationError(s"この仕訳は削除できません（現在のステータス: ${aggregate.status}）"))
            else
              val now = Instant.now()
              val event = JournalEntryDeletedEvent(
                aggregateId = command.aggregateId,
                version = aggregate.version + 1,
                timestamp = now,
                deletedBy = command.deletedBy,
                reason = command.reason,
              )

              eventStoreRepository.append(command.aggregateId, event)
              projectionService.applyEvent(event)
              val updated = aggregate.apply(event)
              saveSnapshotIfNeeded(updated)
              Right(updated)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳の削除に失敗しました", Some(e)))

  override def getJournalEntry(aggregateId: String): Either[AppError, JournalEntryAggregate] =
    try
      DB.readOnly { implicit session =>
        eventStoreRepository.loadAggregate(aggregateId) match
          case None            => Left(NotFoundError(s"仕訳が見つかりません: $aggregateId"))
          case Some(aggregate) => Right(aggregate)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳の取得に失敗しました", Some(e)))

  override def getJournalEntryAt(aggregateId: String, asOf: Instant): Either[AppError, JournalEntryAggregate] =
    try
      DB.readOnly { implicit session =>
        val events = eventStoreRepository.getEventsUntil(aggregateId, asOf)
        if events.isEmpty then Left(NotFoundError(s"仕訳が見つかりません: $aggregateId"))
        else
          JournalEntryAggregate.replay(events) match
            case Left(error)     => Left(InternalError(error))
            case Right(aggregate) => Right(aggregate)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳の取得に失敗しました", Some(e)))

  override def getJournalEntryHistory(aggregateId: String): Either[AppError, List[JournalEntryEvent]] =
    try
      DB.readOnly { implicit session =>
        val events = eventStoreRepository.getEvents(aggregateId)
        if events.isEmpty then Left(NotFoundError(s"仕訳が見つかりません: $aggregateId"))
        else Right(events)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳履歴の取得に失敗しました", Some(e)))

  private def validateCreateCommand(command: CreateJournalEntryCommand): Either[AppError, Unit] =
    if command.description.isEmpty then
      Left(ValidationError("摘要は必須です"))
    else if command.details.isEmpty then
      Left(ValidationError("仕訳明細は1件以上必要です"))
    else
      // 貸借平衡チェック
      val totalDebit = command.details.filter(_.debitCredit == "D").map(_.amount).sum
      val totalCredit = command.details.filter(_.debitCredit == "C").map(_.amount).sum
      if totalDebit != totalCredit then
        Left(InvalidJournalError(s"貸借が一致していません（借方: $totalDebit, 貸方: $totalCredit）"))
      else
        Right(())
