package com.example.accounting.application.service

import com.example.accounting.application.port.out.{EventStoreRepository, JournalReadModelRepository}
import com.example.accounting.domain.eventsourcing.*
import scalikejdbc.*

/**
 * 仕訳プロジェクションサービス
 *
 * イベントストアから Read Model へのプロジェクション（投影）を行う
 * CQRS パターンにおける Write Model → Read Model の同期処理
 */
class JournalProjectionService(
    eventStoreRepository: EventStoreRepository,
    journalReadModelRepository: JournalReadModelRepository,
):

  /**
   * 単一イベントを Read Model に反映
   */
  def applyEvent(event: JournalEntryEvent)(implicit session: DBSession): Unit =
    event match
      case e: JournalEntryCreatedEvent  => handleCreated(e)
      case e: JournalEntryApprovedEvent => handleApproved(e)
      case e: JournalEntryRejectedEvent => handleRejected(e)
      case e: JournalEntryDeletedEvent  => handleDeleted(e)

  /**
   * 集約の全イベントを再投影
   */
  def rebuildProjection(aggregateId: String)(implicit session: DBSession): Unit =
    eventStoreRepository.loadAggregate(aggregateId) match
      case Some(aggregate) => updateReadModel(aggregate)
      case None            => journalReadModelRepository.delete(aggregateId)

  /**
   * 全集約の Read Model を再構築
   */
  def rebuildAllProjections()(implicit session: DBSession): Unit =
    val aggregateIds = eventStoreRepository.getAllAggregateIds("JournalEntry")
    aggregateIds.foreach(rebuildProjection)

  private def handleCreated(event: JournalEntryCreatedEvent)(implicit session: DBSession): Unit =
    val model = JournalReadModel(
      aggregateId = event.aggregateId,
      journalDate = event.journalDate,
      description = event.description,
      totalDebit = event.details.filter(_.debitCredit == "D").map(_.amount).sum,
      totalCredit = event.details.filter(_.debitCredit == "C").map(_.amount).sum,
      status = "DRAFT",
      createdAt = event.timestamp,
      approvedAt = None,
      approvedBy = None,
      version = event.version,
    )
    journalReadModelRepository.save(model)

  private def handleApproved(event: JournalEntryApprovedEvent)(implicit session: DBSession): Unit =
    journalReadModelRepository.findById(event.aggregateId).foreach { model =>
      val updated = model.copy(
        status = "APPROVED",
        approvedAt = Some(event.approvedAt),
        approvedBy = Some(event.approvedBy),
        version = event.version,
      )
      journalReadModelRepository.save(updated)
    }

  private def handleRejected(event: JournalEntryRejectedEvent)(implicit session: DBSession): Unit =
    journalReadModelRepository.findById(event.aggregateId).foreach { model =>
      val updated = model.copy(
        status = "REJECTED",
        version = event.version,
      )
      journalReadModelRepository.save(updated)
    }

  private def handleDeleted(event: JournalEntryDeletedEvent)(implicit session: DBSession): Unit =
    journalReadModelRepository.findById(event.aggregateId).foreach { model =>
      val updated = model.copy(
        status = "DELETED",
        version = event.version,
      )
      journalReadModelRepository.save(updated)
    }

  private def updateReadModel(aggregate: JournalEntryAggregate)(implicit session: DBSession): Unit =
    if aggregate.status == JournalStatus.Deleted then
      // 削除された場合も履歴として保持
      val model = JournalReadModel(
        aggregateId = aggregate.id,
        journalDate = aggregate.journalDate.getOrElse(java.time.LocalDate.now()),
        description = aggregate.description.getOrElse(""),
        totalDebit = aggregate.totalDebit,
        totalCredit = aggregate.totalCredit,
        status = aggregate.status.toString.toUpperCase,
        createdAt = aggregate.createdAt.getOrElse(java.time.Instant.now()),
        approvedAt = aggregate.approvedAt,
        approvedBy = aggregate.approvedBy,
        version = aggregate.version,
      )
      journalReadModelRepository.save(model)
    else
      aggregate.journalDate.foreach { journalDate =>
        val model = JournalReadModel(
          aggregateId = aggregate.id,
          journalDate = journalDate,
          description = aggregate.description.getOrElse(""),
          totalDebit = aggregate.totalDebit,
          totalCredit = aggregate.totalCredit,
          status = aggregate.status.toString.toUpperCase,
          createdAt = aggregate.createdAt.getOrElse(java.time.Instant.now()),
          approvedAt = aggregate.approvedAt,
          approvedBy = aggregate.approvedBy,
          version = aggregate.version,
        )
        journalReadModelRepository.save(model)
      }
