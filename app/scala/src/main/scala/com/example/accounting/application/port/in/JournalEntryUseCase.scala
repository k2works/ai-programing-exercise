package com.example.accounting.application.port.in

import com.example.accounting.application.AppError
import com.example.accounting.domain.eventsourcing.{JournalEntryAggregate, JournalEntryEvent}

import java.time.Instant

/**
 * 仕訳エントリユースケース（Input Port）
 *
 * イベントソーシングパターンを使用した仕訳管理のユースケース
 */
trait JournalEntryUseCase:

  /**
   * 仕訳を作成
   */
  def createJournalEntry(command: CreateJournalEntryCommand): Either[AppError, JournalEntryAggregate]

  /**
   * 仕訳を承認
   */
  def approveJournalEntry(command: ApproveJournalEntryCommand): Either[AppError, JournalEntryAggregate]

  /**
   * 仕訳を却下
   */
  def rejectJournalEntry(command: RejectJournalEntryCommand): Either[AppError, JournalEntryAggregate]

  /**
   * 仕訳を削除
   */
  def deleteJournalEntry(command: DeleteJournalEntryCommand): Either[AppError, JournalEntryAggregate]

  /**
   * 仕訳を取得
   */
  def getJournalEntry(aggregateId: String): Either[AppError, JournalEntryAggregate]

  /**
   * 仕訳を特定時点の状態で取得
   */
  def getJournalEntryAt(aggregateId: String, asOf: Instant): Either[AppError, JournalEntryAggregate]

  /**
   * 仕訳のイベント履歴を取得
   */
  def getJournalEntryHistory(aggregateId: String): Either[AppError, List[JournalEntryEvent]]
