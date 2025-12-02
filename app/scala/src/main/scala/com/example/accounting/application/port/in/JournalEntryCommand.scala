package com.example.accounting.application.port.in

import com.example.accounting.domain.eventsourcing.JournalDetailData

import java.time.LocalDate

/**
 * 仕訳エントリコマンドの基底 trait
 */
sealed trait JournalEntryCommand

/**
 * 仕訳作成コマンド
 */
case class CreateJournalEntryCommand(
    journalDate: LocalDate,
    description: String,
    details: List[JournalDetailData],
    createdBy: String,
) extends JournalEntryCommand

/**
 * 仕訳承認コマンド
 */
case class ApproveJournalEntryCommand(
    aggregateId: String,
    approvedBy: String,
) extends JournalEntryCommand

/**
 * 仕訳却下コマンド
 */
case class RejectJournalEntryCommand(
    aggregateId: String,
    rejectedBy: String,
    reason: String,
) extends JournalEntryCommand

/**
 * 仕訳削除コマンド
 */
case class DeleteJournalEntryCommand(
    aggregateId: String,
    deletedBy: String,
    reason: String,
) extends JournalEntryCommand
