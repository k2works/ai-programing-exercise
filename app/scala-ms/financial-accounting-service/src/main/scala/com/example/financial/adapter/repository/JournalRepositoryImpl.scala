package com.example.financial.adapter.repository

import cats.effect.IO
import scalikejdbc.*
import com.example.financial.application.JournalRepository
import com.example.financial.domain.{Journal, JournalEntry}
import com.example.common.domain.*
import java.time.LocalDateTime

/**
 * 仕訳リポジトリの ScalikeJDBC 実装
 */
class JournalRepositoryImpl extends JournalRepository:

  override def findAll(fiscalYear: FiscalYear): IO[List[Journal]] = IO {
    DB.readOnly { implicit session =>
      val journals = sql"""
        SELECT
          journal_id,
          journal_date,
          description,
          fiscal_year,
          created_at,
          updated_at
        FROM journals
        WHERE fiscal_year = ${fiscalYear.value}
        ORDER BY journal_date, journal_id
      """
        .map(rs => mapToJournalWithoutEntries(rs))
        .list
        .apply()

      // 各仕訳の明細を取得
      journals.map { journal =>
        val entries = findEntriesByJournalId(journal.journalId.get)
        journal.copy(entries = entries)
      }
    }
  }

  override def findById(journalId: Long): IO[Option[Journal]] = IO {
    DB.readOnly { implicit session =>
      sql"""
        SELECT
          journal_id,
          journal_date,
          description,
          fiscal_year,
          created_at,
          updated_at
        FROM journals
        WHERE journal_id = $journalId
      """
        .map(rs => mapToJournalWithoutEntries(rs))
        .single
        .apply()
        .map { journal =>
          val entries = findEntriesByJournalId(journalId)
          journal.copy(entries = entries)
        }
    }
  }

  override def save(journal: Journal): IO[Journal] = IO {
    DB.localTx { implicit session =>
      val now = LocalDateTime.now()

      // 仕訳ヘッダーを保存
      val journalId = sql"""
        INSERT INTO journals (
          journal_date,
          description,
          fiscal_year,
          created_at,
          updated_at
        ) VALUES (
          ${journal.journalDate},
          ${journal.description},
          ${journal.fiscalYear.value},
          $now,
          $now
        )
      """
        .updateAndReturnGeneratedKey
        .apply()

      // 仕訳明細を保存
      val savedEntries = journal.entries.map { entry =>
        val entryId = sql"""
          INSERT INTO journal_entries (
            journal_id,
            account_code,
            debit_amount,
            credit_amount,
            description
          ) VALUES (
            $journalId,
            ${entry.accountCode.value},
            ${entry.debitAmount.value},
            ${entry.creditAmount.value},
            ${entry.description}
          )
        """
          .updateAndReturnGeneratedKey
          .apply()

        entry.copy(entryId = Some(entryId), journalId = Some(journalId))
      }

      journal.copy(
        journalId = Some(journalId),
        entries = savedEntries,
        createdAt = now,
        updatedAt = now
      )
    }
  }

  override def delete(journalId: Long): IO[Boolean] = IO {
    DB.localTx { implicit session =>
      // 明細を先に削除
      sql"""DELETE FROM journal_entries WHERE journal_id = $journalId"""
        .update
        .apply()

      // ヘッダーを削除
      val deleted = sql"""DELETE FROM journals WHERE journal_id = $journalId"""
        .update
        .apply()

      deleted > 0
    }
  }

  private def findEntriesByJournalId(journalId: Long)(implicit session: DBSession): List[JournalEntry] =
    sql"""
      SELECT
        entry_id,
        journal_id,
        account_code,
        debit_amount,
        credit_amount,
        description
      FROM journal_entries
      WHERE journal_id = $journalId
      ORDER BY entry_id
    """
      .map(rs => mapToJournalEntry(rs))
      .list
      .apply()

  private def mapToJournalWithoutEntries(rs: WrappedResultSet): Journal =
    Journal(
      journalId = Some(rs.long("journal_id")),
      journalDate = rs.localDate("journal_date"),
      description = rs.string("description"),
      fiscalYear = FiscalYear.unsafeFrom(rs.int("fiscal_year")),
      entries = List.empty,
      createdAt = rs.localDateTime("created_at"),
      updatedAt = rs.localDateTime("updated_at")
    )

  private def mapToJournalEntry(rs: WrappedResultSet): JournalEntry =
    JournalEntry(
      entryId = Some(rs.long("entry_id")),
      journalId = Some(rs.long("journal_id")),
      accountCode = AccountCode.unsafeFrom(rs.string("account_code")),
      debitAmount = Money(rs.bigDecimal("debit_amount")),
      creditAmount = Money(rs.bigDecimal("credit_amount")),
      description = rs.string("description")
    )
