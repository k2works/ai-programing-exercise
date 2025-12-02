package com.example.financial.application

import cats.effect.IO
import cats.implicits.*
import com.example.financial.domain.*
import com.example.common.domain.*
import java.time.LocalDate

/**
 * 仕訳リポジトリのポートインターフェース
 */
trait JournalRepository:
  def findAll(fiscalYear: FiscalYear): IO[List[Journal]]
  def findById(journalId: Long): IO[Option[Journal]]
  def save(journal: Journal): IO[Journal]
  def delete(journalId: Long): IO[Boolean]

/**
 * 残高リポジトリのポートインターフェース
 */
trait BalanceRepository:
  def findByAccountAndYear(accountCode: AccountCode, fiscalYear: FiscalYear): IO[Option[AccountBalance]]
  def upsert(balance: AccountBalance): IO[AccountBalance]
  def findAllByYear(fiscalYear: FiscalYear): IO[List[AccountBalance]]

/**
 * 仕訳サービス
 *
 * 仕訳の作成と残高更新を行います。
 */
class JournalService(
    journalRepository: JournalRepository,
    balanceRepository: BalanceRepository,
    accountRepository: AccountRepository
):

  /**
   * 指定した会計年度の仕訳一覧を取得
   */
  def findAll(fiscalYear: FiscalYear): IO[List[Journal]] =
    journalRepository.findAll(fiscalYear)

  /**
   * 仕訳IDで検索
   */
  def findById(journalId: Long): IO[Either[DomainError, Journal]] =
    journalRepository.findById(journalId).map {
      case Some(journal) => Right(journal)
      case None          => Left(NotFoundError(s"Journal not found: $journalId"))
    }

  /**
   * 新規仕訳を作成
   *
   * 仕訳の作成と同時に、関連する勘定科目の残高を更新します。
   */
  def create(
      journalDate: LocalDate,
      description: String,
      fiscalYear: FiscalYear,
      entries: List[JournalEntryRequest]
  ): IO[Either[DomainError, Journal]] =
    // リクエストを JournalEntry に変換
    val journalEntries = entries.map { req =>
      JournalEntry(
        entryId = None,
        journalId = None,
        accountCode = req.accountCode,
        debitAmount = req.debitAmount,
        creditAmount = req.creditAmount,
        description = req.description
      )
    }

    // 仕訳を作成
    Journal.create(journalDate, description, fiscalYear, journalEntries) match
      case Left(error) =>
        IO.pure(Left(ValidationError(error)))
      case Right(journal) =>
        for
          // 勘定科目の存在確認
          accountCheck <- validateAccounts(journalEntries.map(_.accountCode))
          result <- accountCheck match
            case Left(error) => IO.pure(Left(error))
            case Right(_) =>
              for
                // 仕訳を保存
                savedJournal <- journalRepository.save(journal)
                // 残高を更新
                _ <- updateBalances(savedJournal)
              yield Right(savedJournal)
        yield result

  /**
   * 勘定科目の存在確認
   */
  private def validateAccounts(accountCodes: List[AccountCode]): IO[Either[DomainError, Unit]] =
    accountCodes.distinct
      .traverse(code => accountRepository.findByCode(code).map(opt => (code, opt)))
      .map { results =>
        val missing = results.collect { case (code, None) => code }
        if missing.isEmpty then Right(())
        else Left(ValidationError(s"Account not found: ${missing.map(_.value).mkString(", ")}"))
      }

  /**
   * 残高を更新
   */
  private def updateBalances(journal: Journal): IO[Unit] =
    journal.entries.traverse_ { entry =>
      for
        existingBalance <- balanceRepository.findByAccountAndYear(entry.accountCode, journal.fiscalYear)
        balance = existingBalance.getOrElse(AccountBalance.create(entry.accountCode, journal.fiscalYear))
        updatedBalance =
          if !entry.debitAmount.isZero then balance.addDebit(entry.debitAmount)
          else balance.addCredit(entry.creditAmount)
        _ <- balanceRepository.upsert(updatedBalance)
      yield ()
    }

  /**
   * 仕訳を削除
   */
  def delete(journalId: Long): IO[Either[DomainError, Boolean]] =
    journalRepository.findById(journalId).flatMap {
      case None =>
        IO.pure(Left(NotFoundError(s"Journal not found: $journalId")))
      case Some(_) =>
        journalRepository.delete(journalId).map(Right(_))
    }

/**
 * 仕訳明細リクエスト
 */
case class JournalEntryRequest(
    accountCode: AccountCode,
    debitAmount: Money,
    creditAmount: Money,
    description: String
)
