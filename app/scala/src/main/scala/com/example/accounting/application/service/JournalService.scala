package com.example.accounting.application.service

import com.example.accounting.application.*
import com.example.accounting.application.port.in.JournalUseCase
import com.example.accounting.application.port.out.{AccountRepository, JournalRepository}
import com.example.accounting.domain.journal.*
import scalikejdbc.*

import java.time.LocalDate

/**
 * 仕訳サービス（JournalUseCase の実装）
 */
class JournalService(
    journalRepository: JournalRepository,
    accountRepository: AccountRepository,
) extends JournalUseCase:

  override def getJournal(
      journalNo: String
  ): Either[AppError, (Journal, List[JournalDetail], List[JournalDebitCreditDetail])] =
    try
      DB.readOnly { implicit session =>
        journalRepository.findByNo(journalNo) match
          case Some(journal) =>
            val details = journalRepository.findDetailsByNo(journalNo)
            val debitCreditDetails = journalRepository.findDebitCreditDetailsByNo(journalNo)
            Right((journal, details, debitCreditDetails))
          case None =>
            Left(NotFoundError(s"仕訳が見つかりません: $journalNo"))
      }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳の取得に失敗しました", Some(e)))

  override def getJournalsByDateRange(
      from: LocalDate,
      to: LocalDate,
  ): Either[AppError, List[(Journal, List[JournalDetail], List[JournalDebitCreditDetail])]] =
    try
      DB.readOnly { implicit session =>
        val journals = journalRepository.findByDateRange(from, to)
        val result = journals.map { journal =>
          val details = journalRepository.findDetailsByNo(journal.journalNo)
          val debitCreditDetails = journalRepository.findDebitCreditDetailsByNo(journal.journalNo)
          (journal, details, debitCreditDetails)
        }
        Right(result)
      }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳の取得に失敗しました", Some(e)))

  override def createJournal(
      journal: Journal,
      details: List[JournalDetail],
      debitCreditDetails: List[JournalDebitCreditDetail],
  ): Either[AppError, (Journal, List[JournalDetail], List[JournalDebitCreditDetail])] =
    try
      DB.localTx { implicit session =>
        // 重複チェック
        journalRepository.findByNo(journal.journalNo) match
          case Some(_) =>
            Left(DuplicateError(s"仕訳伝票番号 ${journal.journalNo} は既に存在します"))
          case None =>
            // バリデーション
            validateJournal(journal, details, debitCreditDetails) match
              case Left(error) => Left(error)
              case Right(_) =>
                // 仕訳を登録
                journalRepository.insertJournal(journal)

                // 明細を登録
                details.foreach(journalRepository.insertDetail)

                // 貸借明細を登録
                debitCreditDetails.foreach(journalRepository.insertDebitCreditDetail)

                // 登録結果を取得
                journalRepository.findByNo(journal.journalNo) match
                  case Some(created) =>
                    val createdDetails = journalRepository.findDetailsByNo(journal.journalNo)
                    val createdDCDetails = journalRepository.findDebitCreditDetailsByNo(journal.journalNo)
                    Right((created, createdDetails, createdDCDetails))
                  case None =>
                    Left(InternalError("仕訳の作成後の取得に失敗しました"))
      }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳の作成に失敗しました", Some(e)))

  override def deleteJournal(journalNo: String): Either[AppError, Unit] =
    try
      DB.localTx { implicit session =>
        journalRepository.findByNo(journalNo) match
          case None =>
            Left(NotFoundError(s"仕訳が見つかりません: $journalNo"))
          case Some(_) =>
            journalRepository.delete(journalNo)
            Right(())
      }
    catch
      case e: Exception =>
        Left(DatabaseError("仕訳の削除に失敗しました", Some(e)))

  override def validateBalance(journalNo: String): Either[AppError, (Boolean, BigDecimal, BigDecimal)] =
    try
      DB.readOnly { implicit session =>
        journalRepository.findByNo(journalNo) match
          case None =>
            Left(NotFoundError(s"仕訳が見つかりません: $journalNo"))
          case Some(_) =>
            journalRepository.validateBalance(journalNo) match
              case Some(result) => Right(result)
              case None         => Left(InternalError("貸借平衡の検証に失敗しました"))
      }
    catch
      case e: Exception =>
        Left(DatabaseError("貸借平衡の検証に失敗しました", Some(e)))

  private def validateJournal(
      journal: Journal,
      details: List[JournalDetail],
      debitCreditDetails: List[JournalDebitCreditDetail],
  )(implicit session: DBSession): Either[AppError, Unit] =
    // 仕訳伝票番号チェック
    if journal.journalNo.isEmpty then return Left(ValidationError("仕訳伝票番号は必須です"))

    // 明細チェック
    if details.isEmpty then return Left(ValidationError("仕訳明細は1件以上必要です"))

    // 貸借明細チェック
    if debitCreditDetails.isEmpty then return Left(ValidationError("仕訳貸借明細は1件以上必要です"))

    // 貸借平衡チェック
    val totalDebit = debitCreditDetails.filter(_.isDebit).map(_.amount).sum
    val totalCredit = debitCreditDetails.filter(_.isCredit).map(_.amount).sum
    if totalDebit != totalCredit then
      return Left(InvalidJournalError(s"貸借が一致していません（借方: $totalDebit, 貸方: $totalCredit）"))

    // 勘定科目の存在チェック
    val invalidCodes = debitCreditDetails.filterNot { detail =>
      accountRepository.findByCode(detail.accountCode).isDefined
    }
    if invalidCodes.nonEmpty then
      val codes = invalidCodes.map(_.accountCode).mkString(", ")
      return Left(ValidationError(s"存在しない勘定科目コード: $codes"))

    Right(())
