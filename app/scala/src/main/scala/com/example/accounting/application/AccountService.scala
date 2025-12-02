package com.example.accounting.application

import com.example.accounting.domain.{Account, AccountType}
import com.example.accounting.infrastructure.persistence.AccountRepository
import scalikejdbc.*

/**
 * 勘定科目サービス
 */
class AccountService(repository: AccountRepository):

  /**
   * すべての勘定科目を取得
   */
  def getAllAccounts(): Either[AppError, List[Account]] =
    try
      DB.readOnly { implicit session =>
        Right(repository.findAll())
      }
    catch
      case e: Exception =>
        Left(DatabaseError("勘定科目の取得に失敗しました", Some(e)))

  /**
   * 勘定科目コードで取得
   */
  def getAccountByCode(code: String): Either[AppError, Account] =
    try
      DB.readOnly { implicit session =>
        repository.findByCode(code) match
          case Some(account) => Right(account)
          case None          => Left(NotFoundError(s"勘定科目が見つかりません: $code"))
      }
    catch
      case e: Exception =>
        Left(DatabaseError("勘定科目の取得に失敗しました", Some(e)))

  /**
   * 勘定科目を作成
   */
  def createAccount(account: Account): Either[AppError, Account] =
    try
      DB.localTx { implicit session =>
        // 重複チェック
        repository.findByCode(account.accountCode) match
          case Some(_) =>
            Left(DuplicateError(s"勘定科目コード ${account.accountCode} は既に存在します"))
          case None =>
            // バリデーション
            validateAccount(account) match
              case Left(error) => Left(error)
              case Right(_) =>
                repository.insert(account)
                repository.findByCode(account.accountCode) match
                  case Some(created) => Right(created)
                  case None          => Left(InternalError("勘定科目の作成後の取得に失敗しました"))
      }
    catch
      case e: Exception =>
        Left(DatabaseError("勘定科目の作成に失敗しました", Some(e)))

  /**
   * 勘定科目を更新
   */
  def updateAccount(code: String, account: Account): Either[AppError, Account] =
    try
      DB.localTx { implicit session =>
        // 存在チェック
        repository.findByCode(code) match
          case None =>
            Left(NotFoundError(s"勘定科目が見つかりません: $code"))
          case Some(_) =>
            // バリデーション
            validateAccount(account) match
              case Left(error) => Left(error)
              case Right(_) =>
                repository.update(account)
                repository.findByCode(code) match
                  case Some(updated) => Right(updated)
                  case None          => Left(InternalError("勘定科目の更新後の取得に失敗しました"))
      }
    catch
      case e: Exception =>
        Left(DatabaseError("勘定科目の更新に失敗しました", Some(e)))

  /**
   * 勘定科目を削除
   */
  def deleteAccount(code: String): Either[AppError, Unit] =
    try
      DB.localTx { implicit session =>
        repository.findByCode(code) match
          case None =>
            Left(NotFoundError(s"勘定科目が見つかりません: $code"))
          case Some(_) =>
            repository.delete(code)
            Right(())
      }
    catch
      case e: Exception =>
        Left(DatabaseError("勘定科目の削除に失敗しました", Some(e)))

  /**
   * 勘定科目種別で検索
   */
  def getAccountsByType(accountType: AccountType): Either[AppError, List[Account]] =
    try
      DB.readOnly { implicit session =>
        Right(repository.findByType(accountType))
      }
    catch
      case e: Exception =>
        Left(DatabaseError("勘定科目の検索に失敗しました", Some(e)))

  /**
   * バリデーション
   */
  private def validateAccount(account: Account): Either[AppError, Unit] =
    if account.accountCode.isEmpty then Left(ValidationError("勘定科目コードは必須です"))
    else if account.accountName.isEmpty then Left(ValidationError("勘定科目名は必須です"))
    else Right(())
