package com.example.financial.application

import cats.effect.IO
import com.example.financial.domain.*
import com.example.common.domain.*

/**
 * 勘定科目リポジトリのポートインターフェース
 */
trait AccountRepository:
  def findAll(): IO[List[Account]]
  def findByCode(accountCode: AccountCode): IO[Option[Account]]
  def save(account: Account): IO[Account]
  def update(account: Account): IO[Account]
  def delete(accountCode: AccountCode): IO[Boolean]

/**
 * 勘定科目サービス
 *
 * 勘定科目の CRUD 操作を提供します。
 */
class AccountService(repository: AccountRepository):

  /**
   * すべての勘定科目を取得
   */
  def findAll(): IO[List[Account]] =
    repository.findAll()

  /**
   * 勘定科目コードで検索
   */
  def findByCode(accountCode: AccountCode): IO[Either[DomainError, Account]] =
    repository.findByCode(accountCode).map {
      case Some(account) => Right(account)
      case None          => Left(NotFoundError(s"Account not found: ${accountCode.value}"))
    }

  /**
   * 新規勘定科目を作成
   */
  def create(
      accountCode: AccountCode,
      accountName: String,
      accountType: AccountType,
      isSummaryAccount: Boolean = false,
      displayOrder: Int = 0,
      isAggregationTarget: Boolean = true
  ): IO[Either[DomainError, Account]] =
    repository.findByCode(accountCode).flatMap {
      case Some(_) =>
        IO.pure(Left(ConflictError(s"Account already exists: ${accountCode.value}")))
      case None =>
        val account = Account.create(
          accountCode = accountCode,
          accountName = accountName,
          accountType = accountType,
          isSummaryAccount = isSummaryAccount,
          displayOrder = displayOrder,
          isAggregationTarget = isAggregationTarget
        )
        repository.save(account).map(Right(_))
    }

  /**
   * 勘定科目を更新
   */
  def update(
      accountCode: AccountCode,
      accountName: Option[String] = None,
      displayOrder: Option[Int] = None,
      isAggregationTarget: Option[Boolean] = None
  ): IO[Either[DomainError, Account]] =
    repository.findByCode(accountCode).flatMap {
      case None =>
        IO.pure(Left(NotFoundError(s"Account not found: ${accountCode.value}")))
      case Some(existing) =>
        val updated = existing.copy(
          accountName = accountName.getOrElse(existing.accountName),
          displayOrder = displayOrder.getOrElse(existing.displayOrder),
          isAggregationTarget = isAggregationTarget.getOrElse(existing.isAggregationTarget)
        )
        repository.update(updated).map(Right(_))
    }

  /**
   * 勘定科目を削除
   */
  def delete(accountCode: AccountCode): IO[Either[DomainError, Boolean]] =
    repository.findByCode(accountCode).flatMap {
      case None =>
        IO.pure(Left(NotFoundError(s"Account not found: ${accountCode.value}")))
      case Some(_) =>
        repository.delete(accountCode).map(Right(_))
    }
