package com.example.accounting.application.port.in

import com.example.accounting.application.AppError
import com.example.accounting.domain.account.{Account, AccountType}

/**
 * 勘定科目ユースケース（Input Port）
 */
trait AccountUseCase:

  /** すべての勘定科目を取得 */
  def getAllAccounts(): Either[AppError, List[Account]]

  /** 勘定科目コードで取得 */
  def getAccountByCode(code: String): Either[AppError, Account]

  /** 勘定科目を作成 */
  def createAccount(account: Account): Either[AppError, Account]

  /** 勘定科目を更新 */
  def updateAccount(code: String, account: Account): Either[AppError, Account]

  /** 勘定科目を削除 */
  def deleteAccount(code: String): Either[AppError, Unit]

  /** 勘定科目種別で検索 */
  def getAccountsByType(accountType: AccountType): Either[AppError, List[Account]]
