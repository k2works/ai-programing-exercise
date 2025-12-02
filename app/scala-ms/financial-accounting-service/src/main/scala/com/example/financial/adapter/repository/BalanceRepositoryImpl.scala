package com.example.financial.adapter.repository

import cats.effect.IO
import scalikejdbc.*
import com.example.financial.application.BalanceRepository
import com.example.financial.domain.AccountBalance
import com.example.common.domain.*
import java.time.LocalDateTime

/**
 * 残高リポジトリの ScalikeJDBC 実装
 */
class BalanceRepositoryImpl extends BalanceRepository:

  override def findByAccountAndYear(
      accountCode: AccountCode,
      fiscalYear: FiscalYear
  ): IO[Option[AccountBalance]] = IO {
    DB.readOnly { implicit session =>
      sql"""
        SELECT
          balance_id,
          account_code,
          fiscal_year,
          debit_total,
          credit_total,
          balance,
          created_at,
          updated_at
        FROM account_balances
        WHERE account_code = ${accountCode.value}
          AND fiscal_year = ${fiscalYear.value}
      """
        .map(rs => mapToAccountBalance(rs))
        .single
        .apply()
    }
  }

  override def upsert(balance: AccountBalance): IO[AccountBalance] = IO {
    DB.localTx { implicit session =>
      val now = LocalDateTime.now()

      balance.balanceId match
        case Some(id) =>
          // 更新
          sql"""
            UPDATE account_balances SET
              debit_total = ${balance.debitTotal.value},
              credit_total = ${balance.creditTotal.value},
              balance = ${balance.balance.value},
              updated_at = $now
            WHERE balance_id = $id
          """
            .update
            .apply()
          balance.copy(updatedAt = now)

        case None =>
          // 挿入または更新（UPSERT）
          sql"""
            INSERT INTO account_balances (
              account_code,
              fiscal_year,
              debit_total,
              credit_total,
              balance,
              created_at,
              updated_at
            ) VALUES (
              ${balance.accountCode.value},
              ${balance.fiscalYear.value},
              ${balance.debitTotal.value},
              ${balance.creditTotal.value},
              ${balance.balance.value},
              $now,
              $now
            )
            ON CONFLICT (account_code, fiscal_year)
            DO UPDATE SET
              debit_total = EXCLUDED.debit_total,
              credit_total = EXCLUDED.credit_total,
              balance = EXCLUDED.balance,
              updated_at = EXCLUDED.updated_at
            RETURNING balance_id
          """
            .map(rs => rs.long("balance_id"))
            .single
            .apply()
            .map { id =>
              balance.copy(
                balanceId = Some(id),
                createdAt = now,
                updatedAt = now
              )
            }
            .getOrElse(balance)
    }
  }

  override def findAllByYear(fiscalYear: FiscalYear): IO[List[AccountBalance]] = IO {
    DB.readOnly { implicit session =>
      sql"""
        SELECT
          balance_id,
          account_code,
          fiscal_year,
          debit_total,
          credit_total,
          balance,
          created_at,
          updated_at
        FROM account_balances
        WHERE fiscal_year = ${fiscalYear.value}
        ORDER BY account_code
      """
        .map(rs => mapToAccountBalance(rs))
        .list
        .apply()
    }
  }

  private def mapToAccountBalance(rs: WrappedResultSet): AccountBalance =
    AccountBalance(
      balanceId = Some(rs.long("balance_id")),
      accountCode = AccountCode.unsafeFrom(rs.string("account_code")),
      fiscalYear = FiscalYear.unsafeFrom(rs.int("fiscal_year")),
      debitTotal = Money(rs.bigDecimal("debit_total")),
      creditTotal = Money(rs.bigDecimal("credit_total")),
      balance = Money(rs.bigDecimal("balance")),
      createdAt = rs.localDateTime("created_at"),
      updatedAt = rs.localDateTime("updated_at")
    )
