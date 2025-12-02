package com.example.financial.adapter.repository

import cats.effect.IO
import scalikejdbc.*
import com.example.financial.application.AccountRepository
import com.example.financial.domain.Account
import com.example.common.domain.*
import java.time.LocalDateTime

/**
 * 勘定科目リポジトリの ScalikeJDBC 実装
 */
class AccountRepositoryImpl extends AccountRepository:

  override def findAll(): IO[List[Account]] = IO {
    DB.readOnly { implicit session =>
      sql"""
        SELECT
          account_id,
          account_code,
          account_name,
          account_type,
          is_summary_account,
          display_order,
          is_aggregation_target,
          balance,
          created_at,
          updated_at
        FROM accounts
        ORDER BY display_order, account_code
      """
        .map(rs => mapToAccount(rs))
        .list
        .apply()
    }
  }

  override def findByCode(accountCode: AccountCode): IO[Option[Account]] = IO {
    DB.readOnly { implicit session =>
      sql"""
        SELECT
          account_id,
          account_code,
          account_name,
          account_type,
          is_summary_account,
          display_order,
          is_aggregation_target,
          balance,
          created_at,
          updated_at
        FROM accounts
        WHERE account_code = ${accountCode.value}
      """
        .map(rs => mapToAccount(rs))
        .single
        .apply()
    }
  }

  override def save(account: Account): IO[Account] = IO {
    DB.localTx { implicit session =>
      val now = LocalDateTime.now()
      val generatedId = sql"""
        INSERT INTO accounts (
          account_code,
          account_name,
          account_type,
          is_summary_account,
          display_order,
          is_aggregation_target,
          balance,
          created_at,
          updated_at
        ) VALUES (
          ${account.accountCode.value},
          ${account.accountName},
          ${account.accountType.toString.toLowerCase},
          ${account.isSummaryAccount},
          ${account.displayOrder},
          ${account.isAggregationTarget},
          ${account.balance.value},
          $now,
          $now
        )
      """
        .updateAndReturnGeneratedKey
        .apply()

      account.copy(
        accountId = Some(generatedId),
        createdAt = now,
        updatedAt = now
      )
    }
  }

  override def update(account: Account): IO[Account] = IO {
    DB.localTx { implicit session =>
      val now = LocalDateTime.now()
      sql"""
        UPDATE accounts SET
          account_name = ${account.accountName},
          account_type = ${account.accountType.toString.toLowerCase},
          is_summary_account = ${account.isSummaryAccount},
          display_order = ${account.displayOrder},
          is_aggregation_target = ${account.isAggregationTarget},
          balance = ${account.balance.value},
          updated_at = $now
        WHERE account_code = ${account.accountCode.value}
      """
        .update
        .apply()

      account.copy(updatedAt = now)
    }
  }

  override def delete(accountCode: AccountCode): IO[Boolean] = IO {
    DB.localTx { implicit session =>
      val deleted = sql"""
        DELETE FROM accounts WHERE account_code = ${accountCode.value}
      """
        .update
        .apply()
      deleted > 0
    }
  }

  private def mapToAccount(rs: WrappedResultSet): Account =
    Account(
      accountId = Some(rs.long("account_id")),
      accountCode = AccountCode.unsafeFrom(rs.string("account_code")),
      accountName = rs.string("account_name"),
      accountType = AccountType.fromString(rs.string("account_type")).getOrElse(AccountType.Asset),
      isSummaryAccount = rs.boolean("is_summary_account"),
      displayOrder = rs.int("display_order"),
      isAggregationTarget = rs.boolean("is_aggregation_target"),
      balance = Money(rs.bigDecimal("balance")),
      createdAt = rs.localDateTime("created_at"),
      updatedAt = rs.localDateTime("updated_at")
    )
