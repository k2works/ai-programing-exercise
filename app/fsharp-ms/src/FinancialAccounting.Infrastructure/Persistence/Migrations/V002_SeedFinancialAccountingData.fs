namespace FinancialAccounting.Infrastructure.Persistence.Migrations

open System
open FluentMigrator

/// <summary>
/// 財務会計システムのシードデータ投入
/// 勘定科目マスタと初期仕訳データを作成
/// </summary>
[<Migration(2L)>]
type V002_SeedFinancialAccountingData() =
    inherit Migration()

    override this.Up() =
        // ========================================
        // 勘定科目マスタのシードデータ
        // ========================================

        // 資産科目 (Asset)
        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "1110"
                "account_name", box "現金預金"
                "account_name_kana", box "ゲンキンヨキン"
                "account_type", box "Asset"
                "is_summary_account", box false
                "bs_pl_type", box "B"
                "transaction_element_type", box "Debit"
                "expense_type", box null
                "display_order", box 10
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 1133270000.00m
            ]
        ) |> ignore

        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "1120"
                "account_name", box "売掛金"
                "account_name_kana", box "ウリカケキン"
                "account_type", box "Asset"
                "is_summary_account", box false
                "bs_pl_type", box "B"
                "transaction_element_type", box "Debit"
                "expense_type", box null
                "display_order", box 20
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 864915000.00m
            ]
        ) |> ignore

        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "1130"
                "account_name", box "商品"
                "account_name_kana", box "ショウヒン"
                "account_type", box "Asset"
                "is_summary_account", box false
                "bs_pl_type", box "B"
                "transaction_element_type", box "Debit"
                "expense_type", box null
                "display_order", box 30
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 325000000.00m
            ]
        ) |> ignore

        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "1200"
                "account_name", box "有形固定資産"
                "account_name_kana", box "ユウケイコテイシサン"
                "account_type", box "Asset"
                "is_summary_account", box true
                "bs_pl_type", box "B"
                "transaction_element_type", box "Debit"
                "expense_type", box null
                "display_order", box 100
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 500000000.00m
            ]
        ) |> ignore

        // 負債科目 (Liability)
        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "2110"
                "account_name", box "買掛金"
                "account_name_kana", box "カイカケキン"
                "account_type", box "Liability"
                "is_summary_account", box false
                "bs_pl_type", box "B"
                "transaction_element_type", box "Credit"
                "expense_type", box null
                "display_order", box 200
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 432500000.00m
            ]
        ) |> ignore

        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "2120"
                "account_name", box "未払金"
                "account_name_kana", box "ミハライキン"
                "account_type", box "Liability"
                "is_summary_account", box false
                "bs_pl_type", box "B"
                "transaction_element_type", box "Credit"
                "expense_type", box null
                "display_order", box 210
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 150000000.00m
            ]
        ) |> ignore

        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "2200"
                "account_name", box "長期借入金"
                "account_name_kana", box "チョウキカリイレキン"
                "account_type", box "Liability"
                "is_summary_account", box false
                "bs_pl_type", box "B"
                "transaction_element_type", box "Credit"
                "expense_type", box null
                "display_order", box 300
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 800000000.00m
            ]
        ) |> ignore

        // 純資産科目 (Equity)
        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "3110"
                "account_name", box "資本金"
                "account_name_kana", box "シホンキン"
                "account_type", box "Equity"
                "is_summary_account", box false
                "bs_pl_type", box "B"
                "transaction_element_type", box "Credit"
                "expense_type", box null
                "display_order", box 400
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 500000000.00m
            ]
        ) |> ignore

        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "3120"
                "account_name", box "利益剰余金"
                "account_name_kana", box "リエキジョウヨキン"
                "account_type", box "Equity"
                "is_summary_account", box false
                "bs_pl_type", box "B"
                "transaction_element_type", box "Credit"
                "expense_type", box null
                "display_order", box 410
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 940685000.00m
            ]
        ) |> ignore

        // 収益科目 (Revenue)
        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "4110"
                "account_name", box "売上高"
                "account_name_kana", box "ウリアゲダカ"
                "account_type", box "Revenue"
                "is_summary_account", box false
                "bs_pl_type", box "P"
                "transaction_element_type", box "Credit"
                "expense_type", box null
                "display_order", box 500
                "is_aggregation_target", box true
                "tax_code", box "10"
                "balance", box 4547908000.00m
            ]
        ) |> ignore

        // 費用科目 (Expense)
        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "5110"
                "account_name", box "売上原価"
                "account_name_kana", box "ウリアゲゲンカ"
                "account_type", box "Expense"
                "is_summary_account", box false
                "bs_pl_type", box "P"
                "transaction_element_type", box "Debit"
                "expense_type", box "売上原価"
                "display_order", box 600
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 3185535000.00m
            ]
        ) |> ignore

        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "5210"
                "account_name", box "販売費及び一般管理費"
                "account_name_kana", box "ハンバイヒオヨビイッパンカンリヒ"
                "account_type", box "Expense"
                "is_summary_account", box true
                "bs_pl_type", box "P"
                "transaction_element_type", box "Debit"
                "expense_type", box "販管費"
                "display_order", box 700
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 907582000.00m
            ]
        ) |> ignore

        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "5211"
                "account_name", box "人件費"
                "account_name_kana", box "ジンケンヒ"
                "account_type", box "Expense"
                "is_summary_account", box false
                "bs_pl_type", box "P"
                "transaction_element_type", box "Debit"
                "expense_type", box "販管費"
                "display_order", box 710
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 450000000.00m
            ]
        ) |> ignore

        this.Insert.IntoTable("accounts").Row(
            dict [
                "account_code", box "5212"
                "account_name", box "減価償却費"
                "account_name_kana", box "ゲンカショウキャクヒ"
                "account_type", box "Expense"
                "is_summary_account", box false
                "bs_pl_type", box "P"
                "transaction_element_type", box "Debit"
                "expense_type", box "販管費"
                "display_order", box 720
                "is_aggregation_target", box true
                "tax_code", box null
                "balance", box 100000000.00m
            ]
        ) |> ignore

        // ========================================
        // 仕訳データのシードデータ（2024年度）
        // ========================================

        // 仕訳1: 売上計上
        this.Insert.IntoTable("journals").Row(
            dict [
                "journal_date", box (DateTime(2024, 4, 1))
                "description", box "2024年度期首売上計上"
                "fiscal_year", box 2024
            ]
        ) |> ignore

        this.Execute.Sql("""
            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            SELECT journal_id, '1120', 1000000.00, 0.00, '売掛金計上'
            FROM journals WHERE description = '2024年度期首売上計上';

            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            SELECT journal_id, '4110', 0.00, 1000000.00, '売上高計上'
            FROM journals WHERE description = '2024年度期首売上計上';
        """) |> ignore

        // 仕訳2: 仕入計上
        this.Insert.IntoTable("journals").Row(
            dict [
                "journal_date", box (DateTime(2024, 4, 5))
                "description", box "商品仕入"
                "fiscal_year", box 2024
            ]
        ) |> ignore

        this.Execute.Sql("""
            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            SELECT journal_id, '1130', 500000.00, 0.00, '商品仕入'
            FROM journals WHERE description = '商品仕入';

            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            SELECT journal_id, '2110', 0.00, 500000.00, '買掛金計上'
            FROM journals WHERE description = '商品仕入';
        """) |> ignore

        // 仕訳3: 給与支払
        this.Insert.IntoTable("journals").Row(
            dict [
                "journal_date", box (DateTime(2024, 4, 25))
                "description", box "4月度給与支払"
                "fiscal_year", box 2024
            ]
        ) |> ignore

        this.Execute.Sql("""
            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            SELECT journal_id, '5211', 2000000.00, 0.00, '人件費'
            FROM journals WHERE description = '4月度給与支払';

            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            SELECT journal_id, '1110', 0.00, 2000000.00, '現金預金支払'
            FROM journals WHERE description = '4月度給与支払';
        """) |> ignore

        // 仕訳4: 売掛金回収
        this.Insert.IntoTable("journals").Row(
            dict [
                "journal_date", box (DateTime(2024, 4, 30))
                "description", box "売掛金回収"
                "fiscal_year", box 2024
            ]
        ) |> ignore

        this.Execute.Sql("""
            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            SELECT journal_id, '1110', 800000.00, 0.00, '現金預金入金'
            FROM journals WHERE description = '売掛金回収';

            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            SELECT journal_id, '1120', 0.00, 800000.00, '売掛金回収'
            FROM journals WHERE description = '売掛金回収';
        """) |> ignore

        // 仕訳5: 買掛金支払
        this.Insert.IntoTable("journals").Row(
            dict [
                "journal_date", box (DateTime(2024, 5, 10))
                "description", box "買掛金支払"
                "fiscal_year", box 2024
            ]
        ) |> ignore

        this.Execute.Sql("""
            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            SELECT journal_id, '2110', 300000.00, 0.00, '買掛金支払'
            FROM journals WHERE description = '買掛金支払';

            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            SELECT journal_id, '1110', 0.00, 300000.00, '現金預金支払'
            FROM journals WHERE description = '買掛金支払';
        """) |> ignore

    override this.Down() =
        this.Delete.FromTable("journal_entries").AllRows() |> ignore
        this.Delete.FromTable("journals").AllRows() |> ignore
        this.Delete.FromTable("accounts").AllRows() |> ignore
