namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 3層構造の仕訳テーブルにCHECK制約を追加するマイグレーション
/// </summary>
[<Migration(20250121008L)>]
type Migration_20250121_008_AddJournalConstraints() =
    inherit Migration()

    override this.Up() =
        // 貸借区分のCHECK制約
        this.Execute.Sql("""
            ALTER TABLE "仕訳貸借明細"
              ADD CONSTRAINT check_debit_credit
              CHECK ("仕訳行貸借区分" IN ('D', 'C'))
        """) |> ignore

        // 金額の妥当性CHECK制約
        this.Execute.Sql("""
            ALTER TABLE "仕訳貸借明細"
              ADD CONSTRAINT check_amount_range
              CHECK ("仕訳金額" >= -999999999999.99 AND "仕訳金額" <= 999999999999.99)
        """) |> ignore

        // 決算仕訳フラグのCHECK制約
        this.Execute.Sql("""
            ALTER TABLE "仕訳"
              ADD CONSTRAINT check_settlement_flag
              CHECK ("決算仕訳フラグ" IN (0, 1))
        """) |> ignore

        // 赤伝フラグのCHECK制約
        this.Execute.Sql("""
            ALTER TABLE "仕訳"
              ADD CONSTRAINT check_red_slip_flag
              CHECK ("赤伝フラグ" IN (0, 1))
        """) |> ignore

        // 赤伝票の場合は赤黒伝票番号が必須
        this.Execute.Sql("""
            ALTER TABLE "仕訳"
              ADD CONSTRAINT check_red_slip_voucher
              CHECK (
                ("赤伝フラグ" = 0)
                OR
                ("赤伝フラグ" = 1 AND "赤黒伝票番号" IS NOT NULL)
              )
        """) |> ignore

        // 通貨コードのCHECK制約（ISO 4217）
        this.Execute.Sql("""
            ALTER TABLE "仕訳貸借明細"
              ADD CONSTRAINT check_currency_code
              CHECK (LENGTH("通貨コード") = 3)
        """) |> ignore

        // 為替レートの妥当性
        this.Execute.Sql("""
            ALTER TABLE "仕訳貸借明細"
              ADD CONSTRAINT check_exchange_rate
              CHECK ("為替レート" > 0 AND "為替レート" <= 99999999.99)
        """) |> ignore

        // 単振フラグのCHECK制約
        this.Execute.Sql("""
            ALTER TABLE "仕訳"
              ADD CONSTRAINT check_single_entry_flag
              CHECK ("単振フラグ" IN (0, 1))
        """) |> ignore

        // 定期計上フラグのCHECK制約
        this.Execute.Sql("""
            ALTER TABLE "仕訳"
              ADD CONSTRAINT check_recurring_flag
              CHECK ("定期計上フラグ" IN (0, 1))
        """) |> ignore

        // 資金繰フラグのCHECK制約
        this.Execute.Sql("""
            ALTER TABLE "仕訳貸借明細"
              ADD CONSTRAINT check_cash_flow_flag
              CHECK ("資金繰フラグ" IN (0, 1))
        """) |> ignore

    override this.Down() =
        // 制約の削除（ロールバック用）
        this.Execute.Sql("""ALTER TABLE "仕訳貸借明細" DROP CONSTRAINT IF EXISTS check_debit_credit""") |> ignore
        this.Execute.Sql("""ALTER TABLE "仕訳貸借明細" DROP CONSTRAINT IF EXISTS check_amount_range""") |> ignore
        this.Execute.Sql("""ALTER TABLE "仕訳" DROP CONSTRAINT IF EXISTS check_settlement_flag""") |> ignore
        this.Execute.Sql("""ALTER TABLE "仕訳" DROP CONSTRAINT IF EXISTS check_red_slip_flag""") |> ignore
        this.Execute.Sql("""ALTER TABLE "仕訳" DROP CONSTRAINT IF EXISTS check_red_slip_voucher""") |> ignore
        this.Execute.Sql("""ALTER TABLE "仕訳貸借明細" DROP CONSTRAINT IF EXISTS check_currency_code""") |> ignore
        this.Execute.Sql("""ALTER TABLE "仕訳貸借明細" DROP CONSTRAINT IF EXISTS check_exchange_rate""") |> ignore
        this.Execute.Sql("""ALTER TABLE "仕訳" DROP CONSTRAINT IF EXISTS check_single_entry_flag""") |> ignore
        this.Execute.Sql("""ALTER TABLE "仕訳" DROP CONSTRAINT IF EXISTS check_recurring_flag""") |> ignore
        this.Execute.Sql("""ALTER TABLE "仕訳貸借明細" DROP CONSTRAINT IF EXISTS check_cash_flow_flag""") |> ignore
