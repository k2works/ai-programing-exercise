namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 複式簿記チェック用のビューと関数を作成するマイグレーション
/// データベースレベルで複式簿記の原理を保証
/// </summary>
[<Migration(20250121010L)>]
type Migration_20250121_010_CreateDoubleEntryCheck() =
    inherit Migration()

    override this.Up() =
        // 仕訳残高チェックビュー
        this.Execute.Sql("""
            CREATE OR REPLACE VIEW "仕訳残高チェック" AS
            SELECT
              "仕訳伝票番号",
              SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) AS "借方合計",
              SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) AS "貸方合計",
              SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) -
              SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) AS "差額"
            FROM "仕訳貸借明細"
            GROUP BY "仕訳伝票番号"
        """) |> ignore

        // 複式簿記チェック関数
        this.Execute.Sql("""
            CREATE OR REPLACE FUNCTION "複式簿記チェック"()
            RETURNS TABLE("不整合伝票番号" VARCHAR(10), "差額" DECIMAL) AS $$
            BEGIN
              RETURN QUERY
              SELECT "仕訳伝票番号", ("借方合計" - "貸方合計") as "差額"
              FROM "仕訳残高チェック"
              WHERE "借方合計" != "貸方合計";
            END;
            $$ LANGUAGE plpgsql
        """) |> ignore

        // 2層構造（仕訳エントリ + 仕訳明細）用の残高チェックビュー
        this.Execute.Sql("""
            CREATE OR REPLACE VIEW "仕訳明細残高チェック" AS
            SELECT
              "伝票番号",
              SUM("借方金額") AS "借方合計",
              SUM("貸方金額") AS "貸方合計",
              SUM("借方金額") - SUM("貸方金額") AS "差額"
            FROM "仕訳明細"
            GROUP BY "伝票番号"
        """) |> ignore

        // 2層構造用の複式簿記チェック関数
        this.Execute.Sql("""
            CREATE OR REPLACE FUNCTION "複式簿記チェック2層"()
            RETURNS TABLE("不整合伝票番号" VARCHAR(10), "差額" DECIMAL) AS $$
            BEGIN
              RETURN QUERY
              SELECT "伝票番号", ("借方合計" - "貸方合計") as "差額"
              FROM "仕訳明細残高チェック"
              WHERE "借方合計" != "貸方合計";
            END;
            $$ LANGUAGE plpgsql
        """) |> ignore

    override this.Down() =
        this.Execute.Sql("""DROP FUNCTION IF EXISTS "複式簿記チェック2層"()""") |> ignore
        this.Execute.Sql("""DROP VIEW IF EXISTS "仕訳明細残高チェック" """) |> ignore
        this.Execute.Sql("""DROP FUNCTION IF EXISTS "複式簿記チェック"()""") |> ignore
        this.Execute.Sql("""DROP VIEW IF EXISTS "仕訳残高チェック" """) |> ignore
