namespace AccountingSystem.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250121013L)>]
type Migration_20250121_013_CreateDailyAccountBalance() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("日次勘定科目残高")
            .WithColumn("起票日").AsDate().NotNullable()
            .WithColumn("勘定科目コード").AsString(10).NotNullable()
            .WithColumn("補助科目コード").AsString(10).NotNullable().WithDefaultValue("")
            .WithColumn("部門コード").AsString(5).NotNullable().WithDefaultValue("")
            .WithColumn("プロジェクトコード").AsString(10).NotNullable().WithDefaultValue("")
            .WithColumn("決算仕訳フラグ").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("借方金額").AsDecimal(15, 2).NotNullable().WithDefaultValue(0m)
            .WithColumn("貸方金額").AsDecimal(15, 2).NotNullable().WithDefaultValue(0m)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            |> ignore

        // 複合主キー
        this.Create.PrimaryKey("PK_日次勘定科目残高")
            .OnTable("日次勘定科目残高")
            .Columns("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
            |> ignore

        // 外部キー制約
        this.Create.ForeignKey("FK_日次勘定科目残高_勘定科目マスタ")
            .FromTable("日次勘定科目残高").ForeignColumn("勘定科目コード")
            .ToTable("勘定科目マスタ").PrimaryColumn("勘定科目コード")
            |> ignore

        // インデックス作成
        this.Create.Index("idx_日次勘定科目残高_起票日")
            .OnTable("日次勘定科目残高")
            .OnColumn("起票日")
            |> ignore

        this.Create.Index("idx_日次勘定科目残高_勘定科目")
            .OnTable("日次勘定科目残高")
            .OnColumn("勘定科目コード")
            |> ignore

        this.Create.Index("idx_日次勘定科目残高_部門")
            .OnTable("日次勘定科目残高")
            .OnColumn("部門コード")
            |> ignore

        this.Create.Index("idx_日次勘定科目残高_プロジェクト")
            .OnTable("日次勘定科目残高")
            .OnColumn("プロジェクトコード")
            |> ignore

        // CHECK 制約
        this.Execute.Sql("""
            ALTER TABLE "日次勘定科目残高"
              ADD CONSTRAINT "check_日次残高_借方金額"
              CHECK ("借方金額" >= 0)
        """) |> ignore

        this.Execute.Sql("""
            ALTER TABLE "日次勘定科目残高"
              ADD CONSTRAINT "check_日次残高_貸方金額"
              CHECK ("貸方金額" >= 0)
        """) |> ignore

        this.Execute.Sql("""
            ALTER TABLE "日次勘定科目残高"
              ADD CONSTRAINT "check_日次残高_決算仕訳フラグ"
              CHECK ("決算仕訳フラグ" IN (0, 1))
        """) |> ignore

        // コメント追加
        this.Execute.Sql("""
            COMMENT ON TABLE "日次勘定科目残高" IS '日次勘定科目残高（日ごとの借方・貸方金額を記録）';
            COMMENT ON COLUMN "日次勘定科目残高"."起票日" IS '実際の取引発生日';
            COMMENT ON COLUMN "日次勘定科目残高"."勘定科目コード" IS '勘定科目マスタの外部キー';
            COMMENT ON COLUMN "日次勘定科目残高"."補助科目コード" IS '補助科目（得意先、仕入先など）';
            COMMENT ON COLUMN "日次勘定科目残高"."部門コード" IS '部門別管理用';
            COMMENT ON COLUMN "日次勘定科目残高"."プロジェクトコード" IS 'プロジェクト別管理用';
            COMMENT ON COLUMN "日次勘定科目残高"."決算仕訳フラグ" IS '0=通常仕訳、1=決算仕訳';
            COMMENT ON COLUMN "日次勘定科目残高"."借方金額" IS '借方合計金額';
            COMMENT ON COLUMN "日次勘定科目残高"."貸方金額" IS '貸方合計金額';
        """) |> ignore

    override this.Down() =
        this.Delete.Table("日次勘定科目残高") |> ignore
