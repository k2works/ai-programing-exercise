namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

[<Migration(20250106009L)>]
type Migration_20250106_009_CreateCompanyTable() =
    inherit Migration()

    override this.Up() =
        this.Create.Table("取引先マスタ")
            .WithColumn("取引先コード").AsString(8).NotNullable().PrimaryKey()
            .WithColumn("取引先名").AsString(40).NotNullable()
            .WithColumn("取引先名カナ").AsString(40).Nullable()
            .WithColumn("仕入先区分").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("郵便番号").AsFixedLengthString(8).Nullable()
            .WithColumn("都道府県").AsString(4).Nullable()
            .WithColumn("住所１").AsString(40).Nullable()
            .WithColumn("住所２").AsString(40).Nullable()
            .WithColumn("取引禁止フラグ").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("雑区分").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("取引先グループコード").AsString(4).NotNullable()
            .WithColumn("与信限度額").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("与信一時増加枠").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者名").AsString(100).NotNullable()
        |> ignore

        this.Create.ForeignKey("fk_company_group")
            .FromTable("取引先マスタ").ForeignColumn("取引先グループコード")
            .ToTable("取引先グループマスタ").PrimaryColumn("取引先グループコード")
        |> ignore

        this.Create.Index("idx_取引先マスタ_グループコード")
            .OnTable("取引先マスタ")
            .OnColumn("取引先グループコード")
        |> ignore

        this.Execute.Sql("COMMENT ON TABLE 取引先マスタ IS '顧客・仕入先の共通基盤情報（Partyモデル）'")
        this.Execute.Sql("COMMENT ON COLUMN 取引先マスタ.与信限度額 IS '与信管理の限度額'")

    override this.Down() =
        this.Delete.Table("取引先マスタ") |> ignore
