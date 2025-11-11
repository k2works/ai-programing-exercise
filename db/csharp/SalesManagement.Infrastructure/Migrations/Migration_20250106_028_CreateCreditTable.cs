using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations;

/// <summary>
/// 入金データのマイグレーション
/// </summary>
[Migration(20250106028)]
public class Migration20250106028CreateCreditTable : Migration
{
    public override void Up()
    {
        Create.Table("入金データ")
            .WithColumn("入金伝票番号").AsString(20).NotNullable().PrimaryKey()
            .WithColumn("入金日").AsDate().NotNullable()
            .WithColumn("得意先コード").AsString(10).NotNullable()
            .WithColumn("入金額").AsDecimal(18, 2).NotNullable()
            .WithColumn("入金消込金額").AsDecimal(18, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("口座コード").AsString(10).Nullable()
            .WithColumn("入金方法").AsInt32().NotNullable()
            .WithColumn("備考").AsString(200).Nullable()
            .WithColumn("部門コード").AsString(10).NotNullable()
            .WithColumn("社員コード").AsString(10).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable()
            .WithColumn("作成者名").AsString(50).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable()
            .WithColumn("更新者名").AsString(50).NotNullable();

        // 外部キー制約
        Create.ForeignKey("fk_credit_company")
            .FromTable("入金データ").ForeignColumn("得意先コード")
            .ToTable("取引先マスタ").PrimaryColumn("取引先コード");

        Create.ForeignKey("fk_credit_account")
            .FromTable("入金データ").ForeignColumn("口座コード")
            .ToTable("入金口座マスタ").PrimaryColumn("口座コード");

        // 部門マスタは複合主キー（部門コード、開始日）のため外部キー制約なし

        Create.ForeignKey("fk_credit_employee")
            .FromTable("入金データ").ForeignColumn("社員コード")
            .ToTable("社員マスタ").PrimaryColumn("社員コード");

        // インデックス
        Create.Index("idx_credit_customer")
            .OnTable("入金データ")
            .OnColumn("得意先コード");

        Create.Index("idx_credit_date")
            .OnTable("入金データ")
            .OnColumn("入金日");
    }

    public override void Down()
    {
        Delete.Table("入金データ");
    }
}
