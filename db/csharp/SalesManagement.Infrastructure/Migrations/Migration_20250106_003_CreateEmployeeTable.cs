using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    /// <summary>
    /// 社員マスタテーブルの作成
    /// </summary>
    [Migration(20250106003)]
    public class Migration20250106003CreateEmployeeTable : Migration
    {
        public override void Up()
        {
            Create.Table("社員マスタ")
                .WithColumn("社員コード").AsString(20).NotNullable().PrimaryKey("pk_社員マスタ")
                .WithColumn("社員名").AsString(100).NotNullable()
                .WithColumn("社員名カナ").AsString(100).Nullable()
                .WithColumn("性別").AsString(1).Nullable()
                .WithColumn("生年月日").AsDate().Nullable()
                .WithColumn("入社年月日").AsDate().NotNullable()
                .WithColumn("部門コード").AsString(20).NotNullable()
                .WithColumn("役職コード").AsString(20).Nullable()
                .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("作成者名").AsString(50).NotNullable()
                .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
                .WithColumn("更新者名").AsString(50).NotNullable();

            Create.Index("idx_社員マスタ_部門コード")
                .OnTable("社員マスタ")
                .OnColumn("部門コード");

            Create.Index("idx_社員マスタ_社員名カナ")
                .OnTable("社員マスタ")
                .OnColumn("社員名カナ");

            // テーブルコメント（PostgreSQL）
            Execute.Sql("COMMENT ON TABLE 社員マスタ IS '社員の基本情報を管理するマスタ'");
            Execute.Sql("COMMENT ON COLUMN 社員マスタ.社員コード IS '社員の一意識別子'");
            Execute.Sql("COMMENT ON COLUMN 社員マスタ.社員名 IS '社員の氏名'");
            Execute.Sql("COMMENT ON COLUMN 社員マスタ.社員名カナ IS '社員の氏名（カナ）'");
            Execute.Sql("COMMENT ON COLUMN 社員マスタ.性別 IS '性別（M:男性, F:女性, O:その他）'");
            Execute.Sql("COMMENT ON COLUMN 社員マスタ.生年月日 IS '生年月日'");
            Execute.Sql("COMMENT ON COLUMN 社員マスタ.入社年月日 IS '入社年月日'");
            Execute.Sql("COMMENT ON COLUMN 社員マスタ.部門コード IS '所属部門コード'");
            Execute.Sql("COMMENT ON COLUMN 社員マスタ.役職コード IS '役職コード'");

            // CHECK制約（性別）
            Execute.Sql("ALTER TABLE 社員マスタ ADD CONSTRAINT chk_社員マスタ_性別 CHECK (性別 IN ('M', 'F', 'O'))");
        }

        public override void Down()
        {
            Delete.Table("社員マスタ");
        }
    }
}
