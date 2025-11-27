using FluentMigrator;

namespace FinancialAccounting.Infrastructure.Persistence.Migrations;

/// <summary>
/// シードデータの投入マイグレーション
/// </summary>
[Migration(99)]
public class V099_SeedFinancialAccountingData : Migration
{
    public override void Up()
    {
        // 勘定科目マスタのシードデータ
        // 資産
        InsertAccount("1110", "現金預金", "資産", 1133270000.00m);
        InsertAccount("1120", "売掛金", "資産", 864915000.00m);
        InsertAccount("1130", "有価証券", "資産", 50000000.00m);
        InsertAccount("1140", "棚卸資産", "資産", 325450000.00m);
        InsertAccount("1210", "建物", "資産", 580000000.00m);
        InsertAccount("1220", "機械装置", "資産", 420000000.00m);
        InsertAccount("1230", "土地", "資産", 750000000.00m);

        // 負債
        InsertAccount("2110", "買掛金", "負債", 523680000.00m);
        InsertAccount("2120", "短期借入金", "負債", 200000000.00m);
        InsertAccount("2210", "長期借入金", "負債", 800000000.00m);

        // 純資産
        InsertAccount("3110", "資本金", "純資産", 1000000000.00m);
        InsertAccount("3120", "資本剰余金", "純資産", 500000000.00m);
        InsertAccount("3130", "利益剰余金", "純資産", 1099955000.00m);

        // 収益
        InsertAccount("4110", "売上高", "収益", 0.00m);
        InsertAccount("4120", "受取利息", "収益", 0.00m);

        // 費用
        InsertAccount("5110", "売上原価", "費用", 0.00m);
        InsertAccount("5210", "販売費及び一般管理費", "費用", 0.00m);
        InsertAccount("5220", "支払利息", "費用", 0.00m);

        // サンプル仕訳データ
        // 売上計上仕訳 (2024年度)
        Execute.Sql(@"
            INSERT INTO journals (journal_date, description, fiscal_year, created_at, updated_at)
            VALUES ('2024-04-01', '売上計上', 2024, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
        ");
        Execute.Sql(@"
            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            VALUES
                ((SELECT MAX(journal_id) FROM journals), '1120', 4547908000.00, 0.00, '売掛金増加'),
                ((SELECT MAX(journal_id) FROM journals), '4110', 0.00, 4547908000.00, '売上高');
        ");

        // 仕入計上仕訳
        Execute.Sql(@"
            INSERT INTO journals (journal_date, description, fiscal_year, created_at, updated_at)
            VALUES ('2024-04-05', '仕入計上', 2024, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
        ");
        Execute.Sql(@"
            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            VALUES
                ((SELECT MAX(journal_id) FROM journals), '5110', 3183535600.00, 0.00, '売上原価'),
                ((SELECT MAX(journal_id) FROM journals), '2110', 0.00, 3183535600.00, '買掛金増加');
        ");

        // 経費支払仕訳
        Execute.Sql(@"
            INSERT INTO journals (journal_date, description, fiscal_year, created_at, updated_at)
            VALUES ('2024-04-10', '経費支払', 2024, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
        ");
        Execute.Sql(@"
            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            VALUES
                ((SELECT MAX(journal_id) FROM journals), '5210', 909581600.00, 0.00, '販管費'),
                ((SELECT MAX(journal_id) FROM journals), '1110', 0.00, 909581600.00, '現金支払');
        ");
    }

    public override void Down()
    {
        Delete.FromTable("journal_entries").AllRows();
        Delete.FromTable("journals").AllRows();
        Delete.FromTable("accounts").AllRows();
    }

    private void InsertAccount(string code, string name, string type, decimal balance)
    {
        Insert.IntoTable("accounts").Row(new
        {
            account_code = code,
            account_name = name,
            account_type = type,
            balance = balance,
            created_at = DateTime.UtcNow,
            updated_at = DateTime.UtcNow
        });
    }
}
