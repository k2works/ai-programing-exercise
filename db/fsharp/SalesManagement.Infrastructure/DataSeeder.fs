module SalesManagement.Infrastructure.DataSeeder

open System
open System.Threading.Tasks
open Npgsql
open Dapper

/// 既存データのクリア
let private clearExistingDataAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "既存データのクリア..."

        // 全テーブルを存在チェック付きで削除（外部キー制約の順序を考慮）
        let sql = """
            DO $$
            BEGIN
                -- トランザクションデータ（子テーブル → 親テーブルの順）
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '請求明細') THEN DELETE FROM 請求明細; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '請求ヘッダ') THEN DELETE FROM 請求ヘッダ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '入庫明細') THEN DELETE FROM 入庫明細; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '入庫ヘッダ') THEN DELETE FROM 入庫ヘッダ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '仕入明細') THEN DELETE FROM 仕入明細; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '仕入ヘッダ') THEN DELETE FROM 仕入ヘッダ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '発注明細') THEN DELETE FROM 発注明細; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '発注ヘッダ') THEN DELETE FROM 発注ヘッダ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '在庫データ') THEN DELETE FROM 在庫データ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '売上明細') THEN DELETE FROM 売上明細; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '売上ヘッダ') THEN DELETE FROM 売上ヘッダ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '受注明細') THEN DELETE FROM 受注明細; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '受注ヘッダ') THEN DELETE FROM 受注ヘッダ; END IF;

                -- 管理系マスタ
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '与信残高データ') THEN DELETE FROM 与信残高データ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '自動採番マスタ') THEN DELETE FROM 自動採番マスタ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '入金口座マスタ') THEN DELETE FROM 入金口座マスタ; END IF;

                -- 商品・在庫マスタ
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '倉庫マスタ') THEN DELETE FROM 倉庫マスタ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '商品マスタ') THEN DELETE FROM 商品マスタ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '商品分類マスタ') THEN DELETE FROM 商品分類マスタ; END IF;

                -- 取引先マスタ（子テーブル → 親テーブルの順）
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '仕入先マスタ') THEN DELETE FROM 仕入先マスタ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '顧客マスタ') THEN DELETE FROM 顧客マスタ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '取引先マスタ') THEN DELETE FROM 取引先マスタ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '取引先グループマスタ') THEN DELETE FROM 取引先グループマスタ; END IF;

                -- 組織マスタ
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '社員マスタ') THEN DELETE FROM 社員マスタ; END IF;
                IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '部門マスタ') THEN DELETE FROM 部門マスタ; END IF;
            END $$;
        """

        do! connection.ExecuteAsync(sql, transaction = transaction) :> Task
        printfn "既存データのクリア完了"
    }

/// 部門マスタの投入
let private seedDepartmentsAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "部門マスタの投入..."

        let now = DateTime.Now
        let startDate = DateTime(2000, 1, 1)
        let endDate = DateTime(9999, 12, 31)

        let departments = [
            // 本社
            {| Code = "000000"; StartDate = startDate; EndDate = endDate; Name = "本社"; Layer = 1; Path = "/000000"; LowestLayer = 1s; SlipEntry = 0s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}

            // 食肉製造・販売事業
            {| Code = "100000"; StartDate = startDate; EndDate = endDate; Name = "食肉製造・販売事業"; Layer = 2; Path = "/000000/100000"; LowestLayer = 0s; SlipEntry = 0s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "110000"; StartDate = startDate; EndDate = endDate; Name = "食肉加工部門"; Layer = 3; Path = "/000000/100000/110000"; LowestLayer = 0s; SlipEntry = 0s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "111000"; StartDate = startDate; EndDate = endDate; Name = "牛肉・豚肉・鶏肉課"; Layer = 4; Path = "/000000/100000/110000/111000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "112000"; StartDate = startDate; EndDate = endDate; Name = "食肉加工品課"; Layer = 4; Path = "/000000/100000/110000/112000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "120000"; StartDate = startDate; EndDate = endDate; Name = "小売販売部門"; Layer = 3; Path = "/000000/100000/120000"; LowestLayer = 0s; SlipEntry = 0s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "121000"; StartDate = startDate; EndDate = endDate; Name = "直営小売店課"; Layer = 4; Path = "/000000/100000/120000/121000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "122000"; StartDate = startDate; EndDate = endDate; Name = "百貨店・スーパー向け販売課"; Layer = 4; Path = "/000000/100000/120000/122000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "130000"; StartDate = startDate; EndDate = endDate; Name = "新規取引先開拓部門"; Layer = 3; Path = "/000000/100000/130000"; LowestLayer = 0s; SlipEntry = 0s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "131000"; StartDate = startDate; EndDate = endDate; Name = "ホテル・旅館向け課"; Layer = 4; Path = "/000000/100000/130000/131000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "132000"; StartDate = startDate; EndDate = endDate; Name = "飲食店向け課"; Layer = 4; Path = "/000000/100000/130000/132000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}

            // 食肉加工品事業
            {| Code = "200000"; StartDate = startDate; EndDate = endDate; Name = "食肉加工品事業"; Layer = 2; Path = "/000000/200000"; LowestLayer = 0s; SlipEntry = 0s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "210000"; StartDate = startDate; EndDate = endDate; Name = "自社ブランド部門"; Layer = 3; Path = "/000000/200000/210000"; LowestLayer = 0s; SlipEntry = 0s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "211000"; StartDate = startDate; EndDate = endDate; Name = "贈答用製品製造課"; Layer = 4; Path = "/000000/200000/210000/211000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "212000"; StartDate = startDate; EndDate = endDate; Name = "道の駅・土産物製品販売課"; Layer = 4; Path = "/000000/200000/210000/212000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "220000"; StartDate = startDate; EndDate = endDate; Name = "相手先ブランド製造(OEM)部門"; Layer = 3; Path = "/000000/200000/220000"; LowestLayer = 0s; SlipEntry = 0s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "221000"; StartDate = startDate; EndDate = endDate; Name = "客先要望対応課"; Layer = 4; Path = "/000000/200000/220000/221000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}

            // コンサルティング事業
            {| Code = "300000"; StartDate = startDate; EndDate = endDate; Name = "コンサルティング事業"; Layer = 2; Path = "/000000/300000"; LowestLayer = 0s; SlipEntry = 0s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "310000"; StartDate = startDate; EndDate = endDate; Name = "顧客対応部門"; Layer = 3; Path = "/000000/300000/310000"; LowestLayer = 0s; SlipEntry = 0s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "311000"; StartDate = startDate; EndDate = endDate; Name = "メニュー提案課"; Layer = 4; Path = "/000000/300000/310000/311000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "312000"; StartDate = startDate; EndDate = endDate; Name = "半加工商品提供課"; Layer = 4; Path = "/000000/300000/310000/312000"; LowestLayer = 1s; SlipEntry = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
        ]

        let sql = """
            INSERT INTO 部門マスタ (
                部門コード, 開始日, 終了日, 部門名, 組織階層, 部門パス, 最下層区分, 伝票入力可否,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @StartDate, @EndDate, @Name, @Layer, @Path, @LowestLayer, @SlipEntry,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """

        let! _ = connection.ExecuteAsync(sql, departments, transaction)
        printfn "部門マスタ: %d件投入完了" departments.Length
    }

/// 社員マスタの投入
let private seedEmployeesAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "社員マスタの投入..."

        let now = DateTime.Now
        let employees = [
            // 経営層
            {| Code = "E00001"; Name = "山田 太郎"; DeptCode = "000000"; HireDate = DateTime(1955, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "E00002"; Name = "佐藤 次郎"; DeptCode = "000000"; HireDate = DateTime(1960, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}

            // 食肉製造・販売事業（正社員）
            {| Code = "E10001"; Name = "鈴木 一郎"; DeptCode = "111000"; HireDate = DateTime(2010, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "E10002"; Name = "高橋 花子"; DeptCode = "112000"; HireDate = DateTime(2012, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "E10003"; Name = "田中 次郎"; DeptCode = "121000"; HireDate = DateTime(2015, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "E10004"; Name = "伊藤 三郎"; DeptCode = "122000"; HireDate = DateTime(2016, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "E10005"; Name = "渡辺 四郎"; DeptCode = "131000"; HireDate = DateTime(2018, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "E10006"; Name = "山本 五郎"; DeptCode = "132000"; HireDate = DateTime(2019, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}

            // 食肉加工品事業（正社員）
            {| Code = "E20001"; Name = "中村 太郎"; DeptCode = "211000"; HireDate = DateTime(2013, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "E20002"; Name = "小林 次郎"; DeptCode = "212000"; HireDate = DateTime(2014, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "E20003"; Name = "加藤 三郎"; DeptCode = "221000"; HireDate = DateTime(2017, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}

            // コンサルティング事業（正社員）
            {| Code = "E30001"; Name = "吉田 太郎"; DeptCode = "311000"; HireDate = DateTime(2020, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "E30002"; Name = "山口 次郎"; DeptCode = "312000"; HireDate = DateTime(2021, 4, 1); CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
        ]

        let sql = """
            INSERT INTO 社員マスタ (
                社員コード, 社員名, 部門コード, 入社年月日,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @DeptCode, @HireDate,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """

        let! _ = connection.ExecuteAsync(sql, employees, transaction)
        printfn "社員マスタ: %d件投入完了" employees.Length
    }

/// 取引先グループの投入
let private seedCompanyGroupsAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "取引先グループマスタの投入..."

        let now = DateTime.Now
        let groups = [
            {| Code = "G001"; Name = "百貨店"; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "G002"; Name = "スーパー"; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "G003"; Name = "ホテル・旅館"; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "G004"; Name = "飲食店"; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "G005"; Name = "観光施設"; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "G006"; Name = "食肉卸"; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "G007"; Name = "畜産業者"; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
        ]

        let sql = """
            INSERT INTO 取引先グループマスタ (
                取引先グループコード, 取引先グループ名,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """

        let! _ = connection.ExecuteAsync(sql, groups, transaction)
        printfn "取引先グループマスタ: %d件投入完了" groups.Length
    }

/// 取引先マスタの投入
let private seedCompaniesAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "取引先マスタの投入..."

        let now = DateTime.Now
        let companies = [
            // 得意先（顧客）
            {| Code = "C0000001"; Name = "X県百貨店"; GroupCode = "G001"; SupplierFlag = 0; Zip = "1000001"; Prefecture = "東京都"; Address1 = "X市中央1-1-1"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 1000000; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000002"; Name = "Y県百貨店"; GroupCode = "G001"; SupplierFlag = 0; Zip = "2000002"; Prefecture = "神奈川"; Address1 = "Y市中央2-2-2"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 1000000; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000003"; Name = "地域スーパーA"; GroupCode = "G002"; SupplierFlag = 0; Zip = "3000003"; Prefecture = "東京都"; Address1 = "X市東3-3-3"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 500000; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000004"; Name = "広域スーパーB"; GroupCode = "G002"; SupplierFlag = 0; Zip = "4000004"; Prefecture = "東京都"; Address1 = "X市西4-4-4"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 800000; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000005"; Name = "シティホテルC"; GroupCode = "G003"; SupplierFlag = 0; Zip = "5000005"; Prefecture = "東京都"; Address1 = "X市南5-5-5"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 300000; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000006"; Name = "温泉旅館D"; GroupCode = "G003"; SupplierFlag = 0; Zip = "6000006"; Prefecture = "神奈川"; Address1 = "Y市北6-6-6"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 200000; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000007"; Name = "焼肉レストランE"; GroupCode = "G004"; SupplierFlag = 0; Zip = "7000007"; Prefecture = "東京都"; Address1 = "X市中央7-7-7"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 150000; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000008"; Name = "イタリアンF"; GroupCode = "G004"; SupplierFlag = 0; Zip = "8000008"; Prefecture = "東京都"; Address1 = "X市東8-8-8"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 150000; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000009"; Name = "道の駅G"; GroupCode = "G005"; SupplierFlag = 0; Zip = "9000009"; Prefecture = "千葉県"; Address1 = "Z市西9-9-9"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 100000; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000010"; Name = "観光センターH"; GroupCode = "G005"; SupplierFlag = 0; Zip = "1100010"; Prefecture = "千葉県"; Address1 = "Z市南10-10-10"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 100000; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}

            // 仕入先
            {| Code = "S0000001"; Name = "地域食肉卸A"; GroupCode = "G006"; SupplierFlag = 1; Zip = "1200011"; Prefecture = "東京都"; Address1 = "X市北11-11-11"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 0; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "S0000002"; Name = "地域食肉卸B"; GroupCode = "G006"; SupplierFlag = 1; Zip = "1300012"; Prefecture = "神奈川"; Address1 = "Y市中央12-12-12"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 0; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "S0000003"; Name = "地域畜産農家"; GroupCode = "G007"; SupplierFlag = 1; Zip = "1400013"; Prefecture = "千葉県"; Address1 = "Z市東13-13-13"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 0; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "S0000004"; Name = "県内畜産組合"; GroupCode = "G007"; SupplierFlag = 1; Zip = "1500014"; Prefecture = "千葉県"; Address1 = "A市西14-14-14"; Address2 = ""; BanFlag = 0; MiscFlag = 0; CreditLimit = 0; CreditTempIncrease = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
        ]

        let sql = """
            INSERT INTO 取引先マスタ (
                取引先コード, 取引先名, 仕入先区分, 取引先グループコード, 郵便番号, 都道府県, 住所１, 住所２,
                取引禁止フラグ, 雑区分, 与信限度額, 与信一時増加枠,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @SupplierFlag, @GroupCode, @Zip, @Prefecture, @Address1, @Address2,
                @BanFlag, @MiscFlag, @CreditLimit, @CreditTempIncrease,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """

        let! _ = connection.ExecuteAsync(sql, companies, transaction)
        printfn "取引先マスタ: %d件投入完了" companies.Length
    }

/// 顧客マスタの投入
let private seedCustomersAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "顧客マスタの投入..."

        let now = DateTime.Now
        let customers = [
            {| Code = "C0000001"; BranchNo = 0; Category = 0; BillToCode = "C0000001"; BillToBranchNo = 0; CollectFromCode = "C0000001"; CollectFromBranchNo = 0; Name = "X県百貨店"; EmployeeCode = "E00001"; BillingCategory = 0; ClosingDay1 = 31; PaymentMonth1 = 1; PaymentDay1 = 31; PaymentMethod1 = 1; ClosingDay2 = 0; PaymentMonth2 = 1; PaymentDay2 = 0; PaymentMethod2 = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000002"; BranchNo = 0; Category = 0; BillToCode = "C0000002"; BillToBranchNo = 0; CollectFromCode = "C0000002"; CollectFromBranchNo = 0; Name = "Y県百貨店"; EmployeeCode = "E00001"; BillingCategory = 0; ClosingDay1 = 31; PaymentMonth1 = 1; PaymentDay1 = 31; PaymentMethod1 = 1; ClosingDay2 = 0; PaymentMonth2 = 1; PaymentDay2 = 0; PaymentMethod2 = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000003"; BranchNo = 0; Category = 0; BillToCode = "C0000003"; BillToBranchNo = 0; CollectFromCode = "C0000003"; CollectFromBranchNo = 0; Name = "地域スーパーA"; EmployeeCode = "E10001"; BillingCategory = 0; ClosingDay1 = 31; PaymentMonth1 = 1; PaymentDay1 = 31; PaymentMethod1 = 1; ClosingDay2 = 0; PaymentMonth2 = 1; PaymentDay2 = 0; PaymentMethod2 = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000004"; BranchNo = 0; Category = 0; BillToCode = "C0000004"; BillToBranchNo = 0; CollectFromCode = "C0000004"; CollectFromBranchNo = 0; Name = "広域スーパーB"; EmployeeCode = "E10002"; BillingCategory = 0; ClosingDay1 = 31; PaymentMonth1 = 1; PaymentDay1 = 31; PaymentMethod1 = 1; ClosingDay2 = 0; PaymentMonth2 = 1; PaymentDay2 = 0; PaymentMethod2 = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000005"; BranchNo = 0; Category = 0; BillToCode = "C0000005"; BillToBranchNo = 0; CollectFromCode = "C0000005"; CollectFromBranchNo = 0; Name = "シティホテルC"; EmployeeCode = "E10003"; BillingCategory = 0; ClosingDay1 = 31; PaymentMonth1 = 1; PaymentDay1 = 31; PaymentMethod1 = 1; ClosingDay2 = 0; PaymentMonth2 = 1; PaymentDay2 = 0; PaymentMethod2 = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000006"; BranchNo = 0; Category = 0; BillToCode = "C0000006"; BillToBranchNo = 0; CollectFromCode = "C0000006"; CollectFromBranchNo = 0; Name = "温泉旅館D"; EmployeeCode = "E10004"; BillingCategory = 0; ClosingDay1 = 31; PaymentMonth1 = 1; PaymentDay1 = 31; PaymentMethod1 = 1; ClosingDay2 = 0; PaymentMonth2 = 1; PaymentDay2 = 0; PaymentMethod2 = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000007"; BranchNo = 0; Category = 0; BillToCode = "C0000007"; BillToBranchNo = 0; CollectFromCode = "C0000007"; CollectFromBranchNo = 0; Name = "焼肉レストランE"; EmployeeCode = "E10005"; BillingCategory = 0; ClosingDay1 = 31; PaymentMonth1 = 1; PaymentDay1 = 31; PaymentMethod1 = 1; ClosingDay2 = 0; PaymentMonth2 = 1; PaymentDay2 = 0; PaymentMethod2 = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000008"; BranchNo = 0; Category = 0; BillToCode = "C0000008"; BillToBranchNo = 0; CollectFromCode = "C0000008"; CollectFromBranchNo = 0; Name = "イタリアンF"; EmployeeCode = "E10006"; BillingCategory = 0; ClosingDay1 = 31; PaymentMonth1 = 1; PaymentDay1 = 31; PaymentMethod1 = 1; ClosingDay2 = 0; PaymentMonth2 = 1; PaymentDay2 = 0; PaymentMethod2 = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000009"; BranchNo = 0; Category = 0; BillToCode = "C0000009"; BillToBranchNo = 0; CollectFromCode = "C0000009"; CollectFromBranchNo = 0; Name = "道の駅G"; EmployeeCode = "E20001"; BillingCategory = 0; ClosingDay1 = 31; PaymentMonth1 = 1; PaymentDay1 = 31; PaymentMethod1 = 1; ClosingDay2 = 0; PaymentMonth2 = 1; PaymentDay2 = 0; PaymentMethod2 = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "C0000010"; BranchNo = 0; Category = 0; BillToCode = "C0000010"; BillToBranchNo = 0; CollectFromCode = "C0000010"; CollectFromBranchNo = 0; Name = "観光センターH"; EmployeeCode = "E20002"; BillingCategory = 0; ClosingDay1 = 31; PaymentMonth1 = 1; PaymentDay1 = 31; PaymentMethod1 = 1; ClosingDay2 = 0; PaymentMonth2 = 1; PaymentDay2 = 0; PaymentMethod2 = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
        ]

        let sql = """
            INSERT INTO 顧客マスタ (
                顧客コード, 顧客枝番, 顧客区分, 請求先コード, 請求先枝番, 回収先コード, 回収先枝番,
                顧客名, 自社担当者コード, 顧客請求区分,
                顧客締日１, 顧客支払月１, 顧客支払日１, 顧客支払方法１,
                顧客締日２, 顧客支払月２, 顧客支払日２, 顧客支払方法２,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @BranchNo, @Category, @BillToCode, @BillToBranchNo, @CollectFromCode, @CollectFromBranchNo,
                @Name, @EmployeeCode, @BillingCategory,
                @ClosingDay1, @PaymentMonth1, @PaymentDay1, @PaymentMethod1,
                @ClosingDay2, @PaymentMonth2, @PaymentDay2, @PaymentMethod2,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """

        let! _ = connection.ExecuteAsync(sql, customers, transaction)
        printfn "顧客マスタ: %d件投入完了" customers.Length
    }

/// 仕入先マスタの投入
let private seedSuppliersAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "仕入先マスタの投入..."

        let now = DateTime.Now
        let suppliers = [
            {| Code = "S0000001"; BranchNo = 0; Name = "地域食肉卸A"; EmployeeCode = "E00002"; ClosingDay = 31; PaymentMonth = 1; PaymentDay = 31; PaymentMethod = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "S0000002"; BranchNo = 0; Name = "地域食肉卸B"; EmployeeCode = "E00002"; ClosingDay = 31; PaymentMonth = 1; PaymentDay = 31; PaymentMethod = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "S0000003"; BranchNo = 0; Name = "地域畜産農家"; EmployeeCode = "E00002"; ClosingDay = 31; PaymentMonth = 1; PaymentDay = 31; PaymentMethod = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "S0000004"; BranchNo = 0; Name = "県内畜産組合"; EmployeeCode = "E00002"; ClosingDay = 31; PaymentMonth = 1; PaymentDay = 31; PaymentMethod = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
        ]

        let sql = """
            INSERT INTO 仕入先マスタ (
                仕入先コード, 仕入先枝番, 仕入先名, 自社担当者コード,
                仕入先締日, 仕入先支払月, 仕入先支払日, 仕入先支払方法,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @BranchNo, @Name, @EmployeeCode,
                @ClosingDay, @PaymentMonth, @PaymentDay, @PaymentMethod,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """

        let! _ = connection.ExecuteAsync(sql, suppliers, transaction)
        printfn "仕入先マスタ: %d件投入完了" suppliers.Length
    }

/// 商品分類マスタの投入
let private seedProductCategoriesAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "商品分類マスタの投入..."

        let now = DateTime.Now
        let categories = [
            {| Code = "CAT001"; Name = "牛肉"; Layer = 1; Path = "/CAT001"; LowestLayer = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "CAT002"; Name = "豚肉"; Layer = 1; Path = "/CAT002"; LowestLayer = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "CAT003"; Name = "鶏肉"; Layer = 1; Path = "/CAT003"; LowestLayer = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "CAT004"; Name = "加工品"; Layer = 1; Path = "/CAT004"; LowestLayer = 1s; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
        ]

        let sql = """
            INSERT INTO 商品分類マスタ (
                商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @Layer, @Path, @LowestLayer,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """

        let! _ = connection.ExecuteAsync(sql, categories, transaction)
        printfn "商品分類マスタ: %d件投入完了" categories.Length
    }

/// 商品マスタの投入
let private seedProductsAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "商品マスタの投入..."

        let now = DateTime.Now
        let products = [
            // 牛肉製品
            {| Code = "PRD001"; FormalName = "黒毛和牛サーロイン"; ShortName = "和牛サーロイン"; Category = "PRODUCT"; CategoryCode = "CAT001"; SalesPrice = 5000; PurchasePrice = 3500; CostOfSales = 3500; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000001"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD002"; FormalName = "黒毛和牛ロース"; ShortName = "和牛ロース"; Category = "PRODUCT"; CategoryCode = "CAT001"; SalesPrice = 4500; PurchasePrice = 3200; CostOfSales = 3200; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000001"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD003"; FormalName = "黒毛和牛カルビ"; ShortName = "和牛カルビ"; Category = "PRODUCT"; CategoryCode = "CAT001"; SalesPrice = 4000; PurchasePrice = 2800; CostOfSales = 2800; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000001"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD004"; FormalName = "黒毛和牛ヒレ"; ShortName = "和牛ヒレ"; Category = "PRODUCT"; CategoryCode = "CAT001"; SalesPrice = 5500; PurchasePrice = 4000; CostOfSales = 4000; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000001"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD005"; FormalName = "国産牛切り落とし"; ShortName = "牛切り落とし"; Category = "PRODUCT"; CategoryCode = "CAT001"; SalesPrice = 2000; PurchasePrice = 1400; CostOfSales = 1400; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000001"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}

            // 豚肉製品
            {| Code = "PRD006"; FormalName = "豚ロース"; ShortName = "豚ロース"; Category = "PRODUCT"; CategoryCode = "CAT002"; SalesPrice = 1500; PurchasePrice = 1000; CostOfSales = 1000; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000002"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD007"; FormalName = "豚バラ"; ShortName = "豚バラ"; Category = "PRODUCT"; CategoryCode = "CAT002"; SalesPrice = 1200; PurchasePrice = 800; CostOfSales = 800; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000002"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD008"; FormalName = "豚ヒレ"; ShortName = "豚ヒレ"; Category = "PRODUCT"; CategoryCode = "CAT002"; SalesPrice = 1800; PurchasePrice = 1200; CostOfSales = 1200; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000002"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD009"; FormalName = "豚コマ"; ShortName = "豚コマ"; Category = "PRODUCT"; CategoryCode = "CAT002"; SalesPrice = 800; PurchasePrice = 500; CostOfSales = 500; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000002"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD010"; FormalName = "豚肩ロース"; ShortName = "豚肩ロース"; Category = "PRODUCT"; CategoryCode = "CAT002"; SalesPrice = 1300; PurchasePrice = 900; CostOfSales = 900; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000002"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}

            // 鶏肉製品
            {| Code = "PRD011"; FormalName = "鶏もも"; ShortName = "鶏もも"; Category = "PRODUCT"; CategoryCode = "CAT003"; SalesPrice = 800; PurchasePrice = 500; CostOfSales = 500; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000003"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD012"; FormalName = "鶏むね"; ShortName = "鶏むね"; Category = "PRODUCT"; CategoryCode = "CAT003"; SalesPrice = 600; PurchasePrice = 400; CostOfSales = 400; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000003"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD013"; FormalName = "手羽先"; ShortName = "手羽先"; Category = "PRODUCT"; CategoryCode = "CAT003"; SalesPrice = 500; PurchasePrice = 300; CostOfSales = 300; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000003"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD014"; FormalName = "手羽元"; ShortName = "手羽元"; Category = "PRODUCT"; CategoryCode = "CAT003"; SalesPrice = 450; PurchasePrice = 280; CostOfSales = 280; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000003"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD015"; FormalName = "鶏ささみ"; ShortName = "鶏ささみ"; Category = "PRODUCT"; CategoryCode = "CAT003"; SalesPrice = 700; PurchasePrice = 450; CostOfSales = 450; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000003"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}

            // 加工品
            {| Code = "PRD016"; FormalName = "ローストビーフ"; ShortName = "ローストビーフ"; Category = "PRODUCT"; CategoryCode = "CAT004"; SalesPrice = 3000; PurchasePrice = 2000; CostOfSales = 2000; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000001"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD017"; FormalName = "特製ハム"; ShortName = "特製ハム"; Category = "PRODUCT"; CategoryCode = "CAT004"; SalesPrice = 1500; PurchasePrice = 1000; CostOfSales = 1000; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000002"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD018"; FormalName = "特製ソーセージ"; ShortName = "特製ソーセージ"; Category = "PRODUCT"; CategoryCode = "CAT004"; SalesPrice = 1200; PurchasePrice = 800; CostOfSales = 800; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000002"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD019"; FormalName = "特製ベーコン"; ShortName = "特製ベーコン"; Category = "PRODUCT"; CategoryCode = "CAT004"; SalesPrice = 1800; PurchasePrice = 1200; CostOfSales = 1200; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000002"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "PRD020"; FormalName = "手作りコロッケ"; ShortName = "手作りコロッケ"; Category = "PRODUCT"; CategoryCode = "CAT004"; SalesPrice = 300; PurchasePrice = 200; CostOfSales = 200; TaxCategory = 1; MiscCategory = 0; StockControl = 1; StockAllocation = 0; SupplierCode = "S0000004"; SupplierBranchNo = 0; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
        ]

        let sql = """
            INSERT INTO 商品マスタ (
                商品コード, 商品正式名, 商品略称, 商品区分, 商品分類コード,
                販売単価, 仕入単価, 売上原価, 税区分, 雑区分,
                在庫管理対象区分, 在庫引当区分, 仕入先コード, 仕入先枝番,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @FormalName, @ShortName, @Category, @CategoryCode,
                @SalesPrice, @PurchasePrice, @CostOfSales, @TaxCategory, @MiscCategory,
                @StockControl, @StockAllocation, @SupplierCode, @SupplierBranchNo,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """

        let! _ = connection.ExecuteAsync(sql, products, transaction)
        printfn "商品マスタ: %d件投入完了" products.Length
    }

/// 倉庫マスタの投入
let private seedWarehousesAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "倉庫マスタの投入..."

        let now = DateTime.Now
        let warehouses = [
            {| Code = "001"; Name = "本社倉庫"; Category = 1; Address = "X県X市中央1-1-1"; Phone = "03-1111-1111"; ManagerCode = "E00001"; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "002"; Name = "工場倉庫"; Category = 1; Address = "X県X市工業団地2-2-2"; Phone = "03-2222-2222"; ManagerCode = "E10001"; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
        ]

        let sql = """
            INSERT INTO 倉庫マスタ (
                倉庫コード, 倉庫名, 倉庫区分, 住所, 電話番号, 責任者コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @Category, @Address, @Phone, @ManagerCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """

        let! _ = connection.ExecuteAsync(sql, warehouses, transaction)
        printfn "倉庫マスタ: %d件投入完了" warehouses.Length
    }

/// 入金口座マスタの投入
let private seedBankAccountsAsync (connection: NpgsqlConnection) (transaction: NpgsqlTransaction) : Task<unit> =
    task {
        printfn "入金口座マスタの投入..."

        let now = DateTime.Now
        let bankAccounts = [
            {| Code = "BA001"; Name = "本店売上入金口座"; BankName = "みずほ銀行"; BranchName = "X県支店"; AccountNumber = "1234567"; AccountType = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
            {| Code = "BA002"; Name = "工場売上入金口座"; BankName = "三菱UFJ銀行"; BranchName = "Y県支店"; AccountNumber = "2345678"; AccountType = 1; CreatedAt = now; CreatedBy = "SYSTEM"; UpdatedAt = now; UpdatedBy = "SYSTEM" |}
        ]

        let sql = """
            INSERT INTO 入金口座マスタ (
                口座コード, 口座名, 銀行名, 支店名, 口座番号, 口座種別,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @BankName, @BranchName, @AccountNumber, @AccountType,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """

        let! _ = connection.ExecuteAsync(sql, bankAccounts, transaction)
        printfn "入金口座マスタ: %d件投入完了" bankAccounts.Length
    }

/// 全てのSeedデータを投入
let seedAllAsync (connectionString: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        do! connection.OpenAsync()

        use transaction = connection.BeginTransaction()

        try
            printfn "=== Seedデータ投入開始 ==="

            // 既存データのクリア（開発環境のみ）
            do! clearExistingDataAsync connection transaction

            // マスタデータの投入（外部キー制約を考慮した順序）
            do! seedDepartmentsAsync connection transaction
            do! seedEmployeesAsync connection transaction
            do! seedCompanyGroupsAsync connection transaction
            do! seedCompaniesAsync connection transaction
            do! seedCustomersAsync connection transaction
            do! seedSuppliersAsync connection transaction
            do! seedProductCategoriesAsync connection transaction
            do! seedProductsAsync connection transaction
            do! seedWarehousesAsync connection transaction
            do! seedBankAccountsAsync connection transaction

            do! transaction.CommitAsync()
            printfn "=== Seedデータ投入完了 ==="
        with
        | ex ->
            do! transaction.RollbackAsync()
            printfn "エラー: %s" ex.Message
            return raise ex
    }
