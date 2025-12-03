module AccountingSystem.Infrastructure.Repositories.AccountStructureRepository

open AccountingSystem.Domain.Models
open Dapper
open Npgsql

/// 勘定科目構成を登録
let insertAsync (connectionString: string) (accountStructure: AccountStructure) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            INSERT INTO "勘定科目構成マスタ" (
                "勘定科目コード",
                "勘定科目パス",
                "階層レベル",
                "親科目コード",
                "表示順序",
                "作成日時",
                "更新日時"
            ) VALUES (
                @AccountCode,
                @AccountPath,
                @HierarchyLevel,
                @ParentAccountCode,
                @DisplayOrder,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        """

        let parameters = {|
            AccountCode = accountStructure.AccountCode
            AccountPath = accountStructure.AccountPath
            HierarchyLevel = accountStructure.HierarchyLevel
            ParentAccountCode = accountStructure.ParentAccountCode |> Option.toObj
            DisplayOrder = accountStructure.DisplayOrder
        |}

        let! _ = conn.ExecuteAsync(sql, parameters)
        ()
    }

/// 勘定科目コードで検索
let findByCodeAsync (connectionString: string) (accountCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目コード" AS "AccountCode",
                   "勘定科目パス" AS "AccountPath",
                   "階層レベル" AS "HierarchyLevel",
                   "親科目コード" AS "ParentAccountCode",
                   "表示順序" AS "DisplayOrder"
            FROM "勘定科目構成マスタ"
            WHERE "勘定科目コード" = @AccountCode
        """

        let! results = conn.QueryAsync<AccountStructure>(sql, {| AccountCode = accountCode |})
        return results |> Seq.tryHead
    }

/// 全ての勘定科目構成を取得
let findAllAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目コード" AS "AccountCode",
                   "勘定科目パス" AS "AccountPath",
                   "階層レベル" AS "HierarchyLevel",
                   "親科目コード" AS "ParentAccountCode",
                   "表示順序" AS "DisplayOrder"
            FROM "勘定科目構成マスタ"
            ORDER BY "勘定科目パス"
        """

        let! results = conn.QueryAsync<AccountStructure>(sql)
        return results |> Seq.toList
    }

/// 特定科目配下の子孫を取得（チルダ連結検索）
let findDescendantsAsync (connectionString: string) (accountCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目コード" AS "AccountCode",
                   "勘定科目パス" AS "AccountPath",
                   "階層レベル" AS "HierarchyLevel",
                   "親科目コード" AS "ParentAccountCode",
                   "表示順序" AS "DisplayOrder"
            FROM "勘定科目構成マスタ"
            WHERE "勘定科目パス" LIKE CONCAT('%~', @AccountCode, '~%')
               OR "勘定科目コード" = @AccountCode
            ORDER BY "勘定科目パス"
        """

        let! results = conn.QueryAsync<AccountStructure>(sql, {| AccountCode = accountCode |})
        return results |> Seq.toList
    }

/// 特定階層レベルの科目を取得
let findByLevelAsync (connectionString: string) (hierarchyLevel: int) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "勘定科目コード" AS "AccountCode",
                   "勘定科目パス" AS "AccountPath",
                   "階層レベル" AS "HierarchyLevel",
                   "親科目コード" AS "ParentAccountCode",
                   "表示順序" AS "DisplayOrder"
            FROM "勘定科目構成マスタ"
            WHERE "階層レベル" = @HierarchyLevel
            ORDER BY "表示順序", "勘定科目コード"
        """

        let! results = conn.QueryAsync<AccountStructure>(sql, {| HierarchyLevel = hierarchyLevel |})
        return results |> Seq.toList
    }

/// 勘定科目構成を更新
let updateAsync (connectionString: string) (accountStructure: AccountStructure) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            UPDATE "勘定科目構成マスタ"
            SET "勘定科目パス" = @AccountPath,
                "階層レベル" = @HierarchyLevel,
                "親科目コード" = @ParentAccountCode,
                "表示順序" = @DisplayOrder,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "勘定科目コード" = @AccountCode
        """

        let parameters = {|
            AccountCode = accountStructure.AccountCode
            AccountPath = accountStructure.AccountPath
            HierarchyLevel = accountStructure.HierarchyLevel
            ParentAccountCode = accountStructure.ParentAccountCode |> Option.toObj
            DisplayOrder = accountStructure.DisplayOrder
        |}

        let! _ = conn.ExecuteAsync(sql, parameters)
        ()
    }

/// 勘定科目構成を削除
let deleteAsync (connectionString: string) (accountCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            DELETE FROM "勘定科目構成マスタ"
            WHERE "勘定科目コード" = @AccountCode
        """

        let! _ = conn.ExecuteAsync(sql, {| AccountCode = accountCode |})
        ()
    }
