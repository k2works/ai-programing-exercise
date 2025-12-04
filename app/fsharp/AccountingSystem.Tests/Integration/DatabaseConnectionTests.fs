module AccountingSystem.Tests.Integration.DatabaseConnectionTests

open Xunit
open FsUnit.Xunit
open Npgsql
open Dapper
open AccountingSystem.Tests.DatabaseTestBase

type DatabaseConnectionTests() =
    inherit DatabaseTestBase()

    /// <summary>
    /// データベースに接続できることを確認します
    /// Testcontainersが正しく動作し、データベースに接続できることを確認します
    /// </summary>
    [<Fact>]
    member this.``データベースに接続できる``() =
        task {
            // Arrange & Act
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()
            let! result = connection.QuerySingleAsync<int>("SELECT 1")

            // Assert
            result |> should equal 1
        }

    /// <summary>
    /// FluentMigratorマイグレーションが実行されていることを確認します
    /// </summary>
    [<Fact>]
    member this.``FluentMigratorマイグレーションが実行されている``() =
        task {
            // Arrange & Act
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let! count = connection.QuerySingleAsync<int64>(
                "SELECT COUNT(*) FROM information_schema.schemata WHERE schema_name = 'public'")

            // Assert
            count |> should be (greaterThan 0L)
        }
