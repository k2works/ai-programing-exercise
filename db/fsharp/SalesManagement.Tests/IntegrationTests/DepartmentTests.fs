module SalesManagement.Tests.IntegrationTests.DepartmentTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories.DepartmentRepository
open SalesManagement.Tests.DatabaseTestBase

type DepartmentTests() =
    inherit DatabaseTestBase()

    /// <summary>
    /// テストデータ作成ヘルパー関数
    /// </summary>
    let createTestDepartment code name =
        {
            DepartmentCode = code
            StartDate = DateTime(2021, 1, 1)
            EndDate = DateTime(2100, 12, 31)
            DepartmentName = name
            OrganizationLevel = 1
            DepartmentPath = code
            LowestLevelFlag = 1
            SlipInputFlag = 0
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    [<Fact>]
    member this.``部門を登録できる``() =
        task {
            // Arrange
            let department = createTestDepartment "11101" "新規部署"

            // Act
            do! insertAsync this.ConnectionString department

            // Assert
            let! found = findByIdAsync this.ConnectionString "11101" (DateTime(2021, 1, 1))
            found |> should not' (equal None)
            found.Value.DepartmentName |> should equal "新規部署"
            found.Value.OrganizationLevel |> should equal 1
        }

    [<Fact>]
    member this.``部門を更新できる``() =
        task {
            // Arrange
            let department = createTestDepartment "11101" "新規部署"
            do! insertAsync this.ConnectionString department

            // Act
            let updated =
                { department with
                    DepartmentName = "更新部署"
                    UpdatedAt = DateTime.Now
                    UpdatedBy = "updater" }
            do! updateAsync this.ConnectionString updated

            // Assert
            let! result = findByIdAsync this.ConnectionString "11101" (DateTime(2021, 1, 1))
            result |> should not' (equal None)
            result.Value.DepartmentName |> should equal "更新部署"
            result.Value.UpdatedBy |> should equal "updater"
        }

    [<Fact>]
    member this.``部門を削除できる``() =
        task {
            // Arrange
            let department = createTestDepartment "11101" "削除対象部署"
            do! insertAsync this.ConnectionString department

            // Act
            do! deleteAsync this.ConnectionString "11101" (DateTime(2021, 1, 1))

            // Assert
            let! deleted = findByIdAsync this.ConnectionString "11101" (DateTime(2021, 1, 1))
            deleted |> should equal None
        }

    [<Fact>]
    member this.``複数の部門を登録して階層構造を確認できる``() =
        task {
            // Arrange
            // 親部門（第1階層）
            let parent =
                { createTestDepartment "10000" "本社" with
                    OrganizationLevel = 1
                    DepartmentPath = "10000"
                    LowestLevelFlag = 0 }
            do! insertAsync this.ConnectionString parent

            // 子部門（第2階層）
            let child =
                { createTestDepartment "11000" "営業本部" with
                    OrganizationLevel = 2
                    DepartmentPath = "10000/11000"
                    LowestLevelFlag = 0 }
            do! insertAsync this.ConnectionString child

            // 孫部門（第3階層）
            let grandChild =
                { createTestDepartment "11101" "営業一課" with
                    OrganizationLevel = 3
                    DepartmentPath = "10000/11000/11101"
                    LowestLevelFlag = 1
                    SlipInputFlag = 1 }
            do! insertAsync this.ConnectionString grandChild

            // Act
            let! departments = findAllAsync this.ConnectionString
            let deptList = departments |> Seq.toList

            // Assert
            deptList |> should haveLength 3
            deptList.[0].DepartmentName |> should equal "本社"
            deptList.[0].OrganizationLevel |> should equal 1
            deptList.[1].DepartmentName |> should equal "営業本部"
            deptList.[1].OrganizationLevel |> should equal 2
            deptList.[2].DepartmentName |> should equal "営業一課"
            deptList.[2].OrganizationLevel |> should equal 3
        }
