module SalesManagement.Tests.IntegrationTests.EmployeeTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type EmployeeTests() =
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
            SlipInputFlag = 1
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    let createTestEmployee code name deptCode =
        {
            EmployeeCode = code
            EmployeeName = name
            EmployeeNameKana = "ヤマダタロウ"
            Gender = "M"
            BirthDate = Some (DateTime(1990, 1, 1))
            JoinDate = DateTime(2015, 4, 1)
            DepartmentCode = deptCode
            PositionCode = Some "POS001"
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    [<Fact>]
    member this.``社員を登録できる``() =
        task {
            // Arrange
            let department = createTestDepartment "10000" "本社"
            do! DepartmentRepository.insertAsync this.ConnectionString department

            let employee = createTestEmployee "EMP001" "山田太郎" "10000"

            // Act
            do! EmployeeRepository.insertAsync this.ConnectionString employee

            // Assert
            let! found = EmployeeRepository.findByIdAsync this.ConnectionString "EMP001"
            found |> should not' (equal None)
            found.Value.EmployeeName |> should equal "山田太郎"
            found.Value.DepartmentCode |> should equal "10000"
        }

    [<Fact>]
    member this.``社員を更新できる``() =
        task {
            // Arrange
            let department = createTestDepartment "10000" "本社"
            do! DepartmentRepository.insertAsync this.ConnectionString department

            let employee = createTestEmployee "EMP001" "山田太郎" "10000"
            do! EmployeeRepository.insertAsync this.ConnectionString employee

            // Act
            let updated =
                { employee with
                    EmployeeName = "山田次郎"
                    UpdatedAt = DateTime.Now
                    UpdatedBy = "updater" }
            do! EmployeeRepository.updateAsync this.ConnectionString updated

            // Assert
            let! result = EmployeeRepository.findByIdAsync this.ConnectionString "EMP001"
            result |> should not' (equal None)
            result.Value.EmployeeName |> should equal "山田次郎"
        }

    [<Fact>]
    member this.``社員を削除できる``() =
        task {
            // Arrange
            let department = createTestDepartment "10000" "本社"
            do! DepartmentRepository.insertAsync this.ConnectionString department

            let employee = createTestEmployee "EMP001" "山田太郎" "10000"
            do! EmployeeRepository.insertAsync this.ConnectionString employee

            // Act
            do! EmployeeRepository.deleteAsync this.ConnectionString "EMP001"

            // Assert
            let! deleted = EmployeeRepository.findByIdAsync this.ConnectionString "EMP001"
            deleted |> should equal None
        }

    [<Fact>]
    member this.``部門に所属する社員を取得できる``() =
        task {
            // Arrange
            let department = createTestDepartment "10000" "本社"
            do! DepartmentRepository.insertAsync this.ConnectionString department

            let employee1 = createTestEmployee "EMP001" "山田太郎" "10000"
            let employee2 = createTestEmployee "EMP002" "佐藤花子" "10000"
            do! EmployeeRepository.insertAsync this.ConnectionString employee1
            do! EmployeeRepository.insertAsync this.ConnectionString employee2

            // Act
            let! employees = EmployeeRepository.findEmployeesWithDepartmentAsync this.ConnectionString "10000"
            let empList = employees |> Seq.toList

            // Assert
            empList |> should haveLength 2
            empList.[0].社員名 |> should equal "山田太郎"
            empList.[0].部門名 |> should equal "本社"
            empList.[1].社員名 |> should equal "佐藤花子"
            empList.[1].部門名 |> should equal "本社"
        }
