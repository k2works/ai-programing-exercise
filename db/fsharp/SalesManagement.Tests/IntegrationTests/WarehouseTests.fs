module SalesManagement.Tests.IntegrationTests.WarehouseTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type WarehouseTests() =
    inherit DatabaseTestBase()

    let setupEmployee (connectionString: string) employeeCode =
        task {
            let department = {
                DepartmentCode = "10000"
                StartDate = DateTime(2021, 1, 1)
                EndDate = DateTime(2100, 12, 31)
                DepartmentName = "本社"
                OrganizationLevel = 1
                DepartmentPath = "10000"
                LowestLevelFlag = 1
                SlipInputFlag = 1
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! DepartmentRepository.insertAsync connectionString department

            let employee = {
                EmployeeCode = employeeCode
                EmployeeName = "倉庫管理者"
                EmployeeNameKana = "ソウコカンリシャ"
                Gender = "M"
                BirthDate = Some (DateTime(1990, 1, 1))
                JoinDate = DateTime(2015, 4, 1)
                DepartmentCode = "10000"
                PositionCode = None
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! EmployeeRepository.insertAsync connectionString employee
        }

    let createTestWarehouse warehouseCode warehouseName warehouseType managerCode =
        {
            WarehouseCode = warehouseCode
            WarehouseName = warehouseName
            WarehouseType = warehouseType
            Address = Some "東京都千代田区1-1-1"
            PhoneNumber = Some "03-1234-5678"
            ManagerCode = managerCode
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``倉庫を登録できる``() =
        task {
            // Arrange
            do! setupEmployee this.ConnectionString "EMP001"
            let warehouse = createTestWarehouse "WH001" "第一倉庫" 1 (Some "EMP001")

            // Act
            do! WarehouseRepository.insertAsync this.ConnectionString warehouse

            // Assert
            let! found = WarehouseRepository.findByIdAsync this.ConnectionString "WH001"
            found |> should not' (equal None)
            found.Value.WarehouseCode |> should equal "WH001"
            found.Value.WarehouseName |> should equal "第一倉庫"
            found.Value.WarehouseType |> should equal 1
        }

    [<Fact>]
    member this.``倉庫を更新できる``() =
        task {
            // Arrange
            do! setupEmployee this.ConnectionString "EMP001"
            let warehouse = createTestWarehouse "WH001" "第一倉庫" 1 (Some "EMP001")
            do! WarehouseRepository.insertAsync this.ConnectionString warehouse

            // Act
            let updated = { warehouse with WarehouseName = "第一倉庫（更新）"; WarehouseType = 2 }
            do! WarehouseRepository.updateAsync this.ConnectionString updated

            // Assert
            let! found = WarehouseRepository.findByIdAsync this.ConnectionString "WH001"
            found.Value.WarehouseName |> should equal "第一倉庫（更新）"
            found.Value.WarehouseType |> should equal 2
        }

    [<Fact>]
    member this.``倉庫を削除できる``() =
        task {
            // Arrange
            do! setupEmployee this.ConnectionString "EMP001"
            let warehouse = createTestWarehouse "WH001" "第一倉庫" 1 (Some "EMP001")
            do! WarehouseRepository.insertAsync this.ConnectionString warehouse

            // Act
            do! WarehouseRepository.deleteAsync this.ConnectionString "WH001"

            // Assert
            let! deleted = WarehouseRepository.findByIdAsync this.ConnectionString "WH001"
            deleted |> should equal None
        }

    [<Fact>]
    member this.``全ての倉庫を取得できる``() =
        task {
            // Arrange
            do! setupEmployee this.ConnectionString "EMP001"
            let warehouse1 = createTestWarehouse "WH001" "第一倉庫" 1 (Some "EMP001")
            let warehouse2 = createTestWarehouse "WH002" "第二倉庫" 1 (Some "EMP001")

            do! WarehouseRepository.insertAsync this.ConnectionString warehouse1
            do! WarehouseRepository.insertAsync this.ConnectionString warehouse2

            // Act
            let! warehouses = WarehouseRepository.findAllAsync this.ConnectionString

            // Assert
            let warehouseList = warehouses |> Seq.toList
            warehouseList |> List.length |> should be (greaterThanOrEqualTo 2)
        }

    [<Fact>]
    member this.``責任者なしの倉庫を登録できる``() =
        task {
            // Arrange
            let warehouse = createTestWarehouse "WH001" "第一倉庫" 1 None

            // Act
            do! WarehouseRepository.insertAsync this.ConnectionString warehouse

            // Assert
            let! found = WarehouseRepository.findByIdAsync this.ConnectionString "WH001"
            found |> should not' (equal None)
            found.Value.ManagerCode |> should equal None
        }
