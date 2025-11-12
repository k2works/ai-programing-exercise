module SalesManagement.Tests.IntegrationTests.CreditBalanceTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type CreditBalanceTests() =
    inherit DatabaseTestBase()

    let setupCompany (connectionString: string) (companyCode: string) =
        task {
            let groupCode = companyCode.Substring(0, min 4 companyCode.Length)
            let group = {
                CompanyGroupCode = groupCode
                CompanyGroupName = "テストグループ"
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! CompanyGroupRepository.insertAsync connectionString group

            let company = {
                CompanyCode = companyCode
                CompanyName = "テスト商事"
                CompanyNameKana = None
                SupplierType = 0
                ZipCode = None
                State = None
                Address1 = None
                Address2 = None
                NoSalesFlag = 0
                WideUseType = 0
                CompanyGroupCode = groupCode
                MaxCredit = 0
                TempCreditUp = 0
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! CompanyRepository.insertAsync connectionString company
        }

    let setupCreditBalance (connectionString: string) (companyCode: string) (orderBalance: decimal) (receivableBalance: decimal) (payableBalance: decimal) =
        task {
            let creditBalance = {
                CompanyCode = companyCode
                OrderBalance = orderBalance
                ReceivableBalance = receivableBalance
                PayableBalance = payableBalance
                CreatedAt = DateTime.Now
                CreatedBy = Some "system"
                UpdatedAt = DateTime.Now
                UpdatedBy = Some "system"
            }
            do! CreditBalanceRepository.insertAsync connectionString creditBalance
        }

    let createTestCreditBalance companyCode orderBalance receivableBalance payableBalance =
        {
            CompanyCode = companyCode
            OrderBalance = orderBalance
            ReceivableBalance = receivableBalance
            PayableBalance = payableBalance
            CreatedAt = DateTime.Now
            CreatedBy = Some "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = Some "system"
        }

    [<Fact>]
    member this.``与信残高を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"

            let creditBalance = createTestCreditBalance "COMP001" 0m 0m 0m

            // Act
            do! CreditBalanceRepository.insertAsync this.ConnectionString creditBalance

            // Assert
            let! found = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            found |> should not' (equal None)
            found.Value.OrderBalance |> should equal 0m
            found.Value.ReceivableBalance |> should equal 0m
            found.Value.PayableBalance |> should equal 0m
        }

    [<Fact>]
    member this.``受注残高を加算できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupCreditBalance this.ConnectionString "COMP001" 0m 0m 0m

            // Act: 受注で100万円加算
            do! CreditBalanceRepository.updateOrderBalanceAsync this.ConnectionString "COMP001" 1000000m

            // Assert
            let! updated = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            updated.Value.OrderBalance |> should equal 1000000m

            // Act: さらに50万円の受注
            do! CreditBalanceRepository.updateOrderBalanceAsync this.ConnectionString "COMP001" 500000m

            // Assert
            let! updated2 = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            updated2.Value.OrderBalance |> should equal 1500000m
        }

    [<Fact>]
    member this.``債権残高を増減できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupCreditBalance this.ConnectionString "COMP001" 1000000m 0m 0m

            // Act: 出荷で受注残高を債権残高に移動
            do! CreditBalanceRepository.updateOrderBalanceAsync this.ConnectionString "COMP001" -1000000m
            do! CreditBalanceRepository.updateReceivableBalanceAsync this.ConnectionString "COMP001" 1000000m

            // Assert
            let! updated = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            updated.Value.OrderBalance |> should equal 0m
            updated.Value.ReceivableBalance |> should equal 1000000m

            // Act: 入金で債権残高を減算
            do! CreditBalanceRepository.updateReceivableBalanceAsync this.ConnectionString "COMP001" -1000000m

            // Assert
            let! updated2 = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            updated2.Value.ReceivableBalance |> should equal 0m
        }

    [<Fact>]
    member this.``債務残高を増減できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupCreditBalance this.ConnectionString "COMP001" 0m 0m 0m

            // Act: 仕入で債務残高を加算
            do! CreditBalanceRepository.updatePayableBalanceAsync this.ConnectionString "COMP001" 500000m

            // Assert
            let! updated = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            updated.Value.PayableBalance |> should equal 500000m

            // Act: 支払で債務残高を減算
            do! CreditBalanceRepository.updatePayableBalanceAsync this.ConnectionString "COMP001" -500000m

            // Assert
            let! updated2 = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            updated2.Value.PayableBalance |> should equal 0m
        }

    [<Fact>]
    member this.``与信残高の全体フロー``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupCreditBalance this.ConnectionString "COMP001" 0m 0m 0m

            // 1. 受注 → 受注残高 +100万円
            do! CreditBalanceRepository.updateOrderBalanceAsync this.ConnectionString "COMP001" 1000000m
            let! balance1 = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            balance1.Value.OrderBalance |> should equal 1000000m

            // 2. 出荷 → 受注残高 -100万円、債権残高 +100万円
            do! CreditBalanceRepository.updateOrderBalanceAsync this.ConnectionString "COMP001" -1000000m
            do! CreditBalanceRepository.updateReceivableBalanceAsync this.ConnectionString "COMP001" 1000000m
            let! balance2 = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            balance2.Value.OrderBalance |> should equal 0m
            balance2.Value.ReceivableBalance |> should equal 1000000m

            // 3. 入金 → 債権残高 -100万円
            do! CreditBalanceRepository.updateReceivableBalanceAsync this.ConnectionString "COMP001" -1000000m
            let! balance3 = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            balance3.Value.ReceivableBalance |> should equal 0m
        }

    [<Fact>]
    member this.``与信残高を削除できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupCreditBalance this.ConnectionString "COMP001" 0m 0m 0m

            // Act
            do! CreditBalanceRepository.deleteAsync this.ConnectionString "COMP001"

            // Assert
            let! deleted = CreditBalanceRepository.findByIdAsync this.ConnectionString "COMP001"
            deleted |> should equal None
        }
