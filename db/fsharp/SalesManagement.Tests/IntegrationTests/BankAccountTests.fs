module SalesManagement.Tests.IntegrationTests.BankAccountTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type BankAccountTests() =
    inherit DatabaseTestBase()

    let createTestBankAccount code name bank branch number accountType =
        {
            AccountCode = code
            AccountName = name
            BankName = bank
            BranchName = branch
            AccountNumber = number
            AccountType = accountType
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``入金口座を登録できる``() =
        task {
            // Arrange
            let bankAccount = createTestBankAccount "BA001" "本店売上入金口座" "みずほ銀行" "東京営業部" "1234567" 1

            // Act
            do! BankAccountRepository.insertAsync this.ConnectionString bankAccount

            // Assert
            let! found = BankAccountRepository.findByIdAsync this.ConnectionString "BA001"
            found |> should not' (equal None)
            found.Value.AccountName |> should equal "本店売上入金口座"
            found.Value.BankName |> should equal "みずほ銀行"
            found.Value.BranchName |> should equal "東京営業部"
            found.Value.AccountNumber |> should equal "1234567"
            found.Value.AccountType |> should equal 1
        }

    [<Fact>]
    member this.``全ての入金口座を取得できる``() =
        task {
            // Arrange
            let account1 = createTestBankAccount "BA001" "口座1" "銀行A" "支店A" "1111111" 1
            let account2 = createTestBankAccount "BA002" "口座2" "銀行B" "支店B" "2222222" 2

            do! BankAccountRepository.insertAsync this.ConnectionString account1
            do! BankAccountRepository.insertAsync this.ConnectionString account2

            // Act
            let! accounts = BankAccountRepository.findAllAsync this.ConnectionString

            // Assert
            let accountList = accounts |> Seq.toList
            accountList |> should haveLength 2
        }

    [<Fact>]
    member this.``入金口座情報を更新できる``() =
        task {
            // Arrange
            let bankAccount = createTestBankAccount "BA001" "口座1" "銀行A" "支店A" "1111111" 1
            do! BankAccountRepository.insertAsync this.ConnectionString bankAccount

            let! found = BankAccountRepository.findByIdAsync this.ConnectionString "BA001"
            let updated = { found.Value with
                              AccountName = "更新後口座名"
                              UpdatedAt = DateTime.Now
                              UpdatedBy = "admin" }

            // Act
            do! BankAccountRepository.updateAsync this.ConnectionString updated

            // Assert
            let! result = BankAccountRepository.findByIdAsync this.ConnectionString "BA001"
            result.Value.AccountName |> should equal "更新後口座名"
            result.Value.UpdatedBy |> should equal "admin"
        }

    [<Fact>]
    member this.``入金口座を削除できる``() =
        task {
            // Arrange
            let bankAccount = createTestBankAccount "BA001" "口座1" "銀行A" "支店A" "1111111" 1
            do! BankAccountRepository.insertAsync this.ConnectionString bankAccount

            // Act
            do! BankAccountRepository.deleteAsync this.ConnectionString "BA001"

            // Assert
            let! deleted = BankAccountRepository.findByIdAsync this.ConnectionString "BA001"
            deleted |> should equal None
        }
