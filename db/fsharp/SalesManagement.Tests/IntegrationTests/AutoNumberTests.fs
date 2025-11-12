module SalesManagement.Tests.IntegrationTests.AutoNumberTests

open System
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type AutoNumberTests() =
    inherit DatabaseTestBase()

    let setupAutoNumber (connectionString: string) (slipType: string) (yearMonth: string) (lastSlipNo: int) =
        task {
            let autoNumber = {
                SlipType = slipType
                YearMonth = yearMonth
                LastSlipNo = lastSlipNo
                CreatedAt = DateTime.Now
                CreatedBy = Some "system"
                UpdatedAt = DateTime.Now
                UpdatedBy = Some "system"
            }
            do! AutoNumberRepository.insertAsync connectionString autoNumber
        }

    let createTestAutoNumber slipType yearMonth lastSlipNo =
        {
            SlipType = slipType
            YearMonth = yearMonth
            LastSlipNo = lastSlipNo
            CreatedAt = DateTime.Now
            CreatedBy = Some "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = Some "system"
        }

    [<Fact>]
    member this.``自動採番を登録できる``() =
        task {
            // Arrange
            let yearMonth = DateTime.Now.ToString("yyyyMM")
            let autoNumber = createTestAutoNumber "OR" yearMonth 0

            // Act
            do! AutoNumberRepository.insertAsync this.ConnectionString autoNumber

            // Assert
            let! found = AutoNumberRepository.findByIdAsync this.ConnectionString "OR" yearMonth
            found |> should not' (equal None)
            found.Value.SlipType |> should equal "OR"
            found.Value.LastSlipNo |> should equal 0
        }

    [<Fact>]
    member this.``最終伝票番号をインクリメントできる``() =
        task {
            // Arrange
            let yearMonth = DateTime.Now.ToString("yyyyMM")
            do! setupAutoNumber this.ConnectionString "OR" yearMonth 0

            // Act
            do! AutoNumberRepository.incrementLastSlipNoAsync this.ConnectionString "OR" yearMonth

            // Assert
            let! updated = AutoNumberRepository.findByIdAsync this.ConnectionString "OR" yearMonth
            updated.Value.LastSlipNo |> should equal 1

            // Act: さらにインクリメント
            do! AutoNumberRepository.incrementLastSlipNoAsync this.ConnectionString "OR" yearMonth

            // Assert
            let! updated2 = AutoNumberRepository.findByIdAsync this.ConnectionString "OR" yearMonth
            updated2.Value.LastSlipNo |> should equal 2
        }

    [<Fact>]
    member this.``伝票番号を生成できる``() =
        task {
            // Arrange & Act: 初回生成（レコード未存在）
            let! slipNo1 = AutoNumberRepository.generateSlipNoAsync this.ConnectionString "OR"

            // Assert
            let yearMonth = DateTime.Now.ToString("yyyyMM")
            slipNo1 |> should startWith "OR"
            slipNo1.Contains(yearMonth) |> should be True
            slipNo1 |> should endWith "00001"

            // Act: 2回目生成
            let! slipNo2 = AutoNumberRepository.generateSlipNoAsync this.ConnectionString "OR"

            // Assert
            slipNo2 |> should endWith "00002"
        }

    [<Fact>]
    member this.``伝票種別ごとに独立して番号を生成できる``() =
        task {
            // Arrange & Act
            let! orderNo1 = AutoNumberRepository.generateSlipNoAsync this.ConnectionString "OR"
            let! salesNo1 = AutoNumberRepository.generateSlipNoAsync this.ConnectionString "SA"
            let! orderNo2 = AutoNumberRepository.generateSlipNoAsync this.ConnectionString "OR"
            let! salesNo2 = AutoNumberRepository.generateSlipNoAsync this.ConnectionString "SA"

            // Assert
            orderNo1 |> should startWith "OR"
            orderNo1 |> should endWith "00001"

            salesNo1 |> should startWith "SA"
            salesNo1 |> should endWith "00001"

            orderNo2 |> should startWith "OR"
            orderNo2 |> should endWith "00002"

            salesNo2 |> should startWith "SA"
            salesNo2 |> should endWith "00002"
        }

    [<Fact>]
    member this.``FOR_UPDATEによる排他制御で一意性が保証される``() =
        task {
            // 同時実行のシミュレーション
            // 連続実行で一意性が保証されることを確認

            // Arrange & Act
            let tasks =
                [1..10]
                |> List.map (fun _ -> AutoNumberRepository.generateSlipNoAsync this.ConnectionString "OR")
                |> Array.ofList

            let! slipNos = Task.WhenAll(tasks)

            // Assert: 全て異なる番号が生成されている
            let uniqueCount = slipNos |> Array.distinct |> Array.length
            uniqueCount |> should equal 10

            // 最後の番号は00010
            let sortedSlipNos = slipNos |> Array.sort
            sortedSlipNos.[9] |> should endWith "00010"
        }

    [<Fact>]
    member this.``自動採番を削除できる``() =
        task {
            // Arrange
            let yearMonth = DateTime.Now.ToString("yyyyMM")
            do! setupAutoNumber this.ConnectionString "OR" yearMonth 0

            // Act
            do! AutoNumberRepository.deleteAsync this.ConnectionString "OR" yearMonth

            // Assert
            let! deleted = AutoNumberRepository.findByIdAsync this.ConnectionString "OR" yearMonth
            deleted |> should equal None
        }
