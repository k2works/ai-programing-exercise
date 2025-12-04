namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Application.Exceptions
open AccountingSystem.Application.Repositories
open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types

/// <summary>
/// 勘定科目サービス実装
/// ビジネスロジックとトランザクション管理
/// </summary>
type AccountService(accountRepository: IAccountRepository) =

    /// <summary>
    /// ビジネスルール検証
    /// </summary>
    let validateAccount (account: Account) : Result<unit, string> =
        // 勘定科目コードは必須
        if String.IsNullOrWhiteSpace(account.AccountCode.Code) then
            Error "勘定科目コードは必須です"
        // 勘定科目名は必須
        elif String.IsNullOrWhiteSpace(account.AccountName) then
            Error "勘定科目名は必須です"
        else
            Ok ()

    interface IAccountService with

        member _.GetAllAccountsAsync() : Task<Account list> =
            accountRepository.FindAllAsync()

        member _.GetAccountByCodeAsync(accountCode: string) : Task<Account> =
            task {
                let! account = accountRepository.FindByCodeAsync(accountCode)

                return
                    account
                    |> Option.defaultWith (fun () ->
                        raise (AccountNotFoundException($"勘定科目が見つかりません: {accountCode}")))
            }

        member _.GetAccountsByTypeAsync(accountType: string) : Task<Account list> =
            task {
                // 勘定科目種別の妥当性チェック
                match accountType with
                | "資産" | "負債" | "純資産" | "収益" | "費用" -> ()
                | _ ->
                    raise (ArgumentException(
                        "勘定科目種別は '資産', '負債', '純資産', '収益', '費用' のいずれかである必要があります",
                        nameof accountType))

                return! accountRepository.FindByTypeAsync(accountType)
            }

        member _.CreateAccountAsync(account: Account) : Task<Account> =
            task {
                // ビジネスルール検証
                match validateAccount account with
                | Error msg -> raise (ArgumentException msg)
                | Ok () -> ()

                // 重複チェック
                let! existing = accountRepository.FindByCodeAsync(account.AccountCode.Code)

                match existing with
                | Some _ ->
                    return raise (DuplicateAccountException(account.AccountCode.Code, null))
                | None ->
                    let! accountId = accountRepository.SaveAsync(account)
                    let! created = accountRepository.FindByIdAsync(accountId)
                    return
                        created
                        |> Option.defaultWith (fun () ->
                            raise (AccountNotFoundException($"作成した勘定科目が見つかりません: {account.AccountCode.Code}")))
            }

        member _.UpdateAccountAsync(accountCode: string) (account: Account) : Task<Account> =
            task {
                // 存在チェック
                let! existing = accountRepository.FindByCodeAsync(accountCode)

                match existing with
                | None ->
                    return raise (AccountNotFoundException($"勘定科目が見つかりません: {accountCode}"))
                | Some existingAccount ->
                    // ビジネスルール検証
                    match validateAccount account with
                    | Error msg -> raise (ArgumentException msg)
                    | Ok () -> ()

                    // 科目コードを保持して更新
                    let updatedAccount = { account with AccountCode = AccountCode.Create(accountCode); AccountId = existingAccount.AccountId }
                    let! _ = accountRepository.UpdateAsync(updatedAccount)
                    let! updated = accountRepository.FindByCodeAsync(accountCode)
                    return
                        updated
                        |> Option.defaultWith (fun () ->
                            raise (AccountNotFoundException($"更新した勘定科目が見つかりません: {accountCode}")))
            }

        member _.DeleteAccountAsync(accountCode: string) : Task<unit> =
            task {
                let! existing = accountRepository.FindByCodeAsync(accountCode)

                match existing with
                | None ->
                    raise (AccountNotFoundException($"勘定科目が見つかりません: {accountCode}"))
                | Some _ ->
                    let! _ = accountRepository.DeleteByCodeAsync(accountCode)
                    return ()
            }
