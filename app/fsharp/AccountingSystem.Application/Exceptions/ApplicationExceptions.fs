namespace AccountingSystem.Application.Exceptions

open System

/// <summary>
/// 勘定科目が見つからない例外
/// </summary>
type AccountNotFoundException(message: string) =
    inherit Exception(message)

    new(accountCode: string, innerException: Exception) =
        AccountNotFoundException($"勘定科目が見つかりません: {accountCode}")

/// <summary>
/// 勘定科目が重複している例外
/// </summary>
type DuplicateAccountException(message: string) =
    inherit Exception(message)

    new(accountCode: string, innerException: Exception) =
        DuplicateAccountException($"勘定科目コードが重複しています: {accountCode}")

/// <summary>
/// 仕訳が見つからない例外
/// </summary>
type JournalNotFoundException(message: string) =
    inherit Exception(message)

    new(voucherNumber: int) =
        JournalNotFoundException($"仕訳が見つかりません: 伝票番号 {voucherNumber}")

/// <summary>
/// 仕訳が無効な例外（貸借不一致など）
/// </summary>
type InvalidJournalEntryException(message: string) =
    inherit Exception(message)

/// <summary>
/// ビジネスルール違反の例外
/// </summary>
type BusinessRuleViolationException(message: string) =
    inherit Exception(message)
