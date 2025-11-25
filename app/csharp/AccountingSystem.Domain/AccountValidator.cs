namespace AccountingSystem.Domain;

/// <summary>
/// 勘定科目の整合性を検証するバリデーター
/// </summary>
public class AccountValidator
{
    /// <summary>
    /// 勘定科目の整合性を検証
    /// </summary>
    /// <param name="accountType">勘定科目種別</param>
    /// <param name="bsplDistinction">BSPL区分</param>
    /// <param name="transactionDistinction">取引要素区分</param>
    /// <param name="costDistinction">費用区分</param>
    /// <returns>検証結果</returns>
    public static ValidationResult Validate(
        string accountType,
        string? bsplDistinction,
        string? transactionDistinction,
        string? costDistinction)
    {
        var errors = new List<string>();

        // BSPL区分と勘定科目種別の整合性
        if (bsplDistinction == "B")
        {
            if (!new[] { "資産", "負債", "純資産" }.Contains(accountType))
            {
                errors.Add(
                    "BSPL区分が 'B' の場合、勘定科目種別は資産・負債・純資産である必要があります"
                );
            }
        }

        if (bsplDistinction == "P")
        {
            if (!new[] { "収益", "費用" }.Contains(accountType))
            {
                errors.Add(
                    "BSPL区分が 'P' の場合、勘定科目種別は収益・費用である必要があります"
                );
            }
        }

        // 取引要素区分と勘定科目種別の整合性
        var validDistinctions = new Dictionary<string, string>
        {
            { "資産", "1" },
            { "負債", "2" },
            { "純資産", "3" },
            { "収益", "4" },
            { "費用", "5" }
        };

        if (transactionDistinction != null &&
            validDistinctions.TryGetValue(accountType, out var expectedDistinction))
        {
            if (expectedDistinction != transactionDistinction)
            {
                errors.Add(
                    $"{accountType} の取引要素区分は '{expectedDistinction}' である必要があります"
                );
            }
        }

        // 費用区分は費用科目のみ
        if (costDistinction != null && accountType != "費用")
        {
            errors.Add("費用区分は費用科目のみ設定可能です");
        }

        return new ValidationResult(errors.Count == 0, errors);
    }

    /// <summary>
    /// 検証結果
    /// </summary>
    public class ValidationResult
    {
        public bool IsValid { get; }
        public IReadOnlyList<string> Errors { get; }

        public ValidationResult(bool isValid, List<string> errors)
        {
            IsValid = isValid;
            Errors = errors.AsReadOnly();
        }
    }
}
