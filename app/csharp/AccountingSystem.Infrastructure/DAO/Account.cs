using System.ComponentModel.DataAnnotations;

namespace AccountingSystem.Infrastructure.DAO
{
    /// <summary>
    /// 勘定科目マスタ
    /// </summary>
    public class Account
    {
        /// <summary>
        /// 勘定科目ID（主キー）
        /// </summary>
        public int AccountId { get; set; }

        /// <summary>
        /// 勘定科目コード（例：1000, 2000）
        /// </summary>
        [Required(ErrorMessage = "勘定科目コードは必須です")]
        [StringLength(20, ErrorMessage = "勘定科目コードは20文字以内で入力してください")]
        public string AccountCode { get; set; } = string.Empty;

        /// <summary>
        /// 勘定科目名（例：現金、売掛金）
        /// </summary>
        [Required(ErrorMessage = "勘定科目名は必須です")]
        [StringLength(100, ErrorMessage = "勘定科目名は100文字以内で入力してください")]
        public string AccountName { get; set; } = string.Empty;

        /// <summary>
        /// 勘定科目種別（資産、負債、純資産、収益、費用）
        /// </summary>
        [Required(ErrorMessage = "勘定科目種別は必須です")]
        public AccountType AccountType { get; set; }

        /// <summary>
        /// 残高
        /// </summary>
        public decimal Balance { get; set; }

        /// <summary>
        /// BSPL区分（B:貸借対照表, P:損益計算書）
        /// </summary>
        [StringLength(1)]
        public string? BsplCategory { get; set; }

        /// <summary>
        /// 取引要素区分（1:資産, 2:負債, 3:純資産, 4:収益, 5:費用）
        /// </summary>
        [StringLength(1)]
        public string? TransactionElementCategory { get; set; }

        /// <summary>
        /// 費用区分（1:売上原価, 2:販売費及び一般管理費, 3:営業外費用）
        /// </summary>
        [StringLength(1)]
        public string? ExpenseCategory { get; set; }

        /// <summary>
        /// 合計科目（true: 集計科目, false: 明細科目）
        /// </summary>
        public bool IsSummaryAccount { get; set; }

        /// <summary>
        /// 表示順序（財務諸表での表示順）
        /// </summary>
        public int? DisplayOrder { get; set; }

        /// <summary>
        /// 集計対象（true: 集計対象, false: 集計対象外）
        /// </summary>
        public bool IsAggregationTarget { get; set; } = true;

        /// <summary>
        /// 勘定科目カナ（検索用）
        /// </summary>
        [StringLength(40)]
        public string? AccountNameKana { get; set; }

        /// <summary>
        /// 作成日時
        /// </summary>
        public DateTime CreatedAt { get; set; }

        /// <summary>
        /// 更新日時
        /// </summary>
        public DateTime UpdatedAt { get; set; }
    }

    /// <summary>
    /// 勘定科目種別
    /// </summary>
    public enum AccountType
    {
        /// <summary>資産</summary>
        Asset,
        /// <summary>負債</summary>
        Liability,
        /// <summary>純資産</summary>
        Equity,
        /// <summary>収益</summary>
        Revenue,
        /// <summary>費用</summary>
        Expense
    }
}
