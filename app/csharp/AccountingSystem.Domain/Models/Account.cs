using System.ComponentModel.DataAnnotations;

namespace AccountingSystem.Domain.Models
{
    /// <summary>
    /// 勘定科目マスタ
    /// </summary>
    public class Account
    {
        [Required(ErrorMessage = "勘定科目コードは必須です")]
        [StringLength(10, ErrorMessage = "勘定科目コードは10文字以内で入力してください")]
        public string AccountCode { get; set; } = string.Empty;

        [Required(ErrorMessage = "勘定科目名は必須です")]
        [StringLength(40, ErrorMessage = "勘定科目名は40文字以内で入力してください")]
        public string AccountName { get; set; } = string.Empty;

        [Required(ErrorMessage = "科目区分は必須です")]
        [StringLength(10, ErrorMessage = "科目区分は10文字以内で入力してください")]
        public string AccountType { get; set; } = string.Empty;

        public bool SumAccount { get; set; }
    }
}
