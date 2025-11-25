using System.ComponentModel.DataAnnotations;
using FluentAssertions;
using AccountingSystem.Domain.Models;
using Xunit;

namespace AccountingSystem.Tests.DomainTests
{
    public class AccountValidationTests
    {
        [Fact]
        public void 正常なデータはバリデーションエラーが発生しない()
        {
            // Arrange
            var account = new Account
            {
                AccountCode = "1110",
                AccountName = "現金預金",
                AccountType = "資産",
                SumAccount = false
            };

            // Act
            var validationResults = new List<ValidationResult>();
            var isValid = Validator.TryValidateObject(
                account,
                new ValidationContext(account),
                validationResults,
                true);

            // Assert
            isValid.Should().BeTrue();
            validationResults.Should().BeEmpty();
        }

        [Fact]
        public void 勘定科目コードがnullの場合はバリデーションエラーが発生する()
        {
            // Arrange
            var account = new Account
            {
                AccountCode = null!,
                AccountName = "現金預金",
                AccountType = "資産",
                SumAccount = false
            };

            // Act
            var validationResults = new List<ValidationResult>();
            var isValid = Validator.TryValidateObject(
                account,
                new ValidationContext(account),
                validationResults,
                true);

            // Assert
            isValid.Should().BeFalse();
            validationResults.Should().HaveCount(1);
            validationResults[0].ErrorMessage.Should().Be("勘定科目コードは必須です");
        }
    }
}
