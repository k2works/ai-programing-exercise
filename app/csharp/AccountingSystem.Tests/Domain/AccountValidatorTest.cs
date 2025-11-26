using AccountingSystem.Domain.Models;
using FluentAssertions;
using Xunit;

namespace AccountingSystem.Tests.Domain;

/// <summary>
/// AccountValidator のテスト
/// </summary>
public class AccountValidatorTest
{
    [Fact(DisplayName = "正しい勘定科目は検証を通過")]
    public void TestValidAccount()
    {
        var result = AccountValidator.Validate("資産", "B", "1", null);

        result.IsValid.Should().BeTrue();
        result.Errors.Should().BeEmpty();
    }

    [Fact(DisplayName = "BSPL区分と勘定科目種別の不整合を検出")]
    public void TestBsplInconsistency()
    {
        var result = AccountValidator.Validate("資産", "P", "1", null);

        result.IsValid.Should().BeFalse();
        result.Errors.Should().Contain(
            "BSPL区分が 'P' の場合、勘定科目種別は収益・費用である必要があります"
        );
    }

    [Fact(DisplayName = "取引要素区分と勘定科目種別の不整合を検出")]
    public void TestTransactionInconsistency()
    {
        var result = AccountValidator.Validate("資産", "B", "2", null);

        result.IsValid.Should().BeFalse();
        result.Errors.Should().Contain(
            "資産 の取引要素区分は '1' である必要があります"
        );
    }

    [Fact(DisplayName = "費用科目以外の費用区分設定を検出")]
    public void TestCostDistinctionOnlyForExpense()
    {
        var result = AccountValidator.Validate("資産", "B", "1", "1");

        result.IsValid.Should().BeFalse();
        result.Errors.Should().Contain("費用区分は費用科目のみ設定可能です");
    }

    [Fact(DisplayName = "費用科目の費用区分設定は有効")]
    public void TestCostDistinctionForExpense()
    {
        var result = AccountValidator.Validate("費用", "P", "5", "1");

        result.IsValid.Should().BeTrue();
        result.Errors.Should().BeEmpty();
    }
}
