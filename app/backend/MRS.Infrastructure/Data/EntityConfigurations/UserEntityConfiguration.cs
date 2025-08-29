using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using MRS.Domain.Entities;

namespace MRS.Infrastructure.Data.EntityConfigurations;

/// <summary>
/// ユーザーエンティティの設定
/// </summary>
public class UserEntityConfiguration : IEntityTypeConfiguration<User>
{
    public void Configure(EntityTypeBuilder<User> builder)
    {
        // テーブル名
        builder.ToTable("Users");

        // 主キー
        builder.HasKey(x => x.UserId);

        // プロパティ設定
        builder.Property(x => x.UserId)
            .HasConversion(
                v => v.Value,
                v => new Domain.ValueObjects.UserId(v))
            .HasMaxLength(50)
            .IsRequired();

        builder.Property(x => x.Name)
            .HasConversion(
                v => v.Value,
                v => new Domain.ValueObjects.Name(v))
            .HasMaxLength(100)
            .IsRequired();

        // Passwordは単一プロパティのHashedValueのみ保存
        builder.Property(x => x.Password)
            .HasConversion(
                v => v.HashedValue,
                v => Domain.ValueObjects.Password.FromHash(v))
            .HasColumnName("HashedPassword")
            .HasMaxLength(255)
            .IsRequired();

        builder.Property(x => x.Role)
            .HasConversion<string>()
            .HasMaxLength(50)
            .IsRequired();

        builder.Property(x => x.IsActive)
            .IsRequired();

        builder.Property(x => x.CreatedAt)
            .IsRequired();

        builder.Property(x => x.UpdatedAt)
            .IsRequired();

        // インデックス
        builder.HasIndex(x => x.Name)
            .IsUnique();
    }
}
