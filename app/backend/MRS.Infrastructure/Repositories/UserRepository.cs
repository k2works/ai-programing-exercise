using Microsoft.EntityFrameworkCore;
using MRS.Application.Ports;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;
using MRS.Infrastructure.Data;

namespace MRS.Infrastructure.Repositories;

/// <summary>
/// ユーザーリポジトリの実装
/// </summary>
public class UserRepository : IUserRepository
{
    private readonly ApplicationDbContext _context;

    public UserRepository(ApplicationDbContext context)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <summary>
    /// ユーザーIDでユーザーを取得
    /// </summary>
    public async Task<User?> GetByIdAsync(UserId userId, CancellationToken cancellationToken = default)
    {
        return await _context.Users
            .FirstOrDefaultAsync(u => u.UserId == userId, cancellationToken);
    }

    /// <summary>
    /// ユーザー名でユーザーを取得
    /// </summary>
    public async Task<User?> GetByNameAsync(Name userName, CancellationToken cancellationToken = default)
    {
        return await _context.Users
            .FirstOrDefaultAsync(u => u.Name == userName, cancellationToken);
    }

    /// <summary>
    /// すべてのユーザーを取得
    /// </summary>
    public async Task<IEnumerable<User>> GetAllAsync(CancellationToken cancellationToken = default)
    {
        return await _context.Users
            .ToListAsync(cancellationToken);
    }

    /// <summary>
    /// ユーザーを追加
    /// </summary>
    public async Task AddAsync(User user, CancellationToken cancellationToken = default)
    {
        await _context.Users.AddAsync(user, cancellationToken);
    }

    /// <summary>
    /// ユーザーを更新
    /// </summary>
    public Task UpdateAsync(User user, CancellationToken cancellationToken = default)
    {
        _context.Users.Update(user);
        return Task.CompletedTask;
    }

    /// <summary>
    /// ユーザーを削除
    /// </summary>
    public async Task DeleteAsync(UserId userId, CancellationToken cancellationToken = default)
    {
        var user = await GetByIdAsync(userId, cancellationToken);
        if (user != null)
        {
            _context.Users.Remove(user);
        }
    }
}
