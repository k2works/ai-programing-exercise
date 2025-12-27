using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

namespace ProductionManagement.Infrastructure.Seed;

/// <summary>
/// Seedデータ投入Runner（コマンドライン実行用）
/// </summary>
public class SeedRunner : IHostedService
{
    private readonly ILogger<SeedRunner> _logger;
    private readonly SeedDataService _seedDataService;
    private readonly IHostApplicationLifetime _applicationLifetime;

    public SeedRunner(
        ILogger<SeedRunner> logger,
        SeedDataService seedDataService,
        IHostApplicationLifetime applicationLifetime)
    {
        _logger = logger;
        _seedDataService = seedDataService;
        _applicationLifetime = applicationLifetime;
    }

    public async Task StartAsync(CancellationToken cancellationToken)
    {
        try
        {
            await _seedDataService.SeedAllAsync();
            _logger.LogInformation("Seed データ投入が正常に完了しました");
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Seed データ投入中にエラーが発生しました");
            throw;
        }
        finally
        {
            _applicationLifetime.StopApplication();
        }
    }

    public Task StopAsync(CancellationToken cancellationToken) => Task.CompletedTask;
}
