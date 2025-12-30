using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using Grpc.Core;
using ProductionManagement.Grpc.Protos;
using ProductionManagement.WPF.Services;
using MrpGrpcServiceClient = ProductionManagement.Grpc.Protos.MrpGrpcService.MrpGrpcServiceClient;

namespace ProductionManagement.WPF.ViewModels.Grpc;

/// <summary>
/// gRPC MRP 実行 ViewModel
/// </summary>
public partial class GrpcMrpViewModel : ViewModelBase
{
    private readonly IGrpcChannelFactory _channelFactory;
    private readonly MrpGrpcServiceClient _client;

    public GrpcMrpViewModel(
        IGrpcChannelFactory channelFactory,
        IDialogService dialogService) : base(dialogService)
    {
        _channelFactory = channelFactory;
        _client = new MrpGrpcServiceClient(_channelFactory.Channel);
    }

    /// <summary>
    /// 進捗メッセージ一覧
    /// </summary>
    public ObservableCollection<MrpProgressItem> ProgressItems { get; } = [];

    /// <summary>
    /// 現在のフェーズ
    /// </summary>
    [ObservableProperty]
    private string _currentPhase = string.Empty;

    /// <summary>
    /// 進捗パーセント
    /// </summary>
    [ObservableProperty]
    private int _progressPercent;

    /// <summary>
    /// 進捗メッセージ
    /// </summary>
    [ObservableProperty]
    private string _progressMessage = string.Empty;

    /// <summary>
    /// 実行中フラグ
    /// </summary>
    [ObservableProperty]
    [NotifyCanExecuteChangedFor(nameof(ExecuteMrpCommand))]
    [NotifyCanExecuteChangedFor(nameof(ExecuteMrpSyncCommand))]
    private bool _isExecuting;

    /// <summary>
    /// 開始日
    /// </summary>
    [ObservableProperty]
    private DateTime _startDate = DateTime.Today;

    /// <summary>
    /// 終了日
    /// </summary>
    [ObservableProperty]
    private DateTime _endDate = DateTime.Today.AddDays(30);

    /// <summary>
    /// MRP 実行結果
    /// </summary>
    [ObservableProperty]
    private MrpResultMessage? _result;

    /// <summary>
    /// MRP 実行（Server Streaming - 進捗通知）
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanExecute))]
    private async Task ExecuteMrpAsync()
    {
        try
        {
            IsExecuting = true;
            ProgressItems.Clear();
            Result = null;

            var request = new ExecuteMrpRequest
            {
                StartDate = new Date { Year = StartDate.Year, Month = StartDate.Month, Day = StartDate.Day },
                EndDate = new Date { Year = EndDate.Year, Month = EndDate.Month, Day = EndDate.Day }
            };

            using var call = _client.ExecuteMrp(request);

            await foreach (var progress in call.ResponseStream.ReadAllAsync())
            {
                CurrentPhase = GetPhaseDisplayName(progress.Phase);
                ProgressPercent = progress.Current;
                ProgressMessage = progress.Message;

                ProgressItems.Add(new MrpProgressItem
                {
                    Timestamp = DateTime.Now,
                    Phase = CurrentPhase,
                    Progress = progress.Current,
                    Message = progress.Message
                });
            }

            await DialogService.ShowInfoAsync("完了", "MRP 処理が完了しました");
        }
        catch (RpcException ex)
        {
            await DialogService.ShowErrorDetailAsync("gRPC エラー", "MRP 実行中にエラーが発生しました", ex.Message);
        }
        finally
        {
            IsExecuting = false;
        }
    }

    /// <summary>
    /// MRP 実行（同期 - 結果のみ）
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanExecute))]
    private async Task ExecuteMrpSyncAsync()
    {
        try
        {
            IsExecuting = true;
            ProgressItems.Clear();
            ProgressMessage = "MRP 実行中（同期モード）...";

            var request = new ExecuteMrpRequest
            {
                StartDate = new Date { Year = StartDate.Year, Month = StartDate.Month, Day = StartDate.Day },
                EndDate = new Date { Year = EndDate.Year, Month = EndDate.Month, Day = EndDate.Day }
            };

            Result = await _client.ExecuteMrpSyncAsync(request);

            ProgressMessage = Result.Success ? "MRP 処理完了" : $"MRP 処理失敗: {Result.ErrorMessage}";

            if (Result.Success)
            {
                await DialogService.ShowInfoAsync(
                    "MRP 実行結果",
                    $"所要量生成: {Result.RequirementsCreated} 件\n" +
                    $"オーダ生成: {Result.OrdersCreated} 件\n" +
                    $"引当実行: {Result.AllocationsMade} 件\n" +
                    $"不足数量合計: {Result.TotalShortageQuantity?.Value ?? "0"}");
            }
        }
        catch (RpcException ex)
        {
            await DialogService.ShowErrorDetailAsync("gRPC エラー", "MRP 実行中にエラーが発生しました", ex.Message);
        }
        finally
        {
            IsExecuting = false;
        }
    }

    private bool CanExecute() => !IsExecuting;

    /// <summary>
    /// クリア
    /// </summary>
    [RelayCommand]
    private void Clear()
    {
        ProgressItems.Clear();
        CurrentPhase = string.Empty;
        ProgressPercent = 0;
        ProgressMessage = string.Empty;
        Result = null;
    }

    private static string GetPhaseDisplayName(MrpPhase phase)
    {
        return phase switch
        {
            MrpPhase.Initializing => "初期化中",
            MrpPhase.ExplodingBom => "BOM 展開中",
            MrpPhase.CalculatingRequirements => "所要量計算中",
            MrpPhase.AllocatingInventory => "在庫引当中",
            MrpPhase.CreatingOrders => "オーダ生成中",
            MrpPhase.MrpCompleted => "完了",
            _ => phase.ToString()
        };
    }
}

/// <summary>
/// MRP 進捗アイテム
/// </summary>
public class MrpProgressItem
{
    public DateTime Timestamp { get; init; }
    public string Phase { get; init; } = string.Empty;
    public int Progress { get; init; }
    public string Message { get; init; } = string.Empty;
}
