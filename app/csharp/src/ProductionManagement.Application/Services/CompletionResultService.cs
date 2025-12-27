using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 完成実績サービス
/// </summary>
public class CompletionResultService
{
    private readonly ICompletionResultRepository _completionResultRepository;
    private readonly ICompletionInspectionResultRepository _completionInspectionResultRepository;
    private readonly IWorkOrderRepository _workOrderRepository;

    public CompletionResultService(
        ICompletionResultRepository completionResultRepository,
        ICompletionInspectionResultRepository completionInspectionResultRepository,
        IWorkOrderRepository workOrderRepository)
    {
        _completionResultRepository = completionResultRepository;
        _completionInspectionResultRepository = completionInspectionResultRepository;
        _workOrderRepository = workOrderRepository;
    }

    /// <summary>
    /// 完成実績番号を生成する
    /// </summary>
    private async Task<string> GenerateCompletionResultNumberAsync(DateOnly completionDate)
    {
        var prefix = $"CR-{completionDate:yyyyMM}-";
        var latestNumber = await _completionResultRepository.FindLatestCompletionResultNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 完成実績を報告する
    /// </summary>
    public async Task<CompletionResult> ReportCompletionAsync(CompletionResultCommand command)
    {
        // 作業指示を取得
        var workOrder = await _workOrderRepository.FindByWorkOrderNumberAsync(command.WorkOrderNumber)
            ?? throw new ArgumentException($"Work order not found: {command.WorkOrderNumber}");

        if (workOrder.Status != WorkOrderStatus.InProgress)
        {
            throw new InvalidOperationException("Only IN_PROGRESS work orders can report completion");
        }

        var completionResultNumber = await GenerateCompletionResultNumberAsync(command.CompletionDate);

        // 完成実績を作成
        var completionResult = new CompletionResult
        {
            CompletionResultNumber = completionResultNumber,
            WorkOrderNumber = command.WorkOrderNumber,
            ItemCode = workOrder.ItemCode,
            CompletionDate = command.CompletionDate,
            CompletedQuantity = command.CompletedQuantity,
            GoodQuantity = command.GoodQuantity,
            DefectQuantity = command.DefectQuantity,
            Remarks = command.Remarks
        };
        await _completionResultRepository.SaveAsync(completionResult);

        // 検査結果を作成
        var inspectionResults = new List<CompletionInspectionResult>();
        if (command.InspectionResults != null)
        {
            foreach (var irCommand in command.InspectionResults)
            {
                var ir = new CompletionInspectionResult
                {
                    CompletionResultNumber = completionResultNumber,
                    DefectCode = irCommand.DefectCode,
                    Quantity = irCommand.Quantity
                };
                await _completionInspectionResultRepository.SaveAsync(ir);
                inspectionResults.Add(ir);
            }
        }

        // 作業指示の累計を更新
        await _workOrderRepository.UpdateCompletionQuantitiesAsync(
            command.WorkOrderNumber,
            command.CompletedQuantity,
            command.GoodQuantity,
            command.DefectQuantity
        );

        completionResult.InspectionResults = inspectionResults;
        return completionResult;
    }

    /// <summary>
    /// 完成実績を検索する
    /// </summary>
    public async Task<CompletionResult?> FindByCompletionResultNumberAsync(string completionResultNumber)
    {
        var completionResult = await _completionResultRepository.FindByCompletionResultNumberAsync(completionResultNumber);
        if (completionResult != null)
        {
            var inspectionResults = await _completionInspectionResultRepository.FindByCompletionResultNumberAsync(completionResultNumber);
            completionResult.InspectionResults = inspectionResults;
        }
        return completionResult;
    }

    /// <summary>
    /// 作業指示に紐づく完成実績を検索する
    /// </summary>
    public async Task<IReadOnlyList<CompletionResult>> FindByWorkOrderNumberAsync(string workOrderNumber)
    {
        return await _completionResultRepository.FindByWorkOrderNumberAsync(workOrderNumber);
    }
}
