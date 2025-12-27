using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.In.Dto;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 工数実績サービス
/// </summary>
public class LaborHoursService
{
    private readonly ILaborHoursRepository _laborHoursRepository;
    private readonly IWorkOrderDetailRepository _workOrderDetailRepository;
    private readonly IWorkOrderRepository _workOrderRepository;
    private readonly IProcessRepository _processRepository;

    public LaborHoursService(
        ILaborHoursRepository laborHoursRepository,
        IWorkOrderDetailRepository workOrderDetailRepository,
        IWorkOrderRepository workOrderRepository,
        IProcessRepository processRepository)
    {
        _laborHoursRepository = laborHoursRepository;
        _workOrderDetailRepository = workOrderDetailRepository;
        _workOrderRepository = workOrderRepository;
        _processRepository = processRepository;
    }

    /// <summary>
    /// 工数実績番号を生成する
    /// </summary>
    private async Task<string> GenerateLaborHoursNumberAsync(DateOnly workDate)
    {
        var prefix = $"LH-{workDate:yyyyMM}-";
        var latestNumber = await _laborHoursRepository.FindLatestLaborHoursNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 工数実績を報告する
    /// </summary>
    public async Task<LaborHours> ReportLaborHoursAsync(LaborHoursCommand command)
    {
        // 作業指示明細を取得
        var detail = await _workOrderDetailRepository.FindByWorkOrderAndSequenceAsync(
            command.WorkOrderNumber, command.Sequence)
            ?? throw new ArgumentException($"Work order detail not found: {command.WorkOrderNumber}, Sequence: {command.Sequence}");

        // 作業指示を取得
        var workOrder = await _workOrderRepository.FindByWorkOrderNumberAsync(command.WorkOrderNumber)
            ?? throw new ArgumentException($"Work order not found: {command.WorkOrderNumber}");

        if (workOrder.Status != WorkOrderStatus.InProgress)
        {
            throw new InvalidOperationException("Only IN_PROGRESS work orders can report labor hours");
        }

        var laborHoursNumber = await GenerateLaborHoursNumberAsync(command.WorkDate);

        // 工数実績を作成
        var laborHours = new LaborHours
        {
            LaborHoursNumber = laborHoursNumber,
            WorkOrderNumber = command.WorkOrderNumber,
            ItemCode = workOrder.ItemCode,
            Sequence = command.Sequence,
            ProcessCode = detail.ProcessCode,
            DepartmentCode = command.DepartmentCode,
            EmployeeCode = command.EmployeeCode,
            WorkDate = command.WorkDate,
            Hours = command.Hours,
            Remarks = command.Remarks
        };
        await _laborHoursRepository.SaveAsync(laborHours);

        return laborHours;
    }

    /// <summary>
    /// 工数実績を検索する
    /// </summary>
    public async Task<LaborHours?> FindByLaborHoursNumberAsync(string laborHoursNumber)
    {
        return await _laborHoursRepository.FindByLaborHoursNumberAsync(laborHoursNumber);
    }

    /// <summary>
    /// 作業指示に紐づく工数実績を検索する
    /// </summary>
    public async Task<IReadOnlyList<LaborHours>> FindByWorkOrderNumberAsync(string workOrderNumber)
    {
        return await _laborHoursRepository.FindByWorkOrderNumberAsync(workOrderNumber);
    }

    /// <summary>
    /// 工順別の工数合計を取得する
    /// </summary>
    public async Task<decimal> GetTotalHoursBySequenceAsync(string workOrderNumber, int sequence)
    {
        return await _laborHoursRepository.SumByWorkOrderAndSequenceAsync(workOrderNumber, sequence);
    }

    /// <summary>
    /// 作業指示の工数サマリを取得する
    /// </summary>
    public async Task<LaborHoursSummary> GetSummaryAsync(string workOrderNumber)
    {
        var workOrder = await _workOrderRepository.FindByWorkOrderNumberAsync(workOrderNumber)
            ?? throw new ArgumentException($"Work order not found: {workOrderNumber}");

        var details = await _workOrderDetailRepository.FindByWorkOrderNumberAsync(workOrderNumber);
        var processHours = new List<ProcessLaborHours>();
        var totalHours = 0m;

        foreach (var detail in details)
        {
            var hours = await _laborHoursRepository.SumByWorkOrderAndSequenceAsync(workOrderNumber, detail.Sequence);
            var process = await _processRepository.FindByProcessCodeAsync(detail.ProcessCode);

            processHours.Add(new ProcessLaborHours
            {
                ProcessCode = detail.ProcessCode,
                ProcessName = process?.ProcessName ?? "",
                Hours = hours
            });

            totalHours += hours;
        }

        return new LaborHoursSummary
        {
            TotalHours = totalHours,
            ProcessHours = processHours
        };
    }

    /// <summary>
    /// 担当者別の工数を取得する
    /// </summary>
    public async Task<decimal> GetTotalHoursByEmployeeAsync(string employeeCode, DateOnly startDate, DateOnly endDate)
    {
        return await _laborHoursRepository.SumByEmployeeAsync(employeeCode, startDate, endDate);
    }
}
