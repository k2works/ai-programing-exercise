using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 払出サービス
/// </summary>
public class IssueService
{
    private readonly IIssueInstructionRepository _instructionRepository;
    private readonly IIssueRepository _issueRepository;
    private readonly IStockRepository _stockRepository;
    private readonly InventoryService _inventoryService;

    public IssueService(
        IIssueInstructionRepository instructionRepository,
        IIssueRepository issueRepository,
        IStockRepository stockRepository,
        InventoryService inventoryService)
    {
        _instructionRepository = instructionRepository;
        _issueRepository = issueRepository;
        _stockRepository = stockRepository;
        _inventoryService = inventoryService;
    }

    /// <summary>
    /// 払出指示番号を生成する
    /// </summary>
    private async Task<string> GenerateInstructionNumberAsync(DateOnly instructionDate)
    {
        var prefix = $"IS-{instructionDate:yyyyMM}-";
        var latestNumber = await _instructionRepository.FindLatestInstructionNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 払出番号を生成する
    /// </summary>
    private async Task<string> GenerateIssueNumberAsync(DateOnly issueDate)
    {
        var prefix = $"PO-{issueDate:yyyyMM}-";
        var latestNumber = await _issueRepository.FindLatestIssueNumberAsync($"{prefix}%");

        var sequence = 1;
        if (latestNumber != null)
        {
            var currentSequence = int.Parse(latestNumber[^4..]);
            sequence = currentSequence + 1;
        }

        return $"{prefix}{sequence:D4}";
    }

    /// <summary>
    /// 払出指示を作成する
    /// </summary>
    public async Task<IssueInstruction> CreateIssueInstructionAsync(IssueInstructionCommand command)
    {
        var instructionNumber = await GenerateInstructionNumberAsync(command.InstructionDate);

        var instruction = new IssueInstruction
        {
            InstructionNumber = instructionNumber,
            OrderNumber = command.OrderNumber,
            InstructionDate = command.InstructionDate,
            LocationCode = command.LocationCode,
            Remarks = command.Remarks
        };

        await _instructionRepository.SaveAsync(instruction);

        var details = new List<IssueInstructionDetail>();
        for (var i = 0; i < command.Details.Count; i++)
        {
            var detailCommand = command.Details[i];
            var detail = new IssueInstructionDetail
            {
                InstructionNumber = instructionNumber,
                LineNumber = i + 1,
                ItemCode = detailCommand.ItemCode,
                RoutingSequence = detailCommand.RoutingSequence,
                IssueQuantity = detailCommand.IssueQuantity
            };
            await _instructionRepository.SaveDetailAsync(detail);
            details.Add(detail);
        }

        instruction.Details = details;
        return instruction;
    }

    /// <summary>
    /// 払出指示を検索する
    /// </summary>
    public async Task<IssueInstruction?> FindIssueInstructionAsync(string instructionNumber)
    {
        var instruction = await _instructionRepository.FindByInstructionNumberAsync(instructionNumber);
        if (instruction == null)
        {
            return null;
        }

        var details = await _instructionRepository.FindDetailsByInstructionNumberAsync(instructionNumber);
        instruction.Details = details.ToList();
        return instruction;
    }

    /// <summary>
    /// 払出を実行する
    /// </summary>
    public async Task<Issue> ExecuteIssueAsync(IssueExecuteCommand command)
    {
        var locationCode = command.LocationCode ?? "WH001";

        // 在庫チェック
        foreach (var detail in command.Details)
        {
            var stock = await _inventoryService.GetStockAsync(locationCode, detail.ItemCode);
            if (stock.PassedQuantity < detail.IssueQuantity)
            {
                throw new InsufficientStockException($"在庫が不足しています: {detail.ItemCode}");
            }
        }

        var issueNumber = await GenerateIssueNumberAsync(command.IssueDate);

        var issue = new Issue
        {
            IssueNumber = issueNumber,
            WorkOrderNumber = command.WorkOrderNumber,
            RoutingSequence = command.RoutingSequence,
            LocationCode = locationCode,
            IssueDate = command.IssueDate,
            IssuerCode = command.IssuerCode
        };

        await _issueRepository.SaveAsync(issue);

        var details = new List<IssueDetail>();
        for (var i = 0; i < command.Details.Count; i++)
        {
            var detailCommand = command.Details[i];
            var detail = new IssueDetail
            {
                IssueNumber = issueNumber,
                LineNumber = i + 1,
                ItemCode = detailCommand.ItemCode,
                IssueQuantity = detailCommand.IssueQuantity
            };
            await _issueRepository.SaveDetailAsync(detail);
            details.Add(detail);
        }

        // 在庫を減少
        foreach (var detail in command.Details)
        {
            await _inventoryService.DecreaseStockAsync(new StockChangeCommand
            {
                LocationCode = locationCode,
                ItemCode = detail.ItemCode,
                Quantity = detail.IssueQuantity,
                StockStatus = StockStatus.Passed
            });
        }

        issue.Details = details;
        return issue;
    }

    /// <summary>
    /// 払出を検索する
    /// </summary>
    public async Task<Issue?> FindIssueAsync(string issueNumber)
    {
        var issue = await _issueRepository.FindByIssueNumberAsync(issueNumber);
        if (issue == null)
        {
            return null;
        }

        var details = await _issueRepository.FindDetailsByIssueNumberAsync(issueNumber);
        issue.Details = details.ToList();
        return issue;
    }

    /// <summary>
    /// 作業指示に紐づく払出を検索する
    /// </summary>
    public async Task<IReadOnlyList<Issue>> FindIssuesByWorkOrderAsync(string workOrderNumber)
    {
        return await _issueRepository.FindByWorkOrderNumberAsync(workOrderNumber);
    }
}
