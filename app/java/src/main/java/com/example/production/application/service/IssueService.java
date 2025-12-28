package com.example.production.application.service;

import com.example.production.application.port.in.command.*;
import com.example.production.application.port.out.*;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.inventory.*;
import com.example.production.domain.model.plan.Order;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.Year;
import java.util.List;
import java.util.stream.IntStream;

/**
 * 払出サービス
 */
@Service
@RequiredArgsConstructor
public class IssueService {

    private final IssueInstructionRepository issueInstructionRepository;
    private final IssueRepository issueRepository;
    private final OrderRepository orderRepository;
    private final BomRepository bomRepository;
    private final InventoryService inventoryService;

    /**
     * 払出指示を作成する
     */
    @Transactional
    public IssueInstruction createIssueInstruction(IssueInstructionCreateCommand command) {
        String instructionNumber = generateInstructionNumber();

        IssueInstruction instruction = IssueInstruction.builder()
                .instructionNumber(instructionNumber)
                .orderNumber(command.getOrderNumber())
                .instructionDate(command.getInstructionDate())
                .locationCode(command.getLocationCode())
                .remarks(command.getRemarks())
                .build();

        issueInstructionRepository.save(instruction);

        List<IssueInstructionDetail> details = IntStream.range(0, command.getDetails().size())
                .mapToObj(i -> {
                    IssueInstructionDetailCommand detailCmd = command.getDetails().get(i);
                    return IssueInstructionDetail.builder()
                            .instructionNumber(instructionNumber)
                            .lineNumber(i + 1)
                            .itemCode(detailCmd.getItemCode())
                            .routingSequence(detailCmd.getRoutingSequence())
                            .issueQuantity(detailCmd.getIssueQuantity())
                            .build();
                })
                .toList();

        details.forEach(issueInstructionRepository::saveDetail);

        instruction.setDetails(details);
        return instruction;
    }

    /**
     * BOM から自動で払出指示を生成する
     */
    @Transactional
    public IssueInstruction autoGenerateIssueInstruction(AutoIssueInstructionCommand command) {
        Order order = orderRepository.findByOrderNumber(command.getOrderNumber())
                .orElseThrow(() -> new IllegalArgumentException("オーダ情報が見つかりません: " + command.getOrderNumber()));

        List<Bom> bomList = bomRepository.findByParentItemCodeAndDate(order.getItemCode(), LocalDate.now());
        BigDecimal planQuantity = order.getPlanQuantity();

        List<IssueInstructionDetailCommand> details = bomList.stream()
                .map(bom -> IssueInstructionDetailCommand.builder()
                        .itemCode(bom.getChildItemCode())
                        .routingSequence(bom.getSequence() != null ? bom.getSequence() : 1)
                        .issueQuantity(planQuantity.multiply(bom.getRequiredQuantity())
                                .divide(bom.getBaseQuantity(), 0, RoundingMode.CEILING))
                        .build())
                .toList();

        return createIssueInstruction(IssueInstructionCreateCommand.builder()
                .orderNumber(command.getOrderNumber())
                .instructionDate(command.getInstructionDate())
                .locationCode(command.getLocationCode())
                .details(details)
                .build());
    }

    /**
     * 払出を実行する
     */
    @Transactional
    public Issue executeIssue(IssueExecuteCommand command) {
        // 在庫チェック
        for (IssueDetailCommand detail : command.getDetails()) {
            Stock stock = inventoryService.getStock(command.getLocationCode(), detail.getItemCode());
            if (stock.getPassedQuantity().compareTo(detail.getIssueQuantity()) < 0) {
                throw new InsufficientStockException(
                        String.format("在庫が不足しています。品目: %s, 要求数: %s, 合格数: %s",
                                detail.getItemCode(), detail.getIssueQuantity(), stock.getPassedQuantity()));
            }
        }

        String issueNumber = generateIssueNumber();

        Issue issue = Issue.builder()
                .issueNumber(issueNumber)
                .workOrderNumber(command.getWorkOrderNumber())
                .routingSequence(command.getRoutingSequence())
                .locationCode(command.getLocationCode())
                .issueDate(command.getIssueDate())
                .issuerCode(command.getIssuerCode())
                .build();

        issueRepository.save(issue);

        List<IssueDetail> details = IntStream.range(0, command.getDetails().size())
                .mapToObj(i -> {
                    IssueDetailCommand detailCmd = command.getDetails().get(i);
                    return IssueDetail.builder()
                            .issueNumber(issueNumber)
                            .lineNumber(i + 1)
                            .itemCode(detailCmd.getItemCode())
                            .issueQuantity(detailCmd.getIssueQuantity())
                            .build();
                })
                .toList();

        details.forEach(issueRepository::saveDetail);

        // 在庫を減少
        for (IssueDetailCommand detail : command.getDetails()) {
            inventoryService.decreaseStock(StockChangeCommand.builder()
                    .locationCode(command.getLocationCode())
                    .itemCode(detail.getItemCode())
                    .quantity(detail.getIssueQuantity())
                    .stockStatus(StockStatus.PASSED)
                    .build());
        }

        issue.setDetails(details);
        return issue;
    }

    /**
     * 払出指示を取得する
     */
    public IssueInstruction findByInstructionNumber(String instructionNumber) {
        IssueInstruction instruction = issueInstructionRepository.findByInstructionNumber(instructionNumber)
                .orElse(null);
        if (instruction != null) {
            instruction.setDetails(issueInstructionRepository.findDetailsByInstructionNumber(instructionNumber));
        }
        return instruction;
    }

    /**
     * 払出を取得する
     */
    public Issue findByIssueNumber(String issueNumber) {
        Issue issue = issueRepository.findByIssueNumber(issueNumber)
                .orElse(null);
        if (issue != null) {
            issue.setDetails(issueRepository.findDetailsByIssueNumber(issueNumber));
        }
        return issue;
    }

    /**
     * 作業指示番号で払出を検索する
     */
    public List<Issue> findByWorkOrderNumber(String workOrderNumber) {
        return issueRepository.findByWorkOrderNumber(workOrderNumber);
    }

    /**
     * 全ての払出を取得する
     */
    public List<Issue> findAll() {
        return issueRepository.findAll();
    }

    private String generateInstructionNumber() {
        int year = Year.now().getValue();
        String prefix = "IS-" + year + "-";
        long count = issueInstructionRepository.countByPrefix(prefix);
        return String.format("IS-%d-%04d", year, count + 1);
    }

    private String generateIssueNumber() {
        int year = Year.now().getValue();
        String prefix = "PO-" + year + "-";
        long count = issueRepository.countByPrefix(prefix);
        return String.format("PO-%d-%04d", year, count + 1);
    }
}
