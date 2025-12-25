package com.example.production.application.port.out;

import com.example.production.domain.model.inventory.IssueInstruction;
import com.example.production.domain.model.inventory.IssueInstructionDetail;

import java.util.List;
import java.util.Optional;

/**
 * 払出指示リポジトリインターフェース
 */
public interface IssueInstructionRepository {

    void save(IssueInstruction instruction);

    void saveDetail(IssueInstructionDetail detail);

    Optional<IssueInstruction> findByInstructionNumber(String instructionNumber);

    List<IssueInstructionDetail> findDetailsByInstructionNumber(String instructionNumber);

    long countByPrefix(String prefix);

    void deleteAll();
}
