package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.IssueInstructionRepository;
import com.example.production.domain.model.inventory.IssueInstruction;
import com.example.production.domain.model.inventory.IssueInstructionDetail;
import com.example.production.infrastructure.persistence.mapper.IssueInstructionMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 払出指示リポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class IssueInstructionRepositoryImpl implements IssueInstructionRepository {

    private final IssueInstructionMapper issueInstructionMapper;

    @Override
    public void save(IssueInstruction instruction) {
        issueInstructionMapper.insert(instruction);
    }

    @Override
    public void saveDetail(IssueInstructionDetail detail) {
        issueInstructionMapper.insertDetail(detail);
    }

    @Override
    public Optional<IssueInstruction> findByInstructionNumber(String instructionNumber) {
        return issueInstructionMapper.findByInstructionNumber(instructionNumber);
    }

    @Override
    public List<IssueInstructionDetail> findDetailsByInstructionNumber(String instructionNumber) {
        return issueInstructionMapper.findDetailsByInstructionNumber(instructionNumber);
    }

    @Override
    public long countByPrefix(String prefix) {
        return issueInstructionMapper.countByPrefix(prefix);
    }

    @Override
    public void deleteAll() {
        issueInstructionMapper.deleteAllDetails();
        issueInstructionMapper.deleteAll();
    }
}
