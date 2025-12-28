package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.inventory.IssueInstruction;
import com.example.production.domain.model.inventory.IssueInstructionDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 払出指示マッパー
 */
@Mapper
public interface IssueInstructionMapper {

    void insert(IssueInstruction instruction);

    void insertDetail(IssueInstructionDetail detail);

    Optional<IssueInstruction> findByInstructionNumber(@Param("instructionNumber") String instructionNumber);

    List<IssueInstructionDetail> findDetailsByInstructionNumber(@Param("instructionNumber") String instructionNumber);

    long countByPrefix(@Param("prefix") String prefix);

    void deleteAllDetails();

    void deleteAll();
}
