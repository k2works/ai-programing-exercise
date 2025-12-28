package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.inventory.Issue;
import com.example.production.domain.model.inventory.IssueDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 払出マッパー
 */
@Mapper
public interface IssueMapper {

    void insert(Issue issue);

    void insertDetail(IssueDetail detail);

    Optional<Issue> findByIssueNumber(@Param("issueNumber") String issueNumber);

    List<IssueDetail> findDetailsByIssueNumber(@Param("issueNumber") String issueNumber);

    List<Issue> findByWorkOrderNumber(@Param("workOrderNumber") String workOrderNumber);

    long countByPrefix(@Param("prefix") String prefix);

    List<Issue> findAll();

    void deleteAllDetails();

    void deleteAll();
}
