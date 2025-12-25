package com.example.production.application.port.out;

import com.example.production.domain.model.inventory.Issue;
import com.example.production.domain.model.inventory.IssueDetail;

import java.util.List;
import java.util.Optional;

/**
 * 払出リポジトリインターフェース
 */
public interface IssueRepository {

    void save(Issue issue);

    void saveDetail(IssueDetail detail);

    Optional<Issue> findByIssueNumber(String issueNumber);

    List<IssueDetail> findDetailsByIssueNumber(String issueNumber);

    List<Issue> findByWorkOrderNumber(String workOrderNumber);

    long countByPrefix(String prefix);

    void deleteAll();
}
