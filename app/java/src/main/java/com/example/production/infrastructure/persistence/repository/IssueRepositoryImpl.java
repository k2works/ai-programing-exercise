package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.IssueRepository;
import com.example.production.domain.model.inventory.Issue;
import com.example.production.domain.model.inventory.IssueDetail;
import com.example.production.infrastructure.persistence.mapper.IssueMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 払出リポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class IssueRepositoryImpl implements IssueRepository {

    private final IssueMapper issueMapper;

    @Override
    public void save(Issue issue) {
        issueMapper.insert(issue);
    }

    @Override
    public void saveDetail(IssueDetail detail) {
        issueMapper.insertDetail(detail);
    }

    @Override
    public Optional<Issue> findByIssueNumber(String issueNumber) {
        return issueMapper.findByIssueNumber(issueNumber);
    }

    @Override
    public List<IssueDetail> findDetailsByIssueNumber(String issueNumber) {
        return issueMapper.findDetailsByIssueNumber(issueNumber);
    }

    @Override
    public List<Issue> findByWorkOrderNumber(String workOrderNumber) {
        return issueMapper.findByWorkOrderNumber(workOrderNumber);
    }

    @Override
    public long countByPrefix(String prefix) {
        return issueMapper.countByPrefix(prefix);
    }

    @Override
    public void deleteAll() {
        issueMapper.deleteAllDetails();
        issueMapper.deleteAll();
    }
}
