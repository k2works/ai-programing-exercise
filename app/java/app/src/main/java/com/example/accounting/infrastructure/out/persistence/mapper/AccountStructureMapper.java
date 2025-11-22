package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.infrastructure.out.persistence.entity.AccountStructure;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface AccountStructureMapper {
    void insert(AccountStructure accountStructure);
    AccountStructure findByCode(@Param("accountCode") String accountCode);
    List<AccountStructure> findAll();
    List<AccountStructure> findChildren(@Param("accountCode") String accountCode);
    List<AccountStructure> findByLevel(@Param("hierarchyLevel") Integer hierarchyLevel);
    void update(AccountStructure accountStructure);
    void delete(@Param("accountCode") String accountCode);
}
