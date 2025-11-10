package com.example.sales.domain.repository;

import com.example.sales.domain.model.CreditBalance;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface CreditBalanceMapper {
    void insert(CreditBalance creditBalance);
    void update(CreditBalance creditBalance);
    void delete(@Param("companyCode") String companyCode);
    Optional<CreditBalance> findById(@Param("companyCode") String companyCode);
    List<CreditBalance> findAll();

    /**
     * 受注残高を更新
     */
    void updateOrderBalance(@Param("companyCode") String companyCode,
                           @Param("amount") Integer amount);

    /**
     * 債権残高を更新
     */
    void updateReceivableBalance(@Param("companyCode") String companyCode,
                                 @Param("amount") Integer amount);

    /**
     * 債務残高を更新
     */
    void updatePayableBalance(@Param("companyCode") String companyCode,
                             @Param("amount") Integer amount);

    /**
     * 与信限度額超過の取引先を検索
     */
    List<CreditBalance> findExceedingCreditLimit(@Param("creditLimit") Integer creditLimit);
}
