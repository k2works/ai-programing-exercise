package com.example.sales.domain.repository;

import com.example.sales.domain.model.BankAccount;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 入金口座マスタのMapperインターフェース
 */
@Mapper
public interface BankAccountMapper {
    /**
     * 入金口座を登録する
     * @param bankAccount 入金口座
     * @return 登録件数
     */
    int insert(BankAccount bankAccount);

    /**
     * 入金口座を更新する
     * @param bankAccount 入金口座
     * @return 更新件数
     */
    int update(BankAccount bankAccount);

    /**
     * 入金口座を削除する
     * @param bankAccountCode 入金口座コード
     * @return 削除件数
     */
    int delete(@Param("bankAccountCode") String bankAccountCode);

    /**
     * 入金口座コードで入金口座を取得する
     * @param bankAccountCode 入金口座コード
     * @return 入金口座
     */
    Optional<BankAccount> findById(@Param("bankAccountCode") String bankAccountCode);

    /**
     * すべての入金口座を取得する
     * @return 入金口座のリスト
     */
    List<BankAccount> findAll();
}
