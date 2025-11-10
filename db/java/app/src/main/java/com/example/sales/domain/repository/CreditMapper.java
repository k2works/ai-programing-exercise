package com.example.sales.domain.repository;

import com.example.sales.domain.model.Credit;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 入金データのMapperインターフェース
 */
@Mapper
public interface CreditMapper {
    /**
     * 入金を登録する
     * @param credit 入金
     * @return 登録件数
     */
    int insert(Credit credit);

    /**
     * 入金を更新する
     * @param credit 入金
     * @return 更新件数
     */
    int update(Credit credit);

    /**
     * 入金を削除する
     * @param creditNo 入金番号
     * @return 削除件数
     */
    int delete(@Param("creditNo") String creditNo);

    /**
     * 入金番号で入金を取得する
     * @param creditNo 入金番号
     * @return 入金
     */
    Optional<Credit> findById(@Param("creditNo") String creditNo);

    /**
     * すべての入金を取得する
     * @return 入金のリスト
     */
    List<Credit> findAll();

    /**
     * 顧客コードで入金を検索
     * @param customerCode 顧客コード
     * @param customerBranch 顧客枝番
     * @return 入金のリスト
     */
    List<Credit> findByCustomerCode(@Param("customerCode") String customerCode,
                                      @Param("customerBranch") Integer customerBranch);
}
