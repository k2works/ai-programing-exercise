package com.example.sales.domain.repository;

import com.example.sales.domain.model.Sales;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 売上データのMapperインターフェース
 */
@Mapper
public interface SalesMapper {
    /**
     * 売上を登録する
     * @param sales 売上
     * @return 登録件数
     */
    int insert(Sales sales);

    /**
     * 売上を更新する
     * @param sales 売上
     * @return 更新件数
     */
    int update(Sales sales);

    /**
     * 売上を削除する
     * @param salesNo 売上番号
     * @return 削除件数
     */
    int delete(@Param("salesNo") String salesNo);

    /**
     * 売上番号で売上を取得する
     * @param salesNo 売上番号
     * @return 売上
     */
    Optional<Sales> findById(@Param("salesNo") String salesNo);

    /**
     * 受注番号で売上を検索
     * @param orderNo 受注番号
     * @return 売上のリスト
     */
    List<Sales> findByOrderNo(@Param("orderNo") String orderNo);

    /**
     * すべての売上を取得する
     * @return 売上のリスト
     */
    List<Sales> findAll();
}
