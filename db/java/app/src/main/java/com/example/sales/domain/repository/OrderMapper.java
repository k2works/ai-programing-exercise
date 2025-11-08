package com.example.sales.domain.repository;

import com.example.sales.domain.model.Order;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 受注データのMapperインターフェース
 */
@Mapper
public interface OrderMapper {
    /**
     * 受注を登録する
     * @param order 受注
     * @return 登録件数
     */
    int insert(Order order);

    /**
     * 受注を更新する
     * @param order 受注
     * @return 更新件数
     */
    int update(Order order);

    /**
     * 受注を削除する
     * @param orderNo 受注番号
     * @return 削除件数
     */
    int delete(@Param("orderNo") String orderNo);

    /**
     * 受注番号で受注を取得する
     * @param orderNo 受注番号
     * @return 受注
     */
    Optional<Order> findById(@Param("orderNo") String orderNo);

    /**
     * 顧客コードで受注を検索
     * @param customerCode 顧客コード
     * @return 受注のリスト
     */
    List<Order> findByCustomerCode(@Param("customerCode") String customerCode);

    /**
     * すべての受注を取得する
     * @return 受注のリスト
     */
    List<Order> findAll();
}
