package com.example.sales.domain.repository;

import com.example.sales.domain.model.OrderDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 受注データ明細のMapperインターフェース
 */
@Mapper
public interface OrderDetailMapper {
    /**
     * 受注明細を登録する
     * @param orderDetail 受注明細
     * @return 登録件数
     */
    int insert(OrderDetail orderDetail);

    /**
     * 受注明細を更新する
     * @param orderDetail 受注明細
     * @return 更新件数
     */
    int update(OrderDetail orderDetail);

    /**
     * 受注明細を削除する
     * @param orderNo 受注番号
     * @param orderRowNo 受注行番号
     * @return 削除件数
     */
    int delete(@Param("orderNo") String orderNo,
               @Param("orderRowNo") Integer orderRowNo);

    /**
     * 受注番号と行番号で受注明細を取得する
     * @param orderNo 受注番号
     * @param orderRowNo 受注行番号
     * @return 受注明細
     */
    Optional<OrderDetail> findById(@Param("orderNo") String orderNo,
                                     @Param("orderRowNo") Integer orderRowNo);

    /**
     * 受注番号で明細を全件取得
     * @param orderNo 受注番号
     * @return 受注明細のリスト
     */
    List<OrderDetail> findByOrderNo(@Param("orderNo") String orderNo);

    /**
     * 商品コードで明細を検索（どの受注に含まれているか）
     * @param productCode 商品コード
     * @return 受注明細のリスト
     */
    List<OrderDetail> findByProductCode(@Param("productCode") String productCode);

    /**
     * すべての受注明細を取得する
     * @return 受注明細のリスト
     */
    List<OrderDetail> findAll();
}
