package com.example.sales.domain.repository;

import com.example.sales.domain.model.Payment;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 支払データのMapperインターフェース
 */
@Mapper
public interface PaymentMapper {
    /**
     * 支払を登録する
     * @param payment 支払
     * @return 登録件数
     */
    int insert(Payment payment);

    /**
     * 支払を更新する
     * @param payment 支払
     * @return 更新件数
     */
    int update(Payment payment);

    /**
     * 支払を削除する
     * @param paymentNo 支払番号
     * @return 削除件数
     */
    int delete(@Param("paymentNo") String paymentNo);

    /**
     * 支払番号で支払を取得する
     * @param paymentNo 支払番号
     * @return 支払
     */
    Optional<Payment> findById(@Param("paymentNo") String paymentNo);

    /**
     * すべての支払を取得する
     * @return 支払のリスト
     */
    List<Payment> findAll();

    /**
     * 仕入先コードで支払を検索
     * @param supplierCode 仕入先コード
     * @param supplierBranch 仕入先枝番
     * @return 支払のリスト
     */
    List<Payment> findBySupplierCode(@Param("supplierCode") String supplierCode,
                                       @Param("supplierBranch") Integer supplierBranch);

    /**
     * 支払日で検索
     * @param paymentDate 支払日（YYYYMMDD形式）
     * @return 支払のリスト
     */
    List<Payment> findByPaymentDate(@Param("paymentDate") Integer paymentDate);
}
