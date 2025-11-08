package com.example.sales.domain.repository;

import com.example.sales.domain.model.PurchaseOrder;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 発注データのMapperインターフェース
 */
@Mapper
public interface PurchaseOrderMapper {
    /**
     * 発注を登録する
     * @param purchaseOrder 発注
     * @return 登録件数
     */
    int insert(PurchaseOrder purchaseOrder);

    /**
     * 発注を更新する
     * @param purchaseOrder 発注
     * @return 更新件数
     */
    int update(PurchaseOrder purchaseOrder);

    /**
     * 発注を削除する
     * @param poNo 発注番号
     * @return 削除件数
     */
    int delete(@Param("poNo") String poNo);

    /**
     * 発注番号で発注を取得する
     * @param poNo 発注番号
     * @return 発注
     */
    Optional<PurchaseOrder> findById(@Param("poNo") String poNo);

    /**
     * すべての発注を取得する
     * @return 発注のリスト
     */
    List<PurchaseOrder> findAll();

    /**
     * 仕入先コードで発注を検索
     * @param supplierCode 仕入先コード
     * @return 発注のリスト
     */
    List<PurchaseOrder> findBySupplierCode(@Param("supplierCode") String supplierCode);

    /**
     * 受注番号から発注を検索
     * @param orderNo 受注番号
     * @return 発注のリスト
     */
    List<PurchaseOrder> findByOrderNo(@Param("orderNo") String orderNo);
}
