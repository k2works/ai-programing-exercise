package com.example.sales.domain.repository;

import com.example.sales.domain.model.Purchase;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 仕入データのMapperインターフェース
 */
@Mapper
public interface PurchaseMapper {
    /**
     * 仕入を登録する
     * @param purchase 仕入
     * @return 登録件数
     */
    int insert(Purchase purchase);

    /**
     * 仕入を更新する
     * @param purchase 仕入
     * @return 更新件数
     */
    int update(Purchase purchase);

    /**
     * 仕入を削除する
     * @param purchaseNo 仕入番号
     * @return 削除件数
     */
    int delete(@Param("purchaseNo") String purchaseNo);

    /**
     * 仕入番号で仕入を取得する
     * @param purchaseNo 仕入番号
     * @return 仕入
     */
    Optional<Purchase> findById(@Param("purchaseNo") String purchaseNo);

    /**
     * すべての仕入を取得する
     * @return 仕入のリスト
     */
    List<Purchase> findAll();

    /**
     * 仕入先コードで仕入を検索
     * @param supplierCode 仕入先コード
     * @return 仕入のリスト
     */
    List<Purchase> findBySupplierCode(@Param("supplierCode") String supplierCode);

    /**
     * 発注番号から仕入を検索
     * @param poNo 発注番号
     * @return 仕入のリスト
     */
    List<Purchase> findByPoNo(@Param("poNo") String poNo);
}
