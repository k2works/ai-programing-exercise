package com.example.sales.domain.repository;

import com.example.sales.domain.model.PurchaseDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 仕入データ明細のMapperインターフェース
 */
@Mapper
public interface PurchaseDetailMapper {
    /**
     * 仕入明細を登録する
     * @param purchaseDetail 仕入明細
     * @return 登録件数
     */
    int insert(PurchaseDetail purchaseDetail);

    /**
     * 仕入明細を更新する
     * @param purchaseDetail 仕入明細
     * @return 更新件数
     */
    int update(PurchaseDetail purchaseDetail);

    /**
     * 仕入明細を削除する
     * @param purchaseNo 仕入番号
     * @param purchaseLineNo 仕入行番号
     * @return 削除件数
     */
    int delete(@Param("purchaseNo") String purchaseNo,
               @Param("purchaseLineNo") Integer purchaseLineNo);

    /**
     * 仕入番号と行番号で仕入明細を取得する
     * @param purchaseNo 仕入番号
     * @param purchaseLineNo 仕入行番号
     * @return 仕入明細
     */
    Optional<PurchaseDetail> findById(@Param("purchaseNo") String purchaseNo,
                                       @Param("purchaseLineNo") Integer purchaseLineNo);

    /**
     * 仕入番号で仕入明細を検索
     * @param purchaseNo 仕入番号
     * @return 仕入明細のリスト
     */
    List<PurchaseDetail> findByPurchaseNo(@Param("purchaseNo") String purchaseNo);
}
