package com.example.sales.domain.repository;

import com.example.sales.domain.model.PurchaseOrderDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 発注データ明細のMapperインターフェース
 */
@Mapper
public interface PurchaseOrderDetailMapper {
    /**
     * 発注明細を登録する
     * @param purchaseOrderDetail 発注明細
     * @return 登録件数
     */
    int insert(PurchaseOrderDetail purchaseOrderDetail);

    /**
     * 発注明細を更新する
     * @param purchaseOrderDetail 発注明細
     * @return 更新件数
     */
    int update(PurchaseOrderDetail purchaseOrderDetail);

    /**
     * 発注明細を削除する
     * @param poNo 発注番号
     * @param poLineNo 発注行番号
     * @return 削除件数
     */
    int delete(@Param("poNo") String poNo, @Param("poLineNo") Integer poLineNo);

    /**
     * 発注番号と行番号で発注明細を取得する
     * @param poNo 発注番号
     * @param poLineNo 発注行番号
     * @return 発注明細
     */
    Optional<PurchaseOrderDetail> findById(@Param("poNo") String poNo,
                                            @Param("poLineNo") Integer poLineNo);

    /**
     * 発注番号で発注明細を検索
     * @param poNo 発注番号
     * @return 発注明細のリスト
     */
    List<PurchaseOrderDetail> findByPoNo(@Param("poNo") String poNo);
}
