package com.example.sales.domain.repository;

import com.example.sales.domain.model.SalesDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 売上データ明細のMapperインターフェース
 */
@Mapper
public interface SalesDetailMapper {
    /**
     * 売上明細を登録する
     * @param salesDetail 売上明細
     * @return 登録件数
     */
    int insert(SalesDetail salesDetail);

    /**
     * 売上明細を更新する
     * @param salesDetail 売上明細
     * @return 更新件数
     */
    int update(SalesDetail salesDetail);

    /**
     * 売上明細を削除する
     * @param salesNo 売上番号
     * @param salesRowNo 売上行番号
     * @return 削除件数
     */
    int delete(@Param("salesNo") String salesNo,
               @Param("salesRowNo") Integer salesRowNo);

    /**
     * 売上番号と行番号で売上明細を取得する
     * @param salesNo 売上番号
     * @param salesRowNo 売上行番号
     * @return 売上明細
     */
    Optional<SalesDetail> findById(@Param("salesNo") String salesNo,
                                     @Param("salesRowNo") Integer salesRowNo);

    /**
     * 売上番号で明細を全件取得
     * @param salesNo 売上番号
     * @return 売上明細のリスト
     */
    List<SalesDetail> findBySalesNo(@Param("salesNo") String salesNo);

    /**
     * 商品コードで明細を検索
     * @param productCode 商品コード
     * @return 売上明細のリスト
     */
    List<SalesDetail> findByProductCode(@Param("productCode") String productCode);

    /**
     * すべての売上明細を取得する
     * @return 売上明細のリスト
     */
    List<SalesDetail> findAll();
}
