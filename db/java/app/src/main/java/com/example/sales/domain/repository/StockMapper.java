package com.example.sales.domain.repository;

import com.example.sales.domain.model.Stock;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 在庫データのMapperインターフェース
 */
@Mapper
public interface StockMapper {
    /**
     * 在庫を登録する
     * @param stock 在庫
     * @return 登録件数
     */
    int insert(Stock stock);

    /**
     * 在庫を更新する
     * @param stock 在庫
     * @return 更新件数
     */
    int update(Stock stock);

    /**
     * 在庫を削除する（5つのフィールドの複合主キー）
     * @param warehouseCode 倉庫コード
     * @param productCode 商品コード
     * @param lotNo ロット番号
     * @param stockType 在庫区分
     * @param qualityType 良品区分
     * @return 削除件数
     */
    int delete(@Param("warehouseCode") String warehouseCode,
               @Param("productCode") String productCode,
               @Param("lotNo") String lotNo,
               @Param("stockType") String stockType,
               @Param("qualityType") String qualityType);

    /**
     * 複合主キーで在庫を取得する
     * @param warehouseCode 倉庫コード
     * @param productCode 商品コード
     * @param lotNo ロット番号
     * @param stockType 在庫区分
     * @param qualityType 良品区分
     * @return 在庫
     */
    Optional<Stock> findById(@Param("warehouseCode") String warehouseCode,
                              @Param("productCode") String productCode,
                              @Param("lotNo") String lotNo,
                              @Param("stockType") String stockType,
                              @Param("qualityType") String qualityType);

    /**
     * 倉庫コードで在庫を検索
     * @param warehouseCode 倉庫コード
     * @return 在庫のリスト
     */
    List<Stock> findByWarehouseCode(@Param("warehouseCode") String warehouseCode);

    /**
     * 商品コードで全倉庫の在庫を検索
     * @param productCode 商品コード
     * @return 在庫のリスト
     */
    List<Stock> findByProductCode(@Param("productCode") String productCode);

    /**
     * 有効在庫数を更新
     * @param warehouseCode 倉庫コード
     * @param productCode 商品コード
     * @param lotNo ロット番号
     * @param stockType 在庫区分
     * @param qualityType 良品区分
     * @param validQuantity 有効在庫数
     * @return 更新件数
     */
    int updateValidQuantity(@Param("warehouseCode") String warehouseCode,
                            @Param("productCode") String productCode,
                            @Param("lotNo") String lotNo,
                            @Param("stockType") String stockType,
                            @Param("qualityType") String qualityType,
                            @Param("validQuantity") Integer validQuantity);
}
