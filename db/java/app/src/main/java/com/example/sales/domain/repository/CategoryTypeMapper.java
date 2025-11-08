package com.example.sales.domain.repository;

import com.example.sales.domain.model.CategoryType;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 取引先分類種別マスタのMapperインターフェース
 */
@Mapper
public interface CategoryTypeMapper {
    /**
     * 取引先分類種別を登録する
     * @param categoryType 取引先分類種別
     * @return 登録件数
     */
    int insert(CategoryType categoryType);

    /**
     * 取引先分類種別を更新する
     * @param categoryType 取引先分類種別
     * @return 更新件数
     */
    int update(CategoryType categoryType);

    /**
     * 取引先分類種別を削除する
     * @param categoryTypeCode 取引先分類種別コード
     * @return 削除件数
     */
    int delete(@Param("categoryTypeCode") String categoryTypeCode);

    /**
     * 取引先分類種別コードで取引先分類種別を取得する
     * @param categoryTypeCode 取引先分類種別コード
     * @return 取引先分類種別
     */
    Optional<CategoryType> findById(@Param("categoryTypeCode") String categoryTypeCode);

    /**
     * すべての取引先分類種別を取得する
     * @return 取引先分類種別のリスト
     */
    List<CategoryType> findAll();
}
