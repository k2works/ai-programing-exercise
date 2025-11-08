package com.example.sales.domain.repository;

import com.example.sales.domain.model.CompanyCategory;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 取引先分類マスタのMapperインターフェース
 */
@Mapper
public interface CompanyCategoryMapper {
    /**
     * 取引先分類を登録する
     * @param companyCategory 取引先分類
     * @return 登録件数
     */
    int insert(CompanyCategory companyCategory);

    /**
     * 取引先分類を更新する
     * @param companyCategory 取引先分類
     * @return 更新件数
     */
    int update(CompanyCategory companyCategory);

    /**
     * 取引先分類を削除する
     * @param categoryTypeCode 取引先分類種別コード
     * @param companyCategoryCode 取引先分類コード
     * @return 削除件数
     */
    int delete(@Param("categoryTypeCode") String categoryTypeCode,
               @Param("companyCategoryCode") String companyCategoryCode);

    /**
     * 取引先分類種別コードと取引先分類コードで取引先分類を取得する
     * @param categoryTypeCode 取引先分類種別コード
     * @param companyCategoryCode 取引先分類コード
     * @return 取引先分類
     */
    Optional<CompanyCategory> findById(@Param("categoryTypeCode") String categoryTypeCode,
                                       @Param("companyCategoryCode") String companyCategoryCode);

    /**
     * 取引先分類種別コードで取引先分類を取得する
     * @param categoryTypeCode 取引先分類種別コード
     * @return 取引先分類のリスト
     */
    List<CompanyCategory> findByCategoryTypeCode(@Param("categoryTypeCode") String categoryTypeCode);

    /**
     * すべての取引先分類を取得する
     * @return 取引先分類のリスト
     */
    List<CompanyCategory> findAll();
}
