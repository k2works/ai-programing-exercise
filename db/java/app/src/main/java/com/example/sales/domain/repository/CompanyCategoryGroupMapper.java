package com.example.sales.domain.repository;

import com.example.sales.domain.model.CompanyCategoryGroup;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 取引先分類所属マスタのMapperインターフェース
 */
@Mapper
public interface CompanyCategoryGroupMapper {
    /**
     * 取引先分類所属を登録する
     * @param companyCategoryGroup 取引先分類所属
     * @return 登録件数
     */
    int insert(CompanyCategoryGroup companyCategoryGroup);

    /**
     * 取引先分類所属を更新する
     * @param companyCategoryGroup 取引先分類所属
     * @return 更新件数
     */
    int update(CompanyCategoryGroup companyCategoryGroup);

    /**
     * 取引先分類所属を削除する
     * @param categoryTypeCode 取引先分類種別コード
     * @param companyCategoryCode 取引先分類コード
     * @param companyCode 取引先コード
     * @return 削除件数
     */
    int delete(@Param("categoryTypeCode") String categoryTypeCode,
               @Param("companyCategoryCode") String companyCategoryCode,
               @Param("companyCode") String companyCode);

    /**
     * 複合主キーで取引先分類所属を取得する
     * @param categoryTypeCode 取引先分類種別コード
     * @param companyCategoryCode 取引先分類コード
     * @param companyCode 取引先コード
     * @return 取引先分類所属
     */
    Optional<CompanyCategoryGroup> findById(@Param("categoryTypeCode") String categoryTypeCode,
                                            @Param("companyCategoryCode") String companyCategoryCode,
                                            @Param("companyCode") String companyCode);

    /**
     * 取引先コードで取引先分類所属を取得する
     * @param companyCode 取引先コード
     * @return 取引先分類所属のリスト
     */
    List<CompanyCategoryGroup> findByCompanyCode(@Param("companyCode") String companyCode);

    /**
     * 取引先分類で取引先分類所属を取得する
     * @param categoryTypeCode 取引先分類種別コード
     * @param companyCategoryCode 取引先分類コード
     * @return 取引先分類所属のリスト
     */
    List<CompanyCategoryGroup> findByCategoryCode(@Param("categoryTypeCode") String categoryTypeCode,
                                                   @Param("companyCategoryCode") String companyCategoryCode);

    /**
     * すべての取引先分類所属を取得する
     * @return 取引先分類所属のリスト
     */
    List<CompanyCategoryGroup> findAll();
}
