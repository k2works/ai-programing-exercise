package com.example.sales.domain.repository;

import com.example.sales.domain.model.Company;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 取引先マスタのMapperインターフェース
 */
@Mapper
public interface CompanyMapper {
    /**
     * 取引先を登録する
     * @param company 取引先
     * @return 登録件数
     */
    int insert(Company company);

    /**
     * 取引先を更新する
     * @param company 取引先
     * @return 更新件数
     */
    int update(Company company);

    /**
     * 取引先を削除する
     * @param companyCode 取引先コード
     * @return 削除件数
     */
    int delete(@Param("companyCode") String companyCode);

    /**
     * 取引先コードで取引先を取得する
     * @param companyCode 取引先コード
     * @return 取引先
     */
    Optional<Company> findById(@Param("companyCode") String companyCode);

    /**
     * すべての取引先を取得する
     * @return 取引先のリスト
     */
    List<Company> findAll();

    /**
     * 取引先グループコードで取引先を取得する
     * @param companyGroupCode 取引先グループコード
     * @return 取引先のリスト
     */
    List<Company> findByGroupCode(@Param("companyGroupCode") String companyGroupCode);
}
