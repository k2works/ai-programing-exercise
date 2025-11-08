package com.example.sales.domain.repository;

import com.example.sales.domain.model.CompanyGroup;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 取引先グループマスタのMapperインターフェース
 */
@Mapper
public interface CompanyGroupMapper {
    /**
     * 取引先グループを登録する
     * @param companyGroup 取引先グループ
     * @return 登録件数
     */
    int insert(CompanyGroup companyGroup);

    /**
     * 取引先グループを更新する
     * @param companyGroup 取引先グループ
     * @return 更新件数
     */
    int update(CompanyGroup companyGroup);

    /**
     * 取引先グループを削除する
     * @param companyGroupCode 取引先グループコード
     * @return 削除件数
     */
    int delete(@Param("companyGroupCode") String companyGroupCode);

    /**
     * 取引先グループコードで取引先グループを取得する
     * @param companyGroupCode 取引先グループコード
     * @return 取引先グループ
     */
    Optional<CompanyGroup> findById(@Param("companyGroupCode") String companyGroupCode);

    /**
     * すべての取引先グループを取得する
     * @return 取引先グループのリスト
     */
    List<CompanyGroup> findAll();
}
