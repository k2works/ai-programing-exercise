package com.example.sales.domain.repository;

import com.example.sales.domain.model.Supplier;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 仕入先マスタのMapperインターフェース
 */
@Mapper
public interface SupplierMapper {
    /**
     * 仕入先を登録する
     * @param supplier 仕入先
     * @return 登録件数
     */
    int insert(Supplier supplier);

    /**
     * 仕入先を更新する
     * @param supplier 仕入先
     * @return 更新件数
     */
    int update(Supplier supplier);

    /**
     * 仕入先を削除する
     * @param supplierCode 仕入先コード
     * @param supplierBranch 仕入先枝番
     * @return 削除件数
     */
    int delete(@Param("supplierCode") String supplierCode,
               @Param("supplierBranch") Integer supplierBranch);

    /**
     * 仕入先コードと枝番で仕入先を取得する
     * @param supplierCode 仕入先コード
     * @param supplierBranch 仕入先枝番
     * @return 仕入先
     */
    Optional<Supplier> findById(@Param("supplierCode") String supplierCode,
                                 @Param("supplierBranch") Integer supplierBranch);

    /**
     * 仕入先コードで仕入先を取得する
     * @param supplierCode 仕入先コード
     * @return 仕入先のリスト
     */
    List<Supplier> findBySupplierCode(@Param("supplierCode") String supplierCode);

    /**
     * 取引先コードに紐づく全仕入先を取得（Companyとの結合）
     * @param companyCode 取引先コード
     * @return 仕入先のリスト
     */
    List<Supplier> findByCompanyCode(@Param("companyCode") String companyCode);

    /**
     * すべての仕入先を取得する
     * @return 仕入先のリスト
     */
    List<Supplier> findAll();
}
