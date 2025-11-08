package com.example.sales.domain.repository;

import com.example.sales.domain.model.Customer;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 顧客マスタのMapperインターフェース
 */
@Mapper
public interface CustomerMapper {
    /**
     * 顧客を登録する
     * @param customer 顧客
     * @return 登録件数
     */
    int insert(Customer customer);

    /**
     * 顧客を更新する
     * @param customer 顧客
     * @return 更新件数
     */
    int update(Customer customer);

    /**
     * 顧客を削除する
     * @param customerCode 顧客コード
     * @param customerBranch 顧客枝番
     * @return 削除件数
     */
    int delete(@Param("customerCode") String customerCode,
               @Param("customerBranch") Integer customerBranch);

    /**
     * 顧客コードと枝番で顧客を取得する
     * @param customerCode 顧客コード
     * @param customerBranch 顧客枝番
     * @return 顧客
     */
    Optional<Customer> findById(@Param("customerCode") String customerCode,
                                 @Param("customerBranch") Integer customerBranch);

    /**
     * 顧客コードで顧客を取得する
     * @param customerCode 顧客コード
     * @return 顧客のリスト
     */
    List<Customer> findByCustomerCode(@Param("customerCode") String customerCode);

    /**
     * 取引先コードに紐づく全顧客を取得（Companyとの結合）
     * @param companyCode 取引先コード
     * @return 顧客のリスト
     */
    List<Customer> findByCompanyCode(@Param("companyCode") String companyCode);

    /**
     * すべての顧客を取得する
     * @return 顧客のリスト
     */
    List<Customer> findAll();
}
