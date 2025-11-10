package com.example.sales.domain.repository;

import com.example.sales.domain.model.Invoice;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 請求データのMapperインターフェース
 */
@Mapper
public interface InvoiceMapper {
    /**
     * 請求を登録する
     * @param invoice 請求
     * @return 登録件数
     */
    int insert(Invoice invoice);

    /**
     * 請求を更新する
     * @param invoice 請求
     * @return 更新件数
     */
    int update(Invoice invoice);

    /**
     * 請求を削除する
     * @param invoiceNo 請求番号
     * @return 削除件数
     */
    int delete(@Param("invoiceNo") String invoiceNo);

    /**
     * 請求番号で請求を取得する
     * @param invoiceNo 請求番号
     * @return 請求
     */
    Optional<Invoice> findById(@Param("invoiceNo") String invoiceNo);

    /**
     * すべての請求を取得する
     * @return 請求のリスト
     */
    List<Invoice> findAll();

    /**
     * 取引先コードで請求を検索
     * @param companyCode 取引先コード
     * @return 請求のリスト
     */
    List<Invoice> findByCompanyCode(@Param("companyCode") String companyCode);
}
