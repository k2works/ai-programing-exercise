package com.example.sales.domain.repository;

import com.example.sales.domain.model.InvoiceDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 請求データ明細のMapperインターフェース
 */
@Mapper
public interface InvoiceDetailMapper {
    /**
     * 請求明細を登録する
     * @param invoiceDetail 請求明細
     * @return 登録件数
     */
    int insert(InvoiceDetail invoiceDetail);

    /**
     * 請求明細を更新する
     * @param invoiceDetail 請求明細
     * @return 更新件数
     */
    int update(InvoiceDetail invoiceDetail);

    /**
     * 請求明細を削除する
     * @param invoiceNo 請求番号
     * @param invoiceLineNo 請求行番号
     * @return 削除件数
     */
    int delete(@Param("invoiceNo") String invoiceNo,
               @Param("invoiceLineNo") Integer invoiceLineNo);

    /**
     * 請求番号と請求行番号で請求明細を取得する
     * @param invoiceNo 請求番号
     * @param invoiceLineNo 請求行番号
     * @return 請求明細
     */
    Optional<InvoiceDetail> findById(@Param("invoiceNo") String invoiceNo,
                                      @Param("invoiceLineNo") Integer invoiceLineNo);

    /**
     * 請求番号で請求明細を検索
     * @param invoiceNo 請求番号
     * @return 請求明細のリスト
     */
    List<InvoiceDetail> findByInvoiceNo(@Param("invoiceNo") String invoiceNo);

    /**
     * 売上番号と売上行番号で請求明細を検索
     * @param salesNo 売上番号
     * @param salesLineNo 売上行番号
     * @return 請求明細のリスト
     */
    List<InvoiceDetail> findBySalesDetail(@Param("salesNo") String salesNo,
                                           @Param("salesLineNo") Integer salesLineNo);
}
