package com.example.sales.domain.repository;

import com.example.sales.domain.model.AutoNumber;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Mapper
public interface AutoNumberMapper {
    void insert(AutoNumber autoNumber);
    void update(AutoNumber autoNumber);
    void delete(@Param("slipType") String slipType,
                @Param("yearMonth") LocalDateTime yearMonth);
    Optional<AutoNumber> findById(@Param("slipType") String slipType,
                                   @Param("yearMonth") LocalDateTime yearMonth);
    List<AutoNumber> findAll();

    /**
     * 次の伝票番号を取得（FOR UPDATEでロック）
     */
    Optional<AutoNumber> findByIdForUpdate(@Param("slipType") String slipType,
                                           @Param("yearMonth") LocalDateTime yearMonth);

    /**
     * 最終伝票番号をインクリメント
     */
    void incrementLastSlipNo(@Param("slipType") String slipType,
                            @Param("yearMonth") LocalDateTime yearMonth);

    /**
     * 新しい年月の採番レコードを初期化
     */
    void initializeForNewMonth(@Param("slipType") String slipType,
                              @Param("yearMonth") LocalDateTime yearMonth);
}
