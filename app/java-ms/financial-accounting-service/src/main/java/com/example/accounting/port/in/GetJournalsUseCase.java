package com.example.accounting.port.in;

import com.example.accounting.domain.Journal;
import java.util.List;

/**
 * 仕訳取得ユースケース
 */
public interface GetJournalsUseCase {
    /**
     * 会計年度で仕訳を取得
     *
     * @param fiscalYear 会計年度
     * @return 仕訳リスト
     */
    List<Journal> getJournalsByFiscalYear(Integer fiscalYear);
}
