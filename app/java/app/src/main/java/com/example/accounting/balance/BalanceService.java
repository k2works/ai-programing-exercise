package com.example.accounting.balance;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.time.LocalDate;

/**
 * 残高管理サービス
 */
public class BalanceService {

    private final String jdbcUrl;
    private final String username;
    private final String password;

    public BalanceService(String jdbcUrl, String username, String password) {
        this.jdbcUrl = jdbcUrl;
        this.username = username;
        this.password = password;
    }

    /**
     * 日次残高を更新（UPSERT）
     *
     * @param entryDate 起票日
     * @param accountCode 勘定科目コード
     * @param subAccountCode 補助科目コード
     * @param departmentCode 部門コード
     * @param projectCode プロジェクトコード
     * @param settlementFlag 決算仕訳フラグ
     * @param debitAmount 借方金額
     * @param creditAmount 貸方金額
     */
    @SuppressWarnings("checkstyle:ParameterNumber")
    public void updateDailyBalance(
            LocalDate entryDate,
            String accountCode,
            String subAccountCode,
            String departmentCode,
            String projectCode,
            Integer settlementFlag,
            BigDecimal debitAmount,
            BigDecimal creditAmount) {

        // PostgreSQL の ON CONFLICT ... DO UPDATE を使用
        String sql = """
            INSERT INTO "日次勘定科目残高" (
                "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
            DO UPDATE SET
                "借方金額" = "日次勘定科目残高"."借方金額" + EXCLUDED."借方金額",
                "貸方金額" = "日次勘定科目残高"."貸方金額" + EXCLUDED."貸方金額",
                "更新日時" = CURRENT_TIMESTAMP
            """;

        try (Connection conn = DriverManager.getConnection(jdbcUrl, username, password);
             PreparedStatement pstmt = conn.prepareStatement(sql)) {

            pstmt.setObject(1, entryDate);
            pstmt.setString(2, accountCode);
            pstmt.setString(3, subAccountCode != null ? subAccountCode : "");
            pstmt.setString(4, departmentCode != null ? departmentCode : "");
            pstmt.setString(5, projectCode != null ? projectCode : "");
            pstmt.setInt(6, settlementFlag != null ? settlementFlag : 0);
            pstmt.setBigDecimal(7, debitAmount);
            pstmt.setBigDecimal(8, creditAmount);

            pstmt.executeUpdate();

        } catch (SQLException e) {
            throw new RuntimeException("Failed to update daily balance", e);
        }
    }
}
