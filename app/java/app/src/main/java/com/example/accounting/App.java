package com.example.accounting;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * 財務会計システム - メインアプリケーション
 *
 * <p>TDDでデータベース設計を進める財務会計システムのSpring Bootアプリケーション
 *
 * <p>このアプリケーションは以下の機能を提供します：
 * <ul>
 *   <li>勘定科目管理
 *   <li>仕訳入力
 *   <li>元帳管理
 *   <li>残高試算表
 *   <li>決算書作成
 * </ul>
 */
@SpringBootApplication
public class App {

    /**
     * アプリケーションのエントリーポイント
     *
     * @param args コマンドライン引数
     */
    public static void main(String[] args) {
        SpringApplication.run(App.class, args);
    }
}
