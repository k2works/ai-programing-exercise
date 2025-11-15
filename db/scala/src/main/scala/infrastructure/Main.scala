package com.example

import scalikejdbc._
import infrastructure.seed.SeedData
import infrastructure.db.{DatabaseConfig, FlywayMigration}

object Main extends App {

  // データベース接続設定
  DatabaseConfig.setupCustom(
    url = "jdbc:postgresql://localhost:5432/sales_management",
    user = "postgres",
    password = "postgres",
  )

  // マイグレーションを実行
  FlywayMigration.migrateCustom(
    url = "jdbc:postgresql://localhost:5432/sales_management",
    user = "postgres",
    password = "postgres",
  )

  try
    DB localTx { implicit session => SeedData.seedAll() }
  catch {
    case e: Exception =>
      println(s"❌ エラーが発生しました: ${e.getMessage}")
      e.printStackTrace()
  } finally
    ConnectionPool.closeAll()
}
