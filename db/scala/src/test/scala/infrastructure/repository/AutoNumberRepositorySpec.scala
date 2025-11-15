package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.domain.AutoNumber
import scalikejdbc._

import java.time.LocalDateTime

class AutoNumberRepositorySpec extends DatabaseSpec {

  "AutoNumberRepository" should "自動採番マスタを登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val autoNumberRepo = AutoNumberRepository()

    DB localTx { implicit session =>
      val autoNumber = AutoNumber(
        slipType = "OR",
        yearMonth = LocalDateTime.of(2024, 1, 1, 0, 0),
        lastSlipNo = 0
      )

      val result = autoNumberRepo.create(autoNumber)
      result shouldBe 1

      val retrieved = autoNumberRepo.findById("OR", LocalDateTime.of(2024, 1, 1, 0, 0))
      retrieved.isDefined shouldBe true
      retrieved.get.lastSlipNo shouldBe 0
    }
  }

  it should "採番を進められる" in withContainers { container =>
    setupWithMigrations(container)

    val autoNumberRepo = AutoNumberRepository()

    DB localTx { implicit session =>
      val newNo1 = autoNumberRepo.getNextNumber("OR", LocalDateTime.of(2024, 1, 1, 0, 0))
      newNo1 shouldBe 1

      val newNo2 = autoNumberRepo.getNextNumber("OR", LocalDateTime.of(2024, 1, 1, 0, 0))
      newNo2 shouldBe 2

      val retrieved = autoNumberRepo.findById("OR", LocalDateTime.of(2024, 1, 1, 0, 0))
      retrieved.isDefined shouldBe true
      retrieved.get.lastSlipNo shouldBe 2
    }
  }

  it should "年月が変わると番号がリセットされる" in withContainers { container =>
    setupWithMigrations(container)

    val autoNumberRepo = AutoNumberRepository()

    DB localTx { implicit session =>
      // 1月に採番
      autoNumberRepo.getNextNumber("OR", LocalDateTime.of(2024, 1, 1, 0, 0))
      autoNumberRepo.getNextNumber("OR", LocalDateTime.of(2024, 1, 1, 0, 0))

      // 2月に採番（初回なので1から）
      val febNo = autoNumberRepo.getNextNumber("OR", LocalDateTime.of(2024, 2, 1, 0, 0))
      febNo shouldBe 1
    }
  }

  it should "伝票種別が異なれば独立して採番される" in withContainers { container =>
    setupWithMigrations(container)

    val autoNumberRepo = AutoNumberRepository()

    DB localTx { implicit session =>
      // 2024年4月で採番（他のテストと年月を分ける）
      // 受注伝票の採番
      val orNo1 = autoNumberRepo.getNextNumber("OR", LocalDateTime.of(2024, 4, 1, 0, 0))
      orNo1 shouldBe 1

      val orNo2 = autoNumberRepo.getNextNumber("OR", LocalDateTime.of(2024, 4, 1, 0, 0))
      orNo2 shouldBe 2

      // 売上伝票の採番（独立しているので1から）
      val slNo1 = autoNumberRepo.getNextNumber("SL", LocalDateTime.of(2024, 4, 1, 0, 0))
      slNo1 shouldBe 1

      val slNo2 = autoNumberRepo.getNextNumber("SL", LocalDateTime.of(2024, 4, 1, 0, 0))
      slNo2 shouldBe 2

      // 受注伝票は継続
      val orNo3 = autoNumberRepo.getNextNumber("OR", LocalDateTime.of(2024, 4, 1, 0, 0))
      orNo3 shouldBe 3
    }
  }

  it should "自動採番マスタを削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val autoNumberRepo = AutoNumberRepository()

    DB localTx { implicit session =>
      val autoNumber = AutoNumber(
        slipType = "IV",
        yearMonth = LocalDateTime.of(2024, 3, 1, 0, 0),
        lastSlipNo = 5
      )

      autoNumberRepo.create(autoNumber)

      val deleteResult = autoNumberRepo.delete("IV", LocalDateTime.of(2024, 3, 1, 0, 0))
      deleteResult shouldBe 1

      val retrieved = autoNumberRepo.findById("IV", LocalDateTime.of(2024, 3, 1, 0, 0))
      retrieved shouldBe None
    }
  }
}
