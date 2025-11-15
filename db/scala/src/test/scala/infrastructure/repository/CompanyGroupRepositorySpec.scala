package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.entity.CompanyGroup
import scalikejdbc._

import java.time.LocalDateTime

class CompanyGroupRepositorySpec extends DatabaseSpec {

  "CompanyGroupRepository" should "取引先グループを登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CompanyGroupRepository()

    val group = CompanyGroup(
      compGroupCode = "G001",
      groupName = "重要顧客グループ",
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )

    val result = DB localTx { implicit session => repo.create(group) }

    result shouldBe 1

    val retrieved = DB readOnly { implicit session => repo.findById("G001") }

    retrieved.isDefined shouldBe true
    retrieved.get.groupName shouldBe "重要顧客グループ"
  }

  it should "全取引先グループを取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CompanyGroupRepository()

    val group = CompanyGroup(
      compGroupCode = "G002",
      groupName = "一般顧客グループ",
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )

    DB localTx { implicit session => repo.create(group) }

    val groups = DB readOnly { implicit session => repo.findAll() }

    groups should not be empty
    groups.map(_.compGroupCode) should contain("G002")
  }

  it should "取引先グループを更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CompanyGroupRepository()

    val group = CompanyGroup(
      compGroupCode = "G003",
      groupName = "新規グループ",
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )

    DB localTx { implicit session => repo.create(group) }

    val updated = group.copy(
      groupName = "更新後グループ",
      updateDate = LocalDateTime.now(),
      updater = "admin2",
    )

    val updateResult = DB localTx { implicit session => repo.update(updated) }

    updateResult shouldBe 1

    val retrieved = DB readOnly { implicit session => repo.findById("G003") }

    retrieved.isDefined shouldBe true
    retrieved.get.groupName shouldBe "更新後グループ"
    retrieved.get.updater shouldBe "admin2"
  }

  it should "取引先グループを削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CompanyGroupRepository()

    val group = CompanyGroup(
      compGroupCode = "G004",
      groupName = "削除対象グループ",
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )

    DB localTx { implicit session => repo.create(group) }

    val deleteResult = DB localTx { implicit session => repo.delete("G004") }

    deleteResult shouldBe 1

    val retrieved = DB readOnly { implicit session => repo.findById("G004") }

    retrieved shouldBe None
  }
}
