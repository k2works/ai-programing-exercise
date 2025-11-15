package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.entity.Department
import scalikejdbc._

import java.time.LocalDateTime

class DepartmentRepositorySpec extends DatabaseSpec {

  "DepartmentRepository" should "部門を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = DepartmentRepository()

    val testDept = Department(
      deptCode = "11101",
      startDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      endDate = LocalDateTime.of(2100, 12, 31, 0, 0, 0),
      name = "新規部署",
      layer = 1,
      path = "D0001",
      lowestType = 1,
      slitYn = 0,
      createDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      updater = "admin",
    )

    // データを登録
    val insertResult = DB localTx { implicit session => repo.create(testDept) }

    insertResult shouldBe 1

    // 取得して検証
    val result = DB readOnly { implicit session => repo.findById(testDept.deptCode) }

    result shouldBe defined
    result.get.deptCode shouldBe testDept.deptCode
    result.get.name shouldBe testDept.name
  }

  it should "すべての部門を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = DepartmentRepository()

    val testDept = Department(
      deptCode = "11102",
      startDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      endDate = LocalDateTime.of(2100, 12, 31, 0, 0, 0),
      name = "新規部署",
      layer = 1,
      path = "D0002",
      lowestType = 1,
      slitYn = 0,
      createDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      updater = "admin",
    )

    DB localTx { implicit session => repo.create(testDept) }

    val results = DB readOnly { implicit session => repo.findAll() }

    results should not be empty
  }

  it should "部門を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = DepartmentRepository()

    val testDept = Department(
      deptCode = "11103",
      startDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      endDate = LocalDateTime.of(2100, 12, 31, 0, 0, 0),
      name = "新規部署",
      layer = 1,
      path = "D0003",
      lowestType = 1,
      slitYn = 0,
      createDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      updater = "admin",
    )

    DB localTx { implicit session => repo.create(testDept) }

    // 名前を更新
    val updatedDept = testDept.copy(
      name = "更新部署",
      updateDate = LocalDateTime.now(),
    )

    val updateResult = DB localTx { implicit session => repo.update(updatedDept) }

    updateResult shouldBe 1

    // 更新されたことを確認
    val result = DB readOnly { implicit session => repo.findById(testDept.deptCode) }

    result shouldBe defined
    result.get.name shouldBe "更新部署"
  }

  it should "部門を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = DepartmentRepository()

    val testDept = Department(
      deptCode = "11104",
      startDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      endDate = LocalDateTime.of(2100, 12, 31, 0, 0, 0),
      name = "新規部署",
      layer = 1,
      path = "D0004",
      lowestType = 1,
      slitYn = 0,
      createDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2021, 1, 1, 0, 0, 0),
      updater = "admin",
    )

    DB localTx { implicit session => repo.create(testDept) }

    // 削除
    val deleteResult = DB localTx { implicit session => repo.delete(testDept.deptCode) }

    deleteResult shouldBe 1

    // 削除されたことを確認
    val result = DB readOnly { implicit session => repo.findById(testDept.deptCode) }

    result shouldBe None
  }
}
