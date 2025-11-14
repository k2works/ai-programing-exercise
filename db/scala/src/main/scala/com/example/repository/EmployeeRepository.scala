package com.example.repository

import com.example.domain.Employee
import scalikejdbc._

/**
 * 社員マスタのリポジトリ
 */
trait EmployeeRepository {
  def create(emp: Employee)(implicit session: DBSession): Int
  def findById(empCode: String)(implicit session: DBSession): Option[Employee]
  def findAll()(implicit session: DBSession): List[Employee]
  def findByDepartment(deptCode: String)(implicit session: DBSession): List[Employee]
  def update(emp: Employee)(implicit session: DBSession): Int
  def delete(empCode: String)(implicit session: DBSession): Int
}

/**
 * EmployeeRepository の実装
 */
class EmployeeRepositoryImpl extends EmployeeRepository {

  override def create(emp: Employee)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 社員マスタ (
        社員コード, 社員名, 社員名カナ, パスワード,
        電話番号, FAX番号, 部門コード, 開始日,
        職種コード, 承認権限コード,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${emp.empCode}, ${emp.name}, ${emp.kana}, ${emp.loginPassword},
        ${emp.tel}, ${emp.fax}, ${emp.deptCode}, ${emp.startDate},
        ${emp.occuCode}, ${emp.approvalCode},
        ${emp.createDate}, ${emp.creator},
        ${emp.updateDate}, ${emp.updater}
      )
    """.update.apply()
  }

  override def findById(empCode: String)(implicit session: DBSession): Option[Employee] = {
    sql"""
      SELECT 社員コード, 社員名, 社員名カナ, パスワード,
             電話番号, FAX番号, 部門コード, 開始日,
             職種コード, 承認権限コード,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 社員マスタ
      WHERE 社員コード = $empCode
    """.map(Employee.apply).single.apply()
  }

  override def findAll()(implicit session: DBSession): List[Employee] = {
    sql"""
      SELECT 社員コード, 社員名, 社員名カナ, パスワード,
             電話番号, FAX番号, 部門コード, 開始日,
             職種コード, 承認権限コード,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 社員マスタ
      ORDER BY 社員コード
    """.map(Employee.apply).list.apply()
  }

  override def findByDepartment(deptCode: String)(implicit session: DBSession): List[Employee] = {
    sql"""
      SELECT 社員コード, 社員名, 社員名カナ, パスワード,
             電話番号, FAX番号, 部門コード, 開始日,
             職種コード, 承認権限コード,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 社員マスタ
      WHERE 部門コード = $deptCode
      ORDER BY 社員コード
    """.map(Employee.apply).list.apply()
  }

  override def update(emp: Employee)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 社員マスタ
      SET 社員名 = ${emp.name},
          社員名カナ = ${emp.kana},
          パスワード = ${emp.loginPassword},
          電話番号 = ${emp.tel},
          FAX番号 = ${emp.fax},
          部門コード = ${emp.deptCode},
          開始日 = ${emp.startDate},
          職種コード = ${emp.occuCode},
          承認権限コード = ${emp.approvalCode},
          更新日時 = ${emp.updateDate},
          更新者名 = ${emp.updater}
      WHERE 社員コード = ${emp.empCode}
    """.update.apply()
  }

  override def delete(empCode: String)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 社員マスタ
      WHERE 社員コード = $empCode
    """.update.apply()
  }
}

object EmployeeRepository {
  def apply(): EmployeeRepository = new EmployeeRepositoryImpl()
}
